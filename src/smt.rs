use crate::z3::{self, *, ast::Ast};
use crate::symbolic::{Symbol, TemporalSymbol};
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::cell::{RefCell, RefMut};

pub enum Assertion<T> {
    Constrained(T),
    Unconstrained(T),
    Unsolveable,
}

pub struct Solver {
    config:         Config,
    ctx:            &'static Context,
    solver:         z3::Solver<'static>,
    syms:           HashMap<Symbol, (z3::FuncDecl<'static>, String)>,
    debug_line:     usize,
    pub debug:      RefCell<File>,
    pub infinite:   bool,
    debug_indent:   String,
}



#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Bool,
    Signed(u32),
    Unsigned(u32),
}

pub struct ModelRef(Model<'static>);

impl Solver {

    pub fn theory(&mut self, sym: Symbol, args: Vec<Type>, name: &str, t: Type) {
        let lname = format!("{}_{}", sym, name.replace(|c: char| !c.is_ascii_alphanumeric(), "_"));

        let mut capi_args = Vec::new();
        let mut debug_args = Vec::new();
        for t in args {
            match t {
                Type::Bool => {
                    capi_args.push(Sort::bool(&self.ctx));
                    debug_args.push("bool".to_string());
                }
                Type::Signed(size) | Type::Unsigned(size) =>  {
                    capi_args.push(Sort::bitvector(&self.ctx, size));
                    debug_args.push(format!("(_ BitVec {})", size));
                }
            }
        }
        let debug_args = debug_args.join(" ");
        let capi_args  = capi_args.iter().map(|s|s).collect::<Vec<_>>();

        let f = match t {
            Type::Signed(size) | Type::Unsigned(size) => {
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "(declare-fun S{} ({}) (_ BitVec 64)); theory {}\n", lname, debug_args, name).unwrap();
                FuncDecl::new(
                    &self.ctx,
                    lname.clone(),
                    &capi_args[..],
                    &Sort::bitvector(&self.ctx, size),
                )
            }
            Type::Bool => {
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "(declare-fun S{} ({}) bool); theory {}\n", lname, debug_args, name).unwrap();
                FuncDecl::new(
                    &self.ctx,
                    lname.clone(),
                    &capi_args[..],
                    &Sort::bool(&self.ctx),
                )
            }
        };
        self.syms.insert(sym, (f, lname));
        self.checkpoint();
    }

    pub fn invocation(&mut self, theory: Symbol, args: Vec<TemporalSymbol>, tmp: Symbol) {

        let mut capi_args = Vec::new();
        let mut debug_args = Vec::new();
        for arg in args {
            debug_args.push(format!("(S{} {})", self.syms[&arg.0].1, arg.1));
            capi_args.push(self.syms[&arg.0].0.apply(&[&ast::Dynamic::from_ast(&ast::Int::from_u64(&self.ctx, arg.1))]));
        }

        let debug_args = debug_args.join(" ");
        let capi_args  = capi_args.iter().map(|s|s).collect::<Vec<_>>();

        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(assert (= (S{} {} ) (S{} 0)))\n", self.syms[&theory].1, debug_args, self.syms[&tmp].1).unwrap();

        let call = self.syms[&theory].0.apply(&capi_args[..]);
        let tmp  = self.syms[&tmp].0.apply(&[&ast::Dynamic::from_ast(&ast::Int::from_u64(&self.ctx, 0))]);
        self.solver.assert(&call._eq(&tmp));
        self.checkpoint();
    }


    pub fn declare(&mut self, sym: Symbol, name: &str, typ: Type) {
        let lname = format!("{}_{}", sym, name.replace(|c: char| !c.is_ascii_alphanumeric(), "_"));

        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(declare-fun S{} (Int) {}); local \n",
        lname,
        match typ {
            Type::Bool => format!("bool"),
            Type::Signed(s) | Type::Unsigned(s) => format!("(_ BitVec {})", s),
        },
        ).unwrap();

        let f = FuncDecl::new(
            &self.ctx,
            lname.clone(),
            &[&Sort::int(&self.ctx)],
            &match typ {
                Type::Bool => Sort::bool(&self.ctx),
                Type::Signed(s) | Type::Unsigned(s) => Sort::bitvector(&self.ctx, s),
            }
        );

        self.syms.insert(sym, (f, lname));
        self.checkpoint();
    }


    pub fn assign(&mut self, lhs: TemporalSymbol, rhs: TemporalSymbol, t: Type) {
        if self.infinite {
            return;
        }


        if t == Type::Bool {


            let lhs_s = self.syms[&lhs.0].0.apply(&[&ast::Dynamic::from_ast(&ast::Int::from_u64(&self.ctx, lhs.1))]).as_bool().unwrap();
            let rhs_s = self.syms[&rhs.0].0.apply(&[&ast::Dynamic::from_ast(&ast::Int::from_u64(&self.ctx, rhs.1))]);



            let rhs_s = match rhs_s.as_bool() {
                Some(b) => b,
                None => {
                    let rhs_s = rhs_s.as_bv().unwrap();
                    let bone = ast::BV::from_u64(&self.ctx, 1, rhs_s.get_size());
                    rhs_s.bvuge(&bone)
                }
            };

            write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
            write!(self.debug.borrow_mut(), "(assert (= (S{} {})  (S{} {})))\n",
            self.syms[&lhs.0].1, lhs.1, self.syms[&rhs.0].1, rhs.1).unwrap();
            self.solver.assert(&lhs_s._eq(&rhs_s));

            return;
        }


        let lhs_s = self.syms[&lhs.0].0.apply(&[&ast::Dynamic::from_ast(&ast::Int::from_u64(&self.ctx, lhs.1))]).as_bv().unwrap();
        let rhs_s = self.syms[&rhs.0].0.apply(&[&ast::Dynamic::from_ast(&ast::Int::from_u64(&self.ctx, rhs.1))]).as_bv().unwrap();


        let lhs_size = lhs_s.get_size();
        let rhs_size = rhs_s.get_size();

        if lhs_size < rhs_size {
            write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
            write!(self.debug.borrow_mut(), "(assert (= (S{} {}) ((_ extract {} {}) (S{} {}))))\n",
            self.syms[&lhs.0].1, lhs.1, rhs_size - 1, rhs_size - lhs_size, self.syms[&rhs.0].1, rhs.1).unwrap();

            self.solver.assert(&lhs_s._eq(&rhs_s.extract(rhs_size -1, rhs_size - lhs_size)));
        } else if lhs_size > rhs_size {
            write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
            write!(self.debug.borrow_mut(), "(assert (= (S{} {}) ((_ zero_extend {}) (S{} {}))))\n",
            self.syms[&lhs.0].1, lhs.1, lhs_size - rhs_size , self.syms[&rhs.0].1, rhs.1).unwrap();

            self.solver.assert(&lhs_s._eq(&rhs_s.zero_ext(lhs_size - rhs_size)));
        } else {
            write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
            write!(self.debug.borrow_mut(), "(assert (= (S{} {})  (S{} {})))\n",
            self.syms[&lhs.0].1, lhs.1, self.syms[&rhs.0].1, rhs.1).unwrap();
            self.solver.assert(&lhs_s._eq(&rhs_s));
        }


        self.checkpoint();
    }

    pub fn literal(&mut self, tmp: Symbol, val: u64, typ: Type) {
        match typ {
            Type::Unsigned(size) | Type::Signed(size) => {
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "(assert (= (S{} 0) (_ bv{} {})))\n",
                self.syms[&tmp].1, val, size).unwrap();

                let tmp = self.syms[&tmp].0.apply(&[&ast::Dynamic::from_ast(&ast::Int::from_u64(&self.ctx, 0))]).as_bv().unwrap();
                let rhs = ast::BV::from_u64(&self.ctx, val, size);
                self.solver.assert(&tmp._eq(&rhs));
            }
            Type::Bool => {
                if val > 0 {
                    write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                    write!(self.debug.borrow_mut(), "(assert (= (S{} 0) true ))\n", self.syms[&tmp].1).unwrap();

                    let tmp = self.syms[&tmp].0.apply(&[&ast::Dynamic::from_ast(&ast::Int::from_u64(&self.ctx, 0))]).as_bool().unwrap();
                    let rhs = ast::Bool::from_bool(&self.ctx, true);
                    self.solver.assert(&tmp._eq(&rhs));
                } else {
                    write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                    write!(self.debug.borrow_mut(), "(assert (= (S{} 0) false ))\n", self.syms[&tmp].1).unwrap();

                    let tmp = self.syms[&tmp].0.apply(&[&ast::Dynamic::from_ast(&ast::Int::from_u64(&self.ctx, 0))]).as_bool().unwrap();
                    let rhs = ast::Bool::from_bool(&self.ctx, false);
                    self.solver.assert(&tmp._eq(&rhs));
                }

            }
        }
        self.checkpoint();
    }

    pub fn infix_op_will_wrap(
        &self,
        lhs:    TemporalSymbol,
        rhs:    TemporalSymbol,
        op:     crate::ast::InfixOperator,
        t:      Type,
    ) -> bool {
        if t == Type::Bool {
            return false;
        }
        let lhs_s = self.syms[&lhs.0].0.apply(&[&ast::Dynamic::from_ast(&ast::Int::from_u64(&self.ctx, lhs.1))]).as_bv().unwrap();
        let rhs_s = self.syms[&rhs.0].0.apply(&[&ast::Dynamic::from_ast(&ast::Int::from_u64(&self.ctx, rhs.1))]).as_bv().unwrap();
        match op {
            crate::ast::InfixOperator::Add       => {
                self.solver.push();
                self.solver.assert(&lhs_s.to_int(false).add(&[&lhs_s.to_int(false)])
                     ._eq(&lhs_s.bvadd(&rhs_s).to_int(false)));
                let r = self.solve();
                self.solver.pop(1);
                !r
            }
            crate::ast::InfixOperator::Subtract  => {
                self.solver.push();
                self.solver.assert(&lhs_s.to_int(false).sub(&[&lhs_s.to_int(false)])
                                   ._eq(&lhs_s.bvsub(&rhs_s).to_int(false)));
                let r = self.solve();
                self.solver.pop(1);
                !r
            }
            crate::ast::InfixOperator::Multiply  => {
                self.solver.push();
                self.solver.assert(&lhs_s.to_int(false).mul(&[&lhs_s.to_int(false)])
                                   ._eq(&lhs_s.bvmul(&rhs_s).to_int(false)));
                let r = self.solve();
                self.solver.pop(1);
                !r
            }
            _ => false,
        }
    }

    pub fn infix_op(
        &self,
        tmp:    Symbol,
        lhs:    TemporalSymbol,
        rhs:    TemporalSymbol,
        op:     crate::ast::InfixOperator,
        t:      Type,
        signed: bool,
        )
    {
        if self.infinite {
            return;
        }
        let d_lhs_n = &self.syms[&lhs.0].1.clone();
        let d_rhs_n = &self.syms[&rhs.0].1.clone();

        let lhs_s = self.syms[&lhs.0].0.apply(&[&ast::Dynamic::from_ast(&ast::Int::from_u64(&self.ctx, lhs.1))]);
        let rhs_s = self.syms[&rhs.0].0.apply(&[&ast::Dynamic::from_ast(&ast::Int::from_u64(&self.ctx, rhs.1))]);
        let tmp_s = self.syms[&tmp].0.apply(&[&ast::Dynamic::from_ast(&ast::Int::from_u64(&self.ctx, 0))]);

        match op {
            crate::ast::InfixOperator::Equals    => {
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "(assert (=  (S{} 0)  (= (S{} {}) (S{} {}))))\n",
                    self.syms[&tmp].1, d_lhs_n, lhs.1, d_rhs_n, rhs.1).unwrap();

                assert!(t == Type::Bool);
                self.solver.assert(&tmp_s._eq(&&ast::Dynamic::from_ast(&lhs_s._eq(&rhs_s))));
            },
            crate::ast::InfixOperator::Nequals   => {
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "(assert (=  (S{} 0)  (not (= (S{} {}) (S{} {})))))\n",
                    self.syms[&tmp].1, d_lhs_n, lhs.1, d_rhs_n, rhs.1).unwrap();

                assert!(t == Type::Bool);
                self.solver.assert(&tmp_s._eq(&&ast::Dynamic::from_ast(&lhs_s._eq(&rhs_s).not())));
            }
            crate::ast::InfixOperator::Add       => {
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "(assert (=  (S{} 0)  (bvadd (S{} {}) (S{} {}))))\n",
                    self.syms[&tmp].1, d_lhs_n, lhs.1, d_rhs_n, rhs.1).unwrap();

                assert!(t != Type::Bool);
                self.solver.assert(&tmp_s.as_bv().unwrap()._eq(&lhs_s.as_bv().unwrap().bvadd(&rhs_s.as_bv().unwrap())));
            }
            crate::ast::InfixOperator::Subtract  => {
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "(assert (=  (S{} 0)  (bvsub (S{} {}) (S{} {}))))\n",
                    self.syms[&tmp].1, d_lhs_n, lhs.1, d_rhs_n, rhs.1).unwrap();

                assert!(t != Type::Bool);
                self.solver.assert(&tmp_s.as_bv().unwrap()._eq(&lhs_s.as_bv().unwrap().bvsub(&rhs_s.as_bv().unwrap())));
            }
            crate::ast::InfixOperator::Multiply  => {
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "(assert (=  (S{} 0)  (bvmul (S{} {}) (S{} {}))))\n",
                    self.syms[&tmp].1, d_lhs_n, lhs.1, d_rhs_n, rhs.1).unwrap();

                assert!(t != Type::Bool);
                self.solver.assert(&tmp_s.as_bv().unwrap()._eq(&lhs_s.as_bv().unwrap().bvmul(&rhs_s.as_bv().unwrap())));
            }
            crate::ast::InfixOperator::Divide    => {
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "(assert (=  (S{} 0)  (bvudiv (S{} {}) (S{} {}))))\n",
                    self.syms[&tmp].1, d_lhs_n, lhs.1, d_rhs_n, rhs.1).unwrap();

                assert!(t != Type::Bool);
                if signed {
                    self.solver.assert(&tmp_s.as_bv().unwrap()._eq(&lhs_s.as_bv().unwrap().bvsdiv(&rhs_s.as_bv().unwrap())));
                } else {
                    self.solver.assert(&tmp_s.as_bv().unwrap()._eq(&lhs_s.as_bv().unwrap().bvudiv(&rhs_s.as_bv().unwrap())));
                }
            }
            crate::ast::InfixOperator::Bitxor => {
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "(assert (=  (S{} 0)  (bvxnor (S{} {}) (S{} {}))))\n",
                    self.syms[&tmp].1, d_lhs_n, lhs.1, d_rhs_n, rhs.1).unwrap();

                assert!(t != Type::Bool);
                self.solver.assert(&tmp_s.as_bv().unwrap()._eq(&lhs_s.as_bv().unwrap().bvxnor(&rhs_s.as_bv().unwrap())));
            }
            crate::ast::InfixOperator::Booland => {
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "(assert (=  (S{} 0) (and (S{} {}) (S{} {}))        ))\n",
                    self.syms[&tmp].1, d_lhs_n, lhs.1, d_rhs_n, rhs.1).unwrap();

                assert!(t == Type::Bool);
                self.solver.assert(&tmp_s.as_bool().unwrap()._eq(&lhs_s.as_bool().unwrap().and(&[&rhs_s.as_bool().unwrap()])));
            }
            crate::ast::InfixOperator::Boolor => {
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "(assert (=  (S{} 0) (or (S{} {}) (S{} {}))        ))\n",
                    self.syms[&tmp].1, d_lhs_n, lhs.1, d_rhs_n, rhs.1).unwrap();

                assert!(t == Type::Bool);
                self.solver.assert(&tmp_s.as_bool().unwrap()._eq(&lhs_s.as_bool().unwrap().or(&[&rhs_s.as_bool().unwrap()])));
            }
            crate::ast::InfixOperator::Moreeq => {
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "(assert (=  (S{} 0) (bvuge (S{} {}) (S{} {}))        ))\n",
                    self.syms[&tmp].1, d_lhs_n, lhs.1, d_rhs_n, rhs.1).unwrap();

                assert!(t == Type::Bool);
                if signed {
                    self.solver.assert(&tmp_s.as_bool().unwrap()._eq(&lhs_s.as_bv().unwrap().bvsge(&rhs_s.as_bv().unwrap())));
                } else {
                    self.solver.assert(&tmp_s.as_bool().unwrap()._eq(&lhs_s.as_bv().unwrap().bvuge(&rhs_s.as_bv().unwrap())));
                }
            }
            crate::ast::InfixOperator::Lesseq => {
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "(assert (=  (S{} 0) (bvule (S{} {}) (S{} {}))        ))\n",
                    self.syms[&tmp].1, d_lhs_n, lhs.1, d_rhs_n, rhs.1).unwrap();

                assert!(t == Type::Bool);
                if signed {
                    self.solver.assert(&tmp_s.as_bool().unwrap()._eq(&lhs_s.as_bv().unwrap().bvule(&rhs_s.as_bv().unwrap())));
                } else {
                    self.solver.assert(&tmp_s.as_bool().unwrap()._eq(&lhs_s.as_bv().unwrap().bvsle(&rhs_s.as_bv().unwrap())));
                }
            }
            crate::ast::InfixOperator::Lessthan => {
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "(assert (=  (S{} 0) (bvult (S{} {}) (S{} {}))        ))\n",
                    self.syms[&tmp].1, d_lhs_n, lhs.1, d_rhs_n, rhs.1).unwrap();

                assert!(t == Type::Bool);
                if signed {
                    self.solver.assert(&tmp_s.as_bool().unwrap()._eq(&lhs_s.as_bv().unwrap().bvslt(&rhs_s.as_bv().unwrap())));
                } else {
                    self.solver.assert(&tmp_s.as_bool().unwrap()._eq(&lhs_s.as_bv().unwrap().bvult(&rhs_s.as_bv().unwrap())));
                }
            }
            crate::ast::InfixOperator::Morethan => {
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "(assert (=  (S{} 0) (bvugt (S{} {}) (S{} {}))        ))\n",
                    self.syms[&tmp].1, d_lhs_n, lhs.1, d_rhs_n, rhs.1).unwrap();

                assert!(t == Type::Bool);
                if signed {
                    self.solver.assert(&tmp_s.as_bool().unwrap()._eq(&lhs_s.as_bv().unwrap().bvsgt(&rhs_s.as_bv().unwrap())));
                } else {
                    self.solver.assert(&tmp_s.as_bool().unwrap()._eq(&lhs_s.as_bv().unwrap().bvugt(&rhs_s.as_bv().unwrap())));
                }
            }
            crate::ast::InfixOperator::Shiftleft => {
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "(assert (=  (S{} 0) (bvshl (S{} {}) (S{} {}))        ))\n",
                    self.syms[&tmp].1, d_lhs_n, lhs.1, d_rhs_n, rhs.1).unwrap();

                assert!(t != Type::Bool);
                self.solver.assert(&tmp_s.as_bv().unwrap()._eq(&lhs_s.as_bv().unwrap().bvshl(&rhs_s.as_bv().unwrap())));
            }
            crate::ast::InfixOperator::Shiftright => {
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "(assert (=  (S{} 0) (bvlshr (S{} {}) (S{} {}))        ))\n",
                    self.syms[&tmp].1, d_lhs_n, lhs.1, d_rhs_n, rhs.1).unwrap();

                assert!(t != Type::Bool);
                self.solver.assert(&tmp_s.as_bv().unwrap()._eq(&lhs_s.as_bv().unwrap().bvlshr(&rhs_s.as_bv().unwrap())));
            }
            crate::ast::InfixOperator::Modulo => {
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "(assert (=  (S{} 0) (bvsmod (S{} {}) (S{} {}))        ))\n",
                    self.syms[&tmp].1, d_lhs_n, lhs.1, d_rhs_n, rhs.1).unwrap();

                assert!(t != Type::Bool);
                self.solver.assert(&tmp_s.as_bv().unwrap()._eq(&lhs_s.as_bv().unwrap().bvsmod(&rhs_s.as_bv().unwrap())));
            }
            crate::ast::InfixOperator::Bitand => {
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "(assert (=  (S{} 0) (bvand (S{} {}) (S{} {}))        ))\n",
                    self.syms[&tmp].1, d_lhs_n, lhs.1, d_rhs_n, rhs.1).unwrap();

                assert!(t != Type::Bool);
                self.solver.assert(&tmp_s.as_bv().unwrap()._eq(&lhs_s.as_bv().unwrap().bvand(&rhs_s.as_bv().unwrap())));
            }
            crate::ast::InfixOperator::Bitor => {
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "(assert (=  (S{} 0) (bvor (S{} {}) (S{} {}))        ))\n",
                    self.syms[&tmp].1, d_lhs_n, lhs.1, d_rhs_n, rhs.1).unwrap();

                assert!(t != Type::Bool);
                self.solver.assert(&tmp_s.as_bv().unwrap()._eq(&lhs_s.as_bv().unwrap().bvor(&rhs_s.as_bv().unwrap())));
            }
        };
        self.checkpoint();
    }






    pub fn postfix_op(&mut self,
                     to:  TemporalSymbol,
                     from:TemporalSymbol,
                     op:  crate::ast::PostfixOperator,
                     t:   Type,
    ) {
        if self.infinite {
            return;
        }

        let size =  match  t {
            Type::Signed(v) | Type::Unsigned(v) => v,
            Type::Bool => panic!("ICE: postfix_op undefined on bool"),
        };

        let d_from_n    = &self.syms[&from.0].1;
        let d_to_n      = &self.syms[&to.0].1;

        let debug_op = match op {
            crate::ast::PostfixOperator::Increment  => format!("(bvadd (S{} {}) (_ bv1 {}))", d_from_n, from.1, size),
            crate::ast::PostfixOperator::Decrement  => format!("(bvsub (S{} {}) (_ bv1 {}))", d_from_n, from.1, size),
        };
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(assert (= (S{} {}) {} ))\n", d_to_n,  to.1, debug_op).unwrap();

        let from= self.syms[&from.0].0.apply(&[&ast::Dynamic::from_ast(&ast::Int::from_u64(&self.ctx, from.1))]);
        let to  = self.syms[&to.0].0.apply(&[&ast::Dynamic::from_ast(&ast::Int::from_u64(&self.ctx, to.1))]);

        let bone = ast::BV::from_u64(&self.ctx, 1, size);

        let e = match op {
            crate::ast::PostfixOperator::Increment  => ast::Dynamic::from_ast(&from.as_bv().unwrap().bvadd(&bone)),
            crate::ast::PostfixOperator::Decrement  => ast::Dynamic::from_ast(&from.as_bv().unwrap().bvsub(&bone)),
        };
        self.solver.assert(&to._eq(&e));
        self.checkpoint();
    }

    pub fn prefix_op(&mut self,
                     tmp: Symbol,
                     lhs: TemporalSymbol,
                     op:  crate::ast::PrefixOperator,
                     t:   Type,
    ) {
        if self.infinite {
            return;
        }

        match  t {
            Type::Signed(size) | Type::Unsigned(size) => {
                let d_lhs_n = &self.syms[&lhs.0].1;
                let debug_op = match op {
                    crate::ast::PrefixOperator::Boolnot     => format!("(not (= (S{} {})))",   d_lhs_n, lhs.1),
                    crate::ast::PrefixOperator::Bitnot      => format!("(bvxnor (S{} {}) #x{})", d_lhs_n, lhs.1, "ff".repeat(size as usize/8)),
                    _ => unreachable!(),
                };
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "(assert (=  (S{} 0) {} ))\n", self.syms[&tmp].1, debug_op).unwrap();


                let lhs = self.syms[&lhs.0].0.apply(&[&ast::Dynamic::from_ast(&ast::Int::from_u64(&self.ctx, lhs.1))]);

                let btrue   = ast::BV::from_u64(&self.ctx, std::u64::MAX, size);

                let e = match op {
                    crate::ast::PrefixOperator::Boolnot     => ast::Dynamic::from_ast(&lhs.as_bool().unwrap().not()),
                    crate::ast::PrefixOperator::Bitnot      => ast::Dynamic::from_ast(&lhs.as_bv().unwrap().bvxnor(&btrue)),
                    _ => unreachable!(),
                };
                let tmp = self.syms[&tmp].0.apply(&[&ast::Dynamic::from_ast(&ast::Int::from_u64(&self.ctx, 0))]);
                self.solver.assert(&tmp._eq(&e));
            }
            Type::Bool => {
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "(assert (=  (S{} 0) (not (= (S{} {})))))\n",
                    self.syms[&tmp].1, &self.syms[&lhs.0].1, lhs.1).unwrap();
                let lhs = self.syms[&lhs.0].0.apply(&[&ast::Dynamic::from_ast(&ast::Int::from_u64(&self.ctx, lhs.1))]);
                let tmp = self.syms[&tmp].0.apply(&[&ast::Dynamic::from_ast(&ast::Int::from_u64(&self.ctx, 0))]);
                self.solver.assert(&tmp._eq(&ast::Dynamic::from_ast(&lhs.as_bool().unwrap().not())));
            }
        }
        self.checkpoint();
    }

    pub fn constrain(&mut self, lhs: (Symbol, u64), compare: bool) -> bool {
        if self.infinite {
            return true;
        }
        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "; constrain. we know this to be true because of an if condition or callsite assert\n").unwrap();
        if compare {
            write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
            write!(self.debug.borrow_mut(), "(assert (S{} {}))\n", self.syms[&lhs.0].1, lhs.1).unwrap();
        } else {
            write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
            write!(self.debug.borrow_mut(), "(assert (not (S{} {})))\n", self.syms[&lhs.0].1, lhs.1).unwrap();
        }

        let lhs = self.syms[&lhs.0].0.apply(&[&ast::Dynamic::from_ast(&ast::Int::from_u64(&self.ctx, lhs.1))]).as_bool().unwrap();

        if compare {
            self.solver.assert(&lhs);
        } else {
            self.solver.assert(&lhs.not());
        };

        write!(self.debug.borrow_mut(), "{}(check-sat); check after constrain\n", self.debug_indent).unwrap();

        //#[cfg(debug_assertions)]
        //{
        //    self.solve()
        //}
        //#[cfg(not(debug_assertions))]
        {
            true
        }
    }



    // must call from within assert or value
    pub fn extract(&self, model: &ModelRef, lhs: (Symbol, u64)) -> Option<u64> {
        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(get-value (S{} {}))\n", self.syms[&lhs.0].1, lhs.1).unwrap();



        let lhs = self.syms[&lhs.0].0.apply(&[&ast::Dynamic::from_ast(&ast::Int::from_u64(&self.ctx, lhs.1))]);

        let bv = model.0.eval(&lhs, true);
        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), ";  = {:?}\n", bv).unwrap();

        match bv {
            Some(bv) => {
                let bvstring = format!("{:?}", bv);
                debug!("extracted: {}", bv);
                if bvstring == "false" {
                    return Some(0)
                } else if bvstring == "true" {
                    return Some(0xffffffff)
                } else  if bvstring.starts_with("#x") {
                    if let Ok(v) = u64::from_str_radix(&bvstring[2..], 16) {
                        return Some(v)
                    }
                } else {
                }
                None
            },
            None => None,
        }
    }

    pub fn assert<R, F> (&self, lhs: (Symbol, u64), with: F) -> R
        where F : Fn(Assertion<bool>, Option<ModelRef>) -> R,
              R : Sized,
    {


        let lhs_s = self.syms[&lhs.0].0.apply(&[&ast::Dynamic::from_ast(&ast::Int::from_u64(&self.ctx, lhs.1))]).as_bool().unwrap();

        write!(self.debug.borrow_mut(), "{}(echo \"\")\n", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "{}(push)\n", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "{} (echo \"vvv positive assert\")\n", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "{} (assert (S{} {}))\n", self.debug_indent, self.syms[&lhs.0].1, lhs.1).unwrap();
        write!(self.debug.borrow_mut(), "{} (check-sat)\n", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "{}(pop)\n", self.debug_indent).unwrap();

        self.solver.push();
        self.solver.assert(&lhs_s);
        let rs1 = self.solver.check();
        self.solver.pop(1);

        write!(self.debug.borrow_mut(), "{}(push)\n", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "{} (echo \"vvv negative assert. to be constrained, this must be different\")\n"
               , self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "{} (assert (not (S{} {})))\n", self.debug_indent, self.syms[&lhs.0].1, lhs.1).unwrap();
        write!(self.debug.borrow_mut(), "{} (check-sat)\n", self.debug_indent).unwrap();

        self.solver.push();
        self.solver.assert(&lhs_s.not());
        let rs2 = self.solver.check();

        let r = match (rs1, rs2) {
            (SatResult::Sat, SatResult::Sat) => {
                write!(self.debug.borrow_mut(), "{}(echo \"unconstrained\")\n", self.debug_indent).unwrap();

                with(Assertion::Unconstrained(false), Some(ModelRef(self.solver.get_model())))
            }
            (SatResult::Sat, SatResult::Unsat) => {

                write!(self.debug.borrow_mut(), "{} (echo \"constrained true \")\n", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "{}(pop)\n", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "{}(push)\n", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "{} (assert (S{} {}))\n", self.debug_indent, self.syms[&lhs.0].1, lhs.1).unwrap();
                write!(self.debug.borrow_mut(), "{} (check-sat)\n", self.debug_indent).unwrap();

                self.solver.pop(1);
                self.solver.push();
                self.solver.assert(&lhs_s);
                self.solver.check();
                with(Assertion::Constrained(true), Some(ModelRef(self.solver.get_model())))
            }
            (SatResult::Unsat, SatResult::Sat) => {
                write!(self.debug.borrow_mut(), "{} (echo \"constrained false\")\n", self.debug_indent).unwrap();

                with(Assertion::Constrained(false), Some(ModelRef(self.solver.get_model())))
            }
            (SatResult::Unsat, SatResult::Unsat) => {
                write!(self.debug.borrow_mut(), "{} (echo \"both unsat. something broke earlier\")\n", self.debug_indent).unwrap();
                with(Assertion::Unsolveable, None)
            }
            _ => {
                write!(self.debug.borrow_mut(), "{} (echo \"unsolveable\")\n", self.debug_indent).unwrap();
                with(Assertion::Unsolveable, None)
            }
        };
        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(pop)\n").unwrap();
        self.solver.pop(1);
        r
    }

    pub fn value<R, F> (&self, lhs: (Symbol, u64), with: F) -> R
        where F : Fn(Assertion<u64>, Option<ModelRef>) -> R,
              R : Sized
    {
        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(push)\n").unwrap();
        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(check-sat)\n").unwrap();
        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(get-value (S{} {}))\n", self.syms[&lhs.0].1, lhs.1).unwrap();
        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(pop)\n").unwrap();

        let lhs_s = self.syms[&lhs.0].0.apply(&[&ast::Dynamic::from_ast(&ast::Int::from_u64(&self.ctx, lhs.1))]).as_bv().unwrap();


        self.solver.push();
        if !self.solve() {
            return with(Assertion::Unsolveable, None);
        }
        let bv = self.solver.get_model().eval(&lhs_s, true);
        let val = if let Some(bv) = &bv {
            write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
            write!(self.debug.borrow_mut(), ";  = {:?}\n", bv).unwrap();
            let bvstring = format!("{:?}", bv);
            if bvstring.starts_with("#x") {
                if let Ok(v) = u64::from_str_radix(&bvstring[2..], 16) {
                    v
                } else {
                    return with(Assertion::Unsolveable, None);
                }
            } else {
                return with(Assertion::Unsolveable, None);
            }
        } else {
            return with(Assertion::Unsolveable, None);
        };


        self.solver.pop(1);
        self.solver.push();
        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(push)\n").unwrap();
        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(assert (not (= (_ bv{} 64) (S{} {}))))\n", val, self.syms[&lhs.0].1, lhs.1).unwrap();
        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(check-sat)\n").unwrap();
        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(get-value (S{} {}))\n", self.syms[&lhs.0].1, lhs.1).unwrap();
        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(pop)\n").unwrap();
        self.solver.assert(&lhs_s._eq(&bv.unwrap()).not());
        let rr = match self.solve() {
            false => {
                self.solver.pop(1);
                self.solver.push();
                with(Assertion::Constrained(val), Some(ModelRef(self.solver.get_model())))
            }
            true => {
                with(Assertion::Unconstrained(val), Some(ModelRef(self.solver.get_model())))
            }
        };
        self.solver.pop(1);
        rr
    }


    pub fn push(&mut self, reason: &str) {
        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(push); {}\n", reason).unwrap();
        self.debug_indent.push(' ');
        self.solver.push();
    }
    pub fn pop(&mut self, reason: &str) {
        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(pop); {}\n", reason).unwrap();
        self.debug_indent.pop();
        self.solver.pop(1);
    }


    pub fn solve(&self) -> bool {
        match self.solver.check() {
            SatResult::Unsat | SatResult::Unknown => {
                false
            }
            SatResult::Sat => {
                true
            }
        }
    }

    #[cfg(debug_assertions)]
    pub fn checkpoint(&self) {
        //if !self.solve() {
        //    panic!("ICE: function is unsolveable");
        //}
    }
    #[cfg(not(debug_assertions))]
    pub fn checkpoint(&self) {}


    pub fn debug(&mut self, m: &str) {
        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "; {}\n", m).unwrap();
    }
    pub fn debug_loc(&mut self, loc: &crate::ast::Location) {
        //if self.debug_line != loc.line() {
            self.debug_line = loc.line();
            write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
            write!(self.debug.borrow_mut(), "; at {}:{}\n", loc.file, self.debug_line).unwrap();
        //}
    }

    pub fn new(module_name: String) -> Self {
        //Config::set_global_param_value(":model.partial", "true");
        let mut config  = Config::new();
        //config.set_model_generation(true);
        config.set_timeout_msec(1000);
        let ctx     = Box::leak(Box::new(Context::new(&config)));
        let solver  = z3::Solver::new(ctx);


        std::fs::create_dir_all("./target/ssa/").unwrap();
        let outfile = format!("./target/ssa/{}.smt2", module_name);
        let debug = RefCell::new(File::create(&outfile).expect(&format!("cannot create {}", outfile)));

        Self {
            config,
            ctx,
            solver,
            syms: HashMap::new(),
            debug_line: 0,
            debug,
            infinite: false,
            debug_indent: String::new(),
        }
    }

}
