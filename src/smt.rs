use crate::z3::{self, *, ast::Ast};
use crate::symbolic::{Symbol, TemporalSymbol};
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::cell::{RefCell};
use std::sync::atomic::{AtomicUsize, Ordering};
use super::parser::{emit_warn};

pub static TIMEOUT: AtomicUsize = AtomicUsize::new(5000);

pub enum Assertion<T> {
    Constrained(T),
    Unconstrained(T),
    Unsolveable,
}


pub struct Var {
    smtname:    String,
    temp:       HashMap<u64, z3::FuncDecl<'static>>,
    typ:        Type
}

pub struct Solver {
    config:         Config,
    ctx:            &'static Context,
    solver:         z3::Solver<'static>,
    vars:           RefCell<HashMap<Symbol, Var>>,
    theories:       HashMap<Symbol, (z3::FuncDecl<'static>, String)>,
    debug_loc:      crate::ast::Location,
    pub debug:      RefCell<File>,
    debug_indent:   String,


    branches:       Vec<Vec<(z3::ast::Bool<'static>, String)>>,

    symbol_stack:   Vec<Vec<(Symbol, String, Type)>>,
    ded_syms:       HashMap<Symbol, String>,
}



#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Bool,
    Signed(u32),
    Unsigned(u32),
}

pub struct ModelRef(Model<'static>);

impl Solver {

    fn var(&self, sym: &TemporalSymbol) -> (z3::ast::Dynamic<'static>, String) {
        let mut fuckyourust = self.vars.borrow_mut();
        let var = fuckyourust.get_mut(&sym.0).unwrap();
        let name = format!("{}__t{}", var.smtname, sym.1);
        if !var.temp.contains_key(&sym.1) {


            write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
            write!(self.debug.borrow_mut(), "(declare-fun {} () {}); local \n",
                name,
                match var.typ {
                    Type::Bool => format!("Bool"),
                    Type::Signed(s) | Type::Unsigned(s) => format!("(_ BitVec {})", s),
                },
            ).unwrap();

            let f = FuncDecl::new(
                &self.ctx,
                name.clone(),
                &[],
                &match var.typ {
                    Type::Bool => Sort::bool(&self.ctx),
                    Type::Signed(s) | Type::Unsigned(s) => Sort::bitvector(&self.ctx, s),
                }
            );

            var.temp.insert(sym.1, f);
        }
        (var.temp[&sym.1].apply(&[]), name)
    }


    pub fn branch(&mut self) {
        self.branches.push(Vec::new());
    }

    pub fn unbranch(&mut self, returned: bool) {
        if returned {
            if let Some((branch_capi, branch_debug)) = self.build_branch_bundle() {
                let branch_capi = branch_capi.not();
                let branch_debug = format!("(not {})", branch_debug);

                write!(self.debug.borrow_mut(), "; branch returned. the rest of the function only happens \
                   if the condition leading to return never happened\n").unwrap();
                write!(self.debug.borrow_mut(), "{}; {}\n", self.debug_indent, branch_debug).unwrap();

                self.branches.pop();

                // ideally we'd just inject all the previous conditions as negative branch conditions, but this bloats up the model. 
                //
                //   self.branches.first_mut().unwrap().push((branch_capi, branch_debug));
                //
                //
                // but we can simply assert it as negative, because symbolic execution is linear anyway. it wont ask about the previous stuff

                self.solver.assert(&branch_capi);
                write!(self.debug.borrow_mut(), "{} (assert {})\n", self.debug_indent, branch_debug).unwrap();

                return;
            }
        }
        self.branches.pop();
    }

    pub fn theory(&mut self, sym: Symbol, args: Vec<Type>, name: &str, t: Type) {
        let lname = format!("theory{}_{}", sym, name.replace(|c: char| !c.is_ascii_alphanumeric(), "_"));

        let mut capi_args = Vec::new();
        let mut debug_args = Vec::new();
        for t in args {
            match t {
                Type::Bool => {
                    capi_args.push(Sort::bool(&self.ctx));
                    debug_args.push("Bool".to_string());
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
                write!(self.debug.borrow_mut(), "(declare-fun {} ({}) (_ BitVec 64)); theory {}\n", lname, debug_args, name).unwrap();
                FuncDecl::new(
                    &self.ctx,
                    lname.clone(),
                    &capi_args[..],
                    &Sort::bitvector(&self.ctx, size),
                )
            }
            Type::Bool => {
                write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
                write!(self.debug.borrow_mut(), "(declare-fun {} ({}) Bool); theory {}\n", lname, debug_args, name).unwrap();
                FuncDecl::new(
                    &self.ctx,
                    lname.clone(),
                    &capi_args[..],
                    &Sort::bool(&self.ctx),
                )
            }
        };
        self.theories.insert(sym, (f, lname));
        self.checkpoint();
    }

    pub fn invocation(&mut self, theory: Symbol, args: Vec<TemporalSymbol>, tmp: TemporalSymbol) {

        let mut capi_args = Vec::new();
        let mut debug_args = Vec::new();
        for arg in &args {
            let arg = self.var(&arg);
            debug_args.push(arg.1);
            capi_args.push(arg.0);
        }

        let debug_args = debug_args.join(" ");
        let capi_args  = capi_args.iter().map(|s|s).collect::<Vec<_>>();

        let (tmp_capi, tmp_debug)  = self.var(&tmp);

        let theory = &self.theories[&theory];
        write!(self.debug.borrow_mut(), "{}(assert (= {} ({} {}) ))\n",
            self.debug_indent, tmp_debug, theory.1, debug_args).unwrap();

        let call = theory.0.apply(&capi_args[..]);
        self.solver.assert(&tmp_capi._eq(&call));
        self.checkpoint();
    }


    pub fn declare(&mut self, sym: Symbol, name: &str, typ: Type) {
        let smtname = format!("var{}_{}", sym, name.replace(|c: char| !c.is_ascii_alphanumeric(), "_"));

        self.vars.borrow_mut().insert(sym, Var{
            smtname,
            typ: typ.clone(),
            temp: HashMap::new(),
        });

        self.symbol_stack.last_mut().as_mut().unwrap().push((sym, name.to_string(), typ));
        self.checkpoint();
    }


    //TODO could be faster by preparing the joins
    fn build_branch_bundle(&self) -> Option<(z3::ast::Bool<'static>, String)> {
        let mut debug = Vec::new();
        let mut capi  = Vec::new();
        for branch in &self.branches {
            for (branch_capi, branch_debug) in branch {
                debug.push(branch_debug.clone());
                capi.push(branch_capi);
            }
        }

        if capi.len() == 0 {
            return None;
        }

        Some((
            ast::Bool::from_bool(&self.ctx, true).and(&capi),
            format!("( and {} )", debug.join(" "))
        ))
    }


    /// an assign that happens depending on branch conditions
    pub fn assign_branch(&mut self, lhs: TemporalSymbol, rhs_if : TemporalSymbol, rhs_else : TemporalSymbol, t: Type) {
        let (branch_capi, branch_debug) = match self.build_branch_bundle() {
            Some(v) => v,
            None => (ast::Bool::from_bool(&self.ctx, true), " true ".to_string()),
        };

        let (capi_lhs,      smt_lhs)      = self.var(&lhs);
        let (capi_if,       mut smt_if)   = self.var(&rhs_if);
        let (capi_else,     mut smt_else) = self.var(&rhs_else);

        if t == Type::Bool {
            let capi_lhs = capi_lhs.as_bool().unwrap();
            // cast to bool
            let capi_if = match capi_if.as_bool() {
                Some(b) => b,
                None => {
                    let capi_if = capi_if.as_bv().unwrap();
                    smt_if = format!("(bvuge {} (_bv1 {})", smt_if, capi_if.get_size());

                    let bone = ast::BV::from_u64(&self.ctx, 1, capi_if.get_size());
                    capi_if.bvuge(&bone)
                }
            };
            let capi_else = match capi_else.as_bool() {
                Some(b) => b,
                None => {
                    let capi_else = capi_else.as_bv().unwrap();
                    smt_else = format!("(bvuge {} (_bv1 {})", smt_else, capi_else.get_size());

                    let bone = ast::BV::from_u64(&self.ctx, 1, capi_else.get_size());
                    capi_else.bvuge(&bone)
                }
            };

            write!(self.debug.borrow_mut(), "{}(assert (= {}  (ite {} {} {})  ))\n",
                self.debug_indent,
                smt_lhs,
                branch_debug,
                smt_if,
                smt_else,
            ).unwrap();

            self.solver.assert(&capi_lhs._eq(&branch_capi.ite(&capi_if, &capi_else)));
            self.checkpoint();

            return;
        }


        let capi_lhs        = capi_lhs.as_bv().unwrap();
        let mut capi_if     = capi_if.as_bv().unwrap();
        let mut capi_else   = capi_else.as_bv().unwrap();
        let lhs_size        = capi_lhs.get_size();

        let if_size = capi_if.get_size();
        if lhs_size < if_size {
            smt_if  = format!("( (_ extract {} {}) {} )", lhs_size - 1, 0, smt_if);
            capi_if = capi_if.extract(lhs_size -1, 0);
        } else if lhs_size > if_size {
            smt_if = format!("( (zero_extend {}) {} )", lhs_size - if_size, smt_if);
            capi_if = capi_if.zero_ext(lhs_size - if_size);
        }

        let else_size = capi_else.get_size();
        if lhs_size < else_size {
            smt_else  = format!("( (_ extract {} {}) {} )", lhs_size - 1, 0, smt_else);
            capi_else = capi_else.extract(lhs_size -1, 0);
        } else if lhs_size > else_size {
            smt_else = format!("( (zero_extend {}) {} )", lhs_size - else_size, smt_else);
            capi_else = capi_else.zero_ext(lhs_size - else_size);
        }

        write!(self.debug.borrow_mut(), "{}(assert (= {}  (ite {} {} {})  ))\n",
            self.debug_indent,
            smt_lhs,
            branch_debug,
            smt_if,
            smt_else,
        ).unwrap();

        self.solver.assert(&capi_lhs._eq(&branch_capi.ite(&capi_if, &capi_else)));

        self.checkpoint();
    }

    pub fn assign(&mut self, lhs: TemporalSymbol, rhs: TemporalSymbol, t: Type) {
        let (capi_lhs,  smt_lhs)        = self.var(&lhs);
        let (capi_rhs,  mut smt_rhs)    = self.var(&rhs);

        if t == Type::Bool {
            let capi_lhs = capi_lhs.as_bool().unwrap();
            // cast to bool
            let capi_rhs = match capi_rhs.as_bool() {
                Some(b) => b,
                None => {
                    let capi_rhs = capi_rhs.as_bv().unwrap();
                    smt_rhs = format!("(bvuge {} (_bv1 {})", smt_rhs, capi_rhs.get_size());

                    let bone = ast::BV::from_u64(&self.ctx, 1, capi_rhs.get_size());
                    capi_rhs.bvuge(&bone)
                }
            };

            write!(self.debug.borrow_mut(), "{}(assert (= {}  {} ))\n",
                self.debug_indent,
                smt_lhs,
                smt_rhs,
            ).unwrap();

            self.solver.assert(&capi_lhs._eq(&capi_rhs));
            self.checkpoint();

            return;
        }


        let capi_lhs        = capi_lhs.as_bv().unwrap();
        let mut capi_rhs    = capi_rhs.as_bv().unwrap();
        let lhs_size        = capi_lhs.get_size();

        let rhs_size = capi_rhs.get_size();
        if lhs_size < rhs_size {
            smt_rhs  = format!("( (_ extract {} {}) {} )", lhs_size - 1, 0, smt_rhs);
            capi_rhs = capi_rhs.extract(lhs_size -1, 0);
        } else if lhs_size > rhs_size {
            smt_rhs = format!("( (zero_extend {}) {} )", lhs_size - rhs_size, smt_rhs);
            capi_rhs = capi_rhs.zero_ext(lhs_size - rhs_size);
        }

        write!(self.debug.borrow_mut(), "{}(assert (= {}  {} ))\n",
            self.debug_indent,
            smt_lhs,
            smt_rhs,
        ).unwrap();

        self.solver.assert(&capi_lhs._eq(&capi_rhs));
        self.checkpoint();


    }

    pub fn literal(&mut self, tmp: Symbol, val: u64, typ: Type) {

        let (capi_lhs, smt_lhs) = self.var(&(tmp,0));

        match typ {
            Type::Unsigned(size) | Type::Signed(size) => {
                write!(self.debug.borrow_mut(), "{}(assert (= {} (_ bv{} {})))\n",
                    self.debug_indent,
                    smt_lhs,
                    val,
                    size
                ).unwrap();

                let rhs = ast::BV::from_u64(&self.ctx, val, size);
                self.solver.assert(&capi_lhs.as_bv().unwrap()._eq(&rhs));
            }
            Type::Bool => {
                if val > 0 {
                    write!(self.debug.borrow_mut(), "{}(assert (= {} true))\n",
                        self.debug_indent,
                        smt_lhs,
                    ).unwrap();

                    let rhs = ast::Bool::from_bool(&self.ctx, true);
                    self.solver.assert(&capi_lhs.as_bool().unwrap()._eq(&rhs));
                } else {
                    write!(self.debug.borrow_mut(), "{}(assert (= {} false))\n",
                        self.debug_indent,
                        smt_lhs,
                    ).unwrap();

                    let rhs = ast::Bool::from_bool(&self.ctx, false);
                    self.solver.assert(&capi_lhs.as_bool().unwrap()._eq(&rhs));
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
        let (capi_lhs,  smt_lhs)    = self.var(&lhs);
        let (capi_rhs,  smt_rhs)    = self.var(&rhs);

        let capi_lhs = capi_lhs.as_bv().unwrap();
        let capi_rhs = capi_rhs.as_bv().unwrap();

        match op {
            crate::ast::InfixOperator::Add       => {
                self.solver.push();
                self.solver.assert(&capi_lhs.to_int(false).add(&[&capi_rhs.to_int(false)])
                     ._eq(&capi_lhs.bvadd(&capi_rhs).to_int(false)));
                let r = self.solve();
                self.solver.pop(1);
                !r
            }
            crate::ast::InfixOperator::Subtract  => {
                self.solver.push();
                self.solver.assert(&capi_lhs.to_int(false).sub(&[&capi_rhs.to_int(false)])
                    ._eq(&capi_lhs.bvsub(&capi_rhs).to_int(false)));
                let r = self.solve();
                self.solver.pop(1);
                !r
            }
            crate::ast::InfixOperator::Multiply  => {
                self.solver.push();
                self.solver.assert(&capi_lhs.to_int(false).mul(&[&capi_rhs.to_int(false)])
                    ._eq(&capi_lhs.bvmul(&capi_rhs).to_int(false)));
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
        let (capi_tmp,  smt_tmp)    = self.var(&(tmp,0));
        let (capi_lhs,  smt_lhs)    = self.var(&lhs);
        let (capi_rhs,  smt_rhs)    = self.var(&rhs);

        match op {
            crate::ast::InfixOperator::Equals    => {
                assert!(t == Type::Bool);

                write!(self.debug.borrow_mut(), "{}(assert (=  {} (= {} {})))\n",
                    self.debug_indent,
                    smt_tmp, smt_lhs, smt_rhs).unwrap();

                self.solver.assert(&capi_tmp._eq(&&ast::Dynamic::from_ast(&capi_lhs._eq(&capi_rhs))));
            },
            crate::ast::InfixOperator::Nequals   => {
                assert!(t == Type::Bool);

                write!(self.debug.borrow_mut(), "{}(assert (=  {} (not (= {} {}))))\n",
                    self.debug_indent,
                    smt_tmp, smt_lhs, smt_rhs).unwrap();

                self.solver.assert(&capi_tmp._eq(&&ast::Dynamic::from_ast(&capi_lhs._eq(&capi_rhs).not())));
            }
            crate::ast::InfixOperator::Add       => {
                assert!(t != Type::Bool);

                write!(self.debug.borrow_mut(), "{}(assert (=  {} (bvadd {} {})))\n",
                    self.debug_indent,
                    smt_tmp, smt_lhs, smt_rhs).unwrap();

                self.solver.assert(&capi_tmp.as_bv().unwrap()._eq(&capi_lhs.as_bv().unwrap().bvadd(&capi_rhs.as_bv().unwrap())));
            }
            crate::ast::InfixOperator::Subtract  => {
                assert!(t != Type::Bool);

                write!(self.debug.borrow_mut(), "{}(assert (=  {} (bvsub {} {})))\n",
                    self.debug_indent,
                    smt_tmp, smt_lhs, smt_rhs).unwrap();

                self.solver.assert(&capi_tmp.as_bv().unwrap()._eq(&capi_lhs.as_bv().unwrap().bvsub(&capi_rhs.as_bv().unwrap())));
            }
            crate::ast::InfixOperator::Multiply  => {
                assert!(t != Type::Bool);

                write!(self.debug.borrow_mut(), "{}(assert (=  {} (bvmul {} {})))\n",
                    self.debug_indent,
                    smt_tmp, smt_lhs, smt_rhs).unwrap();

                self.solver.assert(&capi_tmp.as_bv().unwrap()._eq(&capi_lhs.as_bv().unwrap().bvmul(&capi_rhs.as_bv().unwrap())));
            }
            crate::ast::InfixOperator::Divide    => {
                assert!(t != Type::Bool);

                if signed {
                    write!(self.debug.borrow_mut(), "{}(assert (=  {} (bvsdiv {} {})))\n",
                        self.debug_indent,
                        smt_tmp, smt_lhs, smt_rhs).unwrap();

                    self.solver.assert(&capi_tmp.as_bv().unwrap()._eq(&capi_lhs.as_bv().unwrap().bvsdiv(&capi_rhs.as_bv().unwrap())));
                } else {
                    write!(self.debug.borrow_mut(), "{}(assert (=  {} (bvudiv {} {})))\n",
                        self.debug_indent,
                        smt_tmp, smt_lhs, smt_rhs).unwrap();

                    self.solver.assert(&capi_tmp.as_bv().unwrap()._eq(&capi_lhs.as_bv().unwrap().bvudiv(&capi_rhs.as_bv().unwrap())));
                }
            }
            crate::ast::InfixOperator::Bitxor => {
                assert!(t != Type::Bool);

                write!(self.debug.borrow_mut(), "{}(assert (=  {} (bvxnor {} {})))\n",
                    self.debug_indent,
                    smt_tmp, smt_lhs, smt_rhs).unwrap();

                self.solver.assert(&capi_tmp.as_bv().unwrap()._eq(&capi_lhs.as_bv().unwrap().bvxnor(&capi_rhs.as_bv().unwrap())));
            }
            crate::ast::InfixOperator::Booland => {
                assert!(t == Type::Bool);

                write!(self.debug.borrow_mut(), "{}(assert (=  {} (and {} {})))\n",
                    self.debug_indent,
                    smt_tmp, smt_lhs, smt_rhs).unwrap();

                self.solver.assert(&capi_tmp.as_bool().unwrap()._eq(&capi_lhs.as_bool().unwrap().and(&[&capi_rhs.as_bool().unwrap()])));
            }
            crate::ast::InfixOperator::Boolor => {
                assert!(t == Type::Bool);

                write!(self.debug.borrow_mut(), "{}(assert (=  {} (or {} {})))\n",
                    self.debug_indent,
                    smt_tmp, smt_lhs, smt_rhs).unwrap();

                self.solver.assert(&capi_tmp.as_bool().unwrap()._eq(&capi_lhs.as_bool().unwrap().or(&[&capi_rhs.as_bool().unwrap()])));
            }
            crate::ast::InfixOperator::Moreeq => {
                assert!(t == Type::Bool);

                if signed {
                    write!(self.debug.borrow_mut(), "{}(assert (=  {} (bvsge {} {})))\n",
                        self.debug_indent,
                        smt_tmp, smt_lhs, smt_rhs).unwrap();

                    self.solver.assert(&capi_tmp.as_bool().unwrap()._eq(&capi_lhs.as_bv().unwrap().bvsge(&capi_rhs.as_bv().unwrap())));
                } else {
                    write!(self.debug.borrow_mut(), "{}(assert (=  {} (bvuge {} {})))\n",
                        self.debug_indent,
                        smt_tmp, smt_lhs, smt_rhs).unwrap();

                    self.solver.assert(&capi_tmp.as_bool().unwrap()._eq(&capi_lhs.as_bv().unwrap().bvuge(&capi_rhs.as_bv().unwrap())));
                }
            }
            crate::ast::InfixOperator::Lesseq => {
                assert!(t == Type::Bool);

                if signed {
                    write!(self.debug.borrow_mut(), "{}(assert (=  {} (bvsle {} {})))\n",
                        self.debug_indent,
                        smt_tmp, smt_lhs, smt_rhs).unwrap();

                    self.solver.assert(&capi_tmp.as_bool().unwrap()._eq(&capi_lhs.as_bv().unwrap().bvsle(&capi_rhs.as_bv().unwrap())));
                } else {
                    write!(self.debug.borrow_mut(), "{}(assert (=  {} (bvule {} {})))\n",
                        self.debug_indent,
                        smt_tmp, smt_lhs, smt_rhs).unwrap();

                    self.solver.assert(&capi_tmp.as_bool().unwrap()._eq(&capi_lhs.as_bv().unwrap().bvule(&capi_rhs.as_bv().unwrap())));
                }
            }
            crate::ast::InfixOperator::Lessthan => {
                assert!(t == Type::Bool);

                if signed {
                    write!(self.debug.borrow_mut(), "{}(assert (=  {} (bvslt {} {})))\n",
                        self.debug_indent,
                        smt_tmp, smt_lhs, smt_rhs).unwrap();

                    self.solver.assert(&capi_tmp.as_bool().unwrap()._eq(&capi_lhs.as_bv().unwrap().bvslt(&capi_rhs.as_bv().unwrap())));
                } else {
                    write!(self.debug.borrow_mut(), "{}(assert (=  {} (bvult {} {})))\n",
                        self.debug_indent,
                        smt_tmp, smt_lhs, smt_rhs).unwrap();

                    self.solver.assert(&capi_tmp.as_bool().unwrap()._eq(&capi_lhs.as_bv().unwrap().bvult(&capi_rhs.as_bv().unwrap())));
                }
            }
            crate::ast::InfixOperator::Morethan => {
                assert!(t == Type::Bool);

                if signed {
                    write!(self.debug.borrow_mut(), "{}(assert (=  {} (bvsgt {} {})))\n",
                        self.debug_indent,
                        smt_tmp, smt_lhs, smt_rhs).unwrap();

                    self.solver.assert(&capi_tmp.as_bool().unwrap()._eq(&capi_lhs.as_bv().unwrap().bvsgt(&capi_rhs.as_bv().unwrap())));
                } else {
                    write!(self.debug.borrow_mut(), "{}(assert (=  {} (bvugt {} {})))\n",
                        self.debug_indent,
                        smt_tmp, smt_lhs, smt_rhs).unwrap();

                    self.solver.assert(&capi_tmp.as_bool().unwrap()._eq(&capi_lhs.as_bv().unwrap().bvugt(&capi_rhs.as_bv().unwrap())));
                }
            }
            crate::ast::InfixOperator::Shiftleft => {
                assert!(t != Type::Bool);

                write!(self.debug.borrow_mut(), "{}(assert (=  {} (bvshl {} {})))\n",
                    self.debug_indent,
                    smt_tmp, smt_lhs, smt_rhs).unwrap();

                self.solver.assert(&capi_tmp.as_bv().unwrap()._eq(&capi_lhs.as_bv().unwrap().bvshl(&capi_rhs.as_bv().unwrap())));
            }
            crate::ast::InfixOperator::Shiftright => {
                assert!(t != Type::Bool);

                write!(self.debug.borrow_mut(), "{}(assert (=  {} (bvlshr {} {})))\n",
                    self.debug_indent,
                    smt_tmp, smt_lhs, smt_rhs).unwrap();

                self.solver.assert(&capi_tmp.as_bv().unwrap()._eq(&capi_lhs.as_bv().unwrap().bvlshr(&capi_rhs.as_bv().unwrap())));
            }
            crate::ast::InfixOperator::Modulo => {
                assert!(t != Type::Bool);

                write!(self.debug.borrow_mut(), "{}(assert (=  {} (bvsmod {} {})))\n",
                    self.debug_indent,
                    smt_tmp, smt_lhs, smt_rhs).unwrap();

                self.solver.assert(&capi_tmp.as_bv().unwrap()._eq(&capi_lhs.as_bv().unwrap().bvsmod(&capi_rhs.as_bv().unwrap())));
            }
            crate::ast::InfixOperator::Bitand => {
                assert!(t != Type::Bool);

                write!(self.debug.borrow_mut(), "{}(assert (=  {} (bvand {} {})))\n",
                    self.debug_indent,
                    smt_tmp, smt_lhs, smt_rhs).unwrap();

                self.solver.assert(&capi_tmp.as_bv().unwrap()._eq(&capi_lhs.as_bv().unwrap().bvand(&capi_rhs.as_bv().unwrap())));
            }
            crate::ast::InfixOperator::Bitor => {
                assert!(t != Type::Bool);

                write!(self.debug.borrow_mut(), "{}(assert (=  {} (bvor {} {})))\n",
                    self.debug_indent,
                    smt_tmp, smt_lhs, smt_rhs).unwrap();

                self.solver.assert(&capi_tmp.as_bv().unwrap()._eq(&capi_lhs.as_bv().unwrap().bvor(&capi_rhs.as_bv().unwrap())));
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

        let size =  match  t {
            Type::Signed(v) | Type::Unsigned(v) => v,
            Type::Bool => panic!("ICE: postfix_op undefined on bool"),
        };

        let (capi_to,       smt_to)    = self.var(&to);
        let (capi_from,     smt_from)  = self.var(&from);

        let smt_op = match op {
            crate::ast::PostfixOperator::Increment  => format!("(bvadd {} (_ bv1 {}))", smt_from, size),
            crate::ast::PostfixOperator::Decrement  => format!("(bvsub {} (_ bv1 {}))", smt_from, size),
        };
        write!(self.debug.borrow_mut(), "{}(assert (= {} {} ))\n", self.debug_indent, smt_to, smt_op).unwrap();

        let bone = ast::BV::from_u64(&self.ctx, 1, size);

        let e = match op {
            crate::ast::PostfixOperator::Increment  => ast::Dynamic::from_ast(&capi_from.as_bv().unwrap().bvadd(&bone)),
            crate::ast::PostfixOperator::Decrement  => ast::Dynamic::from_ast(&capi_from.as_bv().unwrap().bvsub(&bone)),
        };
        self.solver.assert(&capi_to._eq(&e));
        self.checkpoint();
    }

    pub fn prefix_op(&mut self,
        to:  TemporalSymbol,
        from:TemporalSymbol,
        op:  crate::ast::PrefixOperator,
        t:   Type,
    ) {

        let (capi_to,       smt_to)    = self.var(&to);
        let (capi_from,     smt_from)  = self.var(&from);

        match  t {
            Type::Signed(size) | Type::Unsigned(size) => {
                let smt_op = match op {
                    crate::ast::PrefixOperator::Boolnot    => format!("(not (= {} ))", smt_from),
                    crate::ast::PrefixOperator::Bitnot     => format!("(bvxnor {} #x{} )", smt_from, "ff".repeat(size as usize/8)),
                    crate::ast::PrefixOperator::Increment  => format!("(bvadd {} (_ bv1 {}))", smt_from, size),
                    crate::ast::PrefixOperator::Decrement  => format!("(bvsub {} (_ bv1 {}))", smt_from, size),
                    _ => unreachable!(),
                };
                write!(self.debug.borrow_mut(), "{}(assert (=  {} {} ))\n",
                    self.debug_indent, smt_to, smt_op).unwrap();

                let btrue = ast::BV::from_u64(&self.ctx, std::u64::MAX, size);
                let bone  = ast::BV::from_u64(&self.ctx, 1, size);

                let e = match op {
                    crate::ast::PrefixOperator::Boolnot    => ast::Dynamic::from_ast(&capi_from.as_bool().unwrap().not()),
                    crate::ast::PrefixOperator::Bitnot     => ast::Dynamic::from_ast(&capi_from.as_bv().unwrap().bvxnor(&btrue)),
                    crate::ast::PrefixOperator::Increment  => ast::Dynamic::from_ast(&capi_from.as_bv().unwrap().bvadd(&bone)),
                    crate::ast::PrefixOperator::Decrement  => ast::Dynamic::from_ast(&capi_from.as_bv().unwrap().bvsub(&bone)),
                    _ => unreachable!(),
                };
                self.solver.assert(&capi_to._eq(&e));
            }
            Type::Bool => {
                assert!(op == crate::ast::PrefixOperator::Boolnot);
                write!(self.debug.borrow_mut(), "{}(assert (=  {} (not {} )))\n",
                    self.debug_indent,
                    smt_to,
                    smt_from
                ).unwrap();

                self.solver.assert(&capi_to._eq(&ast::Dynamic::from_ast(&capi_from.as_bool().unwrap().not())));
            }
        }
        self.checkpoint();
    }

    pub fn constrain_branch(&mut self, lhs: TemporalSymbol, positive: bool) {
        let (capi_lhs, smt_lhs)  = self.var(&lhs);

        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        let smt = if positive {
            write!(self.debug.borrow_mut(), "; branch constrain. this condition is true because we entered a branch\n").unwrap();
            smt_lhs
        } else {
            write!(self.debug.borrow_mut(), "; branch constrain. this condition is false because we're in an else\n").unwrap();
            format!("(not {})", smt_lhs)
        };
        write!(self.debug.borrow_mut(), "{}; {}\n", self.debug_indent, smt).unwrap();


        let capi = capi_lhs.as_bool().unwrap();
        if positive {
            self.branches.last_mut().unwrap().push((capi, smt));
        } else {
            self.branches.last_mut().unwrap().push((capi.not(), smt));
        };
    }

    pub fn attest(&mut self, lhs: TemporalSymbol, compare: bool) -> bool {
        let (capi_lhs, smt_lhs)  = self.var(&lhs);

        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "; attest. where/model clauses or manual attestation\n").unwrap();
        if compare {
            write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
            write!(self.debug.borrow_mut(), "(assert {})\n", smt_lhs).unwrap();
        } else {
            write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
            write!(self.debug.borrow_mut(), "(assert (not {}))\n", smt_lhs).unwrap();
        }

        let capi_lhs = capi_lhs.as_bool().unwrap();

        if compare {
            self.solver.assert(&capi_lhs);
        } else {
            self.solver.assert(&capi_lhs.not());
        };

        write!(self.debug.borrow_mut(), "{}(check-sat); check after constrain\n", self.debug_indent).unwrap();

        self.solve()
        //#[cfg(debug_assertions)]
        //{
        //    self.solve()
        //}
        //#[cfg(not(debug_assertions))]
        //{
        //    true
        //}
    }



    // must call from within assert or value
    pub fn extract(&self, model: &ModelRef, lhs: TemporalSymbol) -> Option<u64> {

        let (capi_lhs, smt_lhs)  = self.var(&lhs);

        write!(self.debug.borrow_mut(), "{}(get-value ({}))\n", self.debug_indent, smt_lhs).unwrap();

        let bv = model.0.eval(&capi_lhs, true);
        write!(self.debug.borrow_mut(), "{};  = {:?}\n", self.debug_indent, bv).unwrap();

        match bv {
            Some(bv) => {
                let bvstring = format!("{:?}", bv);
                debug!("extracted: {}", bv);
                if bvstring == "false" {
                    return Some(0)
                } else if bvstring == "true" {
                    return Some(1)
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

    // asserts are false if
    //  - we reached it due to branch flow
    //  - the opposite of the assert condition is solveable
    // assert(a) ->   !solveable(assert(branch && !a));
    pub fn assert<R, F> (&self, lhs: Vec<(Symbol, u64)>, with: F) -> R
        where F : Fn(bool, Option<ModelRef>) -> R,
              R : Sized,
    {
        let (branch_capi, branch_debug) = match self.build_branch_bundle() {
            Some(v) => v,
            None => (ast::Bool::from_bool(&self.ctx, true), " true ".to_string()),
        };

        assert!(lhs.len() > 0);

        let mut asserts_capi  = Vec::new();
        let mut asserts_debug = Vec::new();
        for lhs in lhs {
            let (capi_lhs, smt_lhs)  = self.var(&lhs);
            asserts_capi.push(capi_lhs.as_bool().unwrap().not());
            asserts_debug.push(format!("(not {} )", smt_lhs));
        }
        let asserts_capi  : Vec<_> = asserts_capi.iter().map(|s|s).collect();

        write!(self.debug.borrow_mut(), "{}(echo \"\")\n", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "{}(push)\n", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "{} (echo \"vvv assert (if any conditions could be negative/sat, the assert fails)\")\n",
            self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "{} (assert (and {} (or {} )))\n",
            self.debug_indent,
            branch_debug,
            asserts_debug.join(" ")
        ).unwrap();
        write!(self.debug.borrow_mut(), "{} (check-sat)\n", self.debug_indent).unwrap();





        let assertion = branch_capi.and(&[&ast::Bool::from_bool(&self.ctx, false).or(&asserts_capi)]);


        // broken on old z3
        //let rs = self.solver.check_assumptions(&[assertion]);

        self.solver.push();
        self.solver.assert(&assertion);
        let rs = self.solver.check();
        self.solver.pop(1);

        let r = match rs {
            SatResult::Sat  => {
                write!(self.debug.borrow_mut(), "{}(echo \"sat / failed\")\n", self.debug_indent).unwrap();
                with(false, Some(ModelRef(self.solver.get_model())))
            }
            SatResult::Unsat => {
                write!(self.debug.borrow_mut(), "{}(echo \"unsat / pass \")\n", self.debug_indent).unwrap();
                with(true, None)
            }
            SatResult::Unknown  => {
                warn!("sat solver took too long. you can increase timeout with --smt-timeout=10000");
                write!(self.debug.borrow_mut(), "{} (echo \"unsolveable\")\n", self.debug_indent).unwrap();
                with(false, None)
            }
        };
        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(pop)\n").unwrap();
        r
    }

    pub fn value<R, F> (&self, lhs: TemporalSymbol, with: F) -> R
        where F : Fn(Assertion<u64>, Option<ModelRef>) -> R,
              R : Sized
    {
        let (capi_lhs, smt_lhs)  = self.var(&lhs);

        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(push)\n").unwrap();
        write!(self.debug.borrow_mut(), " {}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), " (check-sat)\n").unwrap();
        write!(self.debug.borrow_mut(), " {}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), " (get-value ({}) )\n", smt_lhs).unwrap();
        write!(self.debug.borrow_mut(), " {}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(pop)\n").unwrap();


        let capi_lhs = capi_lhs.as_bv().unwrap();

        self.solver.push();
        if !self.solve() {
            warn!("model broke earlier");
            return with(Assertion::Unsolveable, None);
        }
        let bv = self.solver.get_model().eval(&capi_lhs, true);
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
        let bv = bv.unwrap();


        self.solver.pop(1);
        self.solver.push();
        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(push)\n").unwrap();
        write!(self.debug.borrow_mut(), " {}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), " (assert (not (= (_ bv{} {}) {})))\n", val, bv.get_size(),
               smt_lhs).unwrap();
        write!(self.debug.borrow_mut(), " {}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), " (check-sat)\n").unwrap();
        write!(self.debug.borrow_mut(), " {}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), " (get-value ({}))\n", smt_lhs).unwrap();
        write!(self.debug.borrow_mut(), " {}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(pop)\n").unwrap();
        self.solver.assert(&capi_lhs._eq(&bv).not());
        let rr = match self.solve() {
            false => {
                self.solver.pop(1);
                self.solver.push();
                if self.solve() {
                    with(Assertion::Constrained(val), Some(ModelRef(self.solver.get_model())))
                } else {
                    with(Assertion::Constrained(val), None)
                }
            }
            true => {
                with(Assertion::Unconstrained(val), Some(ModelRef(self.solver.get_model())))
            }
        };
        self.solver.pop(1);
        rr
    }

    pub fn bool_value<R, F> (&self, lhs: (Symbol, u64), with: F) -> R
        where F : Fn(Assertion<bool>, Option<ModelRef>) -> R,
              R : Sized
    {
        let (capi_lhs, smt_lhs)  = self.var(&lhs);

        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(push)\n").unwrap();
        write!(self.debug.borrow_mut(), " {}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), " (check-sat)\n").unwrap();
        write!(self.debug.borrow_mut(), " {}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), " (get-value ({}))\n", smt_lhs).unwrap();
        write!(self.debug.borrow_mut(), " {}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(pop)\n").unwrap();


        let capi_lhs = capi_lhs.as_bool().unwrap();


        self.solver.push();
        if !self.solve() {
            warn!("model broke earlier");
            return with(Assertion::Unsolveable, None);
        }
        let bv = self.solver.get_model().eval(&capi_lhs, true);
        let val = if let Some(bv) = &bv {
            write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
            write!(self.debug.borrow_mut(), ";  = {:?}\n", bv).unwrap();
            let bvstring = format!("{:?}", bv);
            if bvstring.contains("true") {
                true
            } else if bvstring.contains("false") {
                false
            } else {
                return with(Assertion::Unsolveable, None);
            }
        } else {
            return with(Assertion::Unsolveable, None);
        };
        let bv = bv.unwrap();


        self.solver.pop(1);
        self.solver.push();
        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), " (push)\n").unwrap();
        write!(self.debug.borrow_mut(), " {}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), " (assert (not (= {} {} )))\n", val, smt_lhs).unwrap();
        write!(self.debug.borrow_mut(), " {}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), " (check-sat)\n").unwrap();
        write!(self.debug.borrow_mut(), " {}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), " (get-value ({}))\n", smt_lhs).unwrap();
        write!(self.debug.borrow_mut(), " {}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(pop)\n").unwrap();
        self.solver.assert(&capi_lhs._eq(&bv).not());
        let rr = match self.solve() {
            false => {
                self.solver.pop(1);
                self.solver.push();
                if self.solve() {
                    with(Assertion::Constrained(val), Some(ModelRef(self.solver.get_model())))
                } else {
                    with(Assertion::Constrained(val), None)
                }
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


        self.symbol_stack.push(Vec::new());
    }
    pub fn pop(&mut self, reason: &str) {
        write!(self.debug.borrow_mut(), "{}", self.debug_indent).unwrap();
        write!(self.debug.borrow_mut(), "(pop); {}\n", reason).unwrap();
        self.debug_indent.pop();
        self.solver.pop(1);


        if self.debug_indent.len() < 1 && self.branches.len() > 0 {
            panic!("ICE: last pop without unbranch");
        }

        self.debug("recover declarations");
        // recover declarations done inside the scope. symbols are global, but z3 doesn't have a way to specify that
        for (sym,name, t) in self.symbol_stack.pop().unwrap() {
            self.declare(sym,&name,t);
            self.ded_syms.insert(sym,name);
        }
        self.debug("end of recover declarations");
    }


    pub fn check_ded(&self, sym: Symbol, here: &crate::ast::Location) {
        if let Some(name) = self.ded_syms.get(&sym) {
            emit_warn(format!("ICE: reuse of dead symbol '{}'", name), &[
                (here.clone(), format!("reuse in this scope will lead to strange behaviour"))
            ]);
        }
    }


    pub fn solve(&self) -> bool {
        match self.solver.check() {
            SatResult::Unknown => {
                warn!("sat solver took too long. you can increase timeout with --smt-timeout=10000");
                false
            }
            SatResult::Unsat => {
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
        if &self.debug_loc != loc {
            self.debug_loc = loc.clone();

            let code = loc.span.as_str().replace("\n", &format!("\n{}; ", self.debug_indent));
            write!(self.debug.borrow_mut(), "{}; : {}:{}\n", self.debug_indent, loc.file, loc.line()).unwrap();
            write!(self.debug.borrow_mut(), "{}; {}\n", self.debug_indent, code).unwrap();
        }
    }

    pub fn new(module_name: String) -> Self {
        //Config::set_global_param_value(":model.partial", "true");
        //Config::set_global_param_value(":parallel.enable", "true");
        let mut config  = Config::new();
        //config.set_model_generation(true);
        config.set_timeout_msec(TIMEOUT.load(Ordering::Relaxed) as u64);
        let ctx     = Box::leak(Box::new(Context::new(&config)));
        let solver  = z3::Solver::new_for_logic(ctx, "QF_UFBV".to_string());


        std::fs::create_dir_all("./target/ssa/").unwrap();
        let outfile = format!("./target/ssa/{}.smt2", module_name);
        let debug = RefCell::new(File::create(&outfile).expect(&format!("cannot create {}", outfile)));

        Self {
            config,
            ctx,
            solver,
            vars: RefCell::new(HashMap::new()),
            theories: HashMap::new(),
            debug_loc: super::ast::Location::builtin(),
            debug,
            debug_indent: String::new(),
            branches: Vec::new(),
            symbol_stack:   vec![Vec::new()],
            ded_syms:       HashMap::new(),
        }
    }

}
