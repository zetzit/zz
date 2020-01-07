use crate::symbolic::{Symbol, TemporalSymbol};
use std::collections::HashMap;
use std::collections::HashSet;
use std::io::Write;
use std::cell::{RefCell};
use std::sync::atomic::{AtomicUsize};
use super::parser::{emit_warn};
use rsmt2_zz as rsmt2;
pub static TIMEOUT: AtomicUsize = AtomicUsize::new(5000);

pub enum Assertion<T> {
    Constrained(T),
    Unconstrained(T),
    Unsolveable,
}


pub struct Var {
    smtname:    String,
    temp:       HashSet<u64>,
    typ:        Type
}

pub struct Solver {
    solver:         RefCell<rsmt2::Solver<Rsmt2Junk>>,
    vars:           RefCell<HashMap<Symbol, Var>>,
    theories:       HashMap<Symbol, String>,
    debug_loc:      crate::ast::Location,

    branches:       Vec<Vec<String>>,

    symbol_stack:   RefCell<Vec<Vec<(TemporalSymbol, String, Type)>>>,
    ded_syms:       HashMap<Symbol, String>,

    assert_counter: usize,
}



#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Bool,
    Signed(u32),
    Unsigned(u32),
}

pub struct ModelRef(());

impl Solver {

    fn var(&self, sym: &TemporalSymbol) -> String {
        let mut fuckyourust = self.vars.borrow_mut();
        let var = fuckyourust.get_mut(&sym.0).unwrap();
        let name = format!("{}__t{}", var.smtname, sym.1);
        if !var.temp.contains(&sym.1) {
            write!(self.solver.borrow_mut(), "(declare-fun {} () {})\n",
                name,
                match var.typ {
                    Type::Bool => format!("Bool"),
                    Type::Signed(s) | Type::Unsigned(s) => format!("(_ BitVec {})", s),
                },
            ).unwrap();

            var.temp.insert(sym.1);

            self.symbol_stack.borrow_mut().last_mut().as_mut().unwrap().push((sym.clone(), name.to_string(), var.typ.clone()));
        }
        name
    }

    fn var_as(&self, sym: &TemporalSymbol, t2: Type ) -> String {
        let mut fuckyourust = self.vars.borrow_mut();
        let var = fuckyourust.get_mut(&sym.0).unwrap();
        let name = format!("{}__t{}", var.smtname, sym.1);
        if !var.temp.contains(&sym.1) {
            write!(self.solver.borrow_mut(), "(declare-fun {} () {})\n",
                name,
                match var.typ {
                    Type::Bool => format!("Bool"),
                    Type::Signed(s) | Type::Unsigned(s) => format!("(_ BitVec {})", s),
                },
            ).unwrap();

            var.temp.insert(sym.1);
        }

        match (&var.typ, t2) {
            (Type::Bool, Type::Bool) => {
                name
            }
            (Type::Bool, Type::Signed(size)) | (Type::Bool, Type::Unsigned(size)) => {
                format!("(ite {} (_ bv1 {}) (_ bv0 {}))", name, size, size)
            }
            (Type::Signed(size), Type::Bool) |  (Type::Unsigned(size), Type::Bool) => {
                format!("(bvuge {} (_ bv1 {}))", name, size)
            }
            (Type::Signed(rhs_size), Type::Signed(lhs_size)) |
            (Type::Signed(rhs_size), Type::Unsigned(lhs_size)) |
            (Type::Unsigned(rhs_size), Type::Unsigned(lhs_size)) |
            (Type::Unsigned(rhs_size), Type::Signed(lhs_size)) => {
                    if lhs_size < *rhs_size {
                        format!("( (_ extract {} {}) {} )", lhs_size - 1, 0, name)
                    } else if lhs_size > *rhs_size {
                        format!("( (_ zero_extend {}) {} )", lhs_size - rhs_size, name)
                    } else {
                        name
                    }
                }
        }
    }


    pub fn branch(&mut self) {
        self.branches.push(Vec::new());
    }

    pub fn unbranch(&mut self, returned: bool) {
        if returned {
            let branch_smt = self.build_branch_bundle();
            if branch_smt != "true" {
                let branch_smt = format!("(not {})", branch_smt);

                write!(self.solver.borrow_mut(), "; branch returned. the rest of the function only happens \
                   if the condition leading to return never happened\n").unwrap();
                write!(self.solver.borrow_mut(), "; {}\n", branch_smt).unwrap();

                self.branches.pop();

                // ideally we'd just inject all the previous conditions as negative branch conditions, but this bloats up the model. 
                //
                //   self.branches.first_mut().unwrap().push((branch_capi, branch_debug));
                //
                //
                // but we can simply assert it as negative, because symbolic execution is linear anyway. it wont ask about the previous stuff

                self.solver.borrow_mut().assert(&branch_smt).unwrap();
                return;
            }
        }
        self.branches.pop();
    }

    pub fn theory(&mut self, sym: Symbol, args: Vec<Type>, name: &str, t: Type) {
        let lname = format!("theory{}_{}", sym, name.replace(|c: char| !c.is_ascii_alphanumeric(), "_"));

        let mut debug_args = Vec::new();
        for t in args {
            match t {
                Type::Bool => {
                    debug_args.push("Bool".to_string());
                }
                Type::Signed(size) | Type::Unsigned(size) =>  {
                    debug_args.push(format!("(_ BitVec {})", size));
                }
            }
        }
        let debug_args = debug_args.join(" ");

        match t {
            Type::Signed(size) | Type::Unsigned(size) => {
                write!(self.solver.borrow_mut(), "(declare-fun {} ({}) (_ BitVec {})); theory {}\n", lname, debug_args, size, name).unwrap();
            }
            Type::Bool => {
                write!(self.solver.borrow_mut(), "(declare-fun {} ({}) Bool); theory {}\n", lname, debug_args, name).unwrap();
            }
        };
        self.theories.insert(sym, lname);
        self.checkpoint();
    }

    pub fn invocation(&mut self, theory: Symbol, args: Vec<TemporalSymbol>, tmp: TemporalSymbol) {
        let mut debug_args = Vec::new();
        for arg in &args {
            let arg = self.var(&arg);
            debug_args.push(arg);
        }

        let debug_args = debug_args.join(" ");
        let tmp_debug  = self.var(&tmp);

        let theory = &self.theories[&theory];
        self.solver.borrow_mut().assert(&format!("(= {} ({} {}) )", tmp_debug, theory, debug_args)).unwrap();
        self.checkpoint();
    }


    pub fn declare(&mut self, sym: Symbol, name: &str, typ: Type) {
        let smtname = format!("var{}_{}", sym, name.replace(|c: char| !c.is_ascii_alphanumeric(), "_"));

        self.vars.borrow_mut().insert(sym, Var{
            smtname,
            typ: typ.clone(),
            temp: HashSet::new(),
        });

        self.checkpoint();
    }


    //TODO could be faster by preparing the joins
    fn build_branch_bundle(&self) -> String {
        let mut smt = Vec::new();
        for branch in &self.branches {
            for branch_smt in branch {
                smt.push(branch_smt.clone());
            }
        }

        if smt.len() == 0 {
            return "true".to_string();
        }

        if smt.len() == 1 {
            return smt[0].clone();
        }


        format!("( and {} )", smt.join(" "))
    }


    /// an assign that happens depending on branch conditions
    pub fn assign_branch(&mut self, lhs: TemporalSymbol, rhs_if : TemporalSymbol, rhs_else : TemporalSymbol, t: Type) {
        let branch_smt = self.build_branch_bundle();

        let smt_lhs  = self.var_as(&lhs, t.clone());
        let smt_if   = self.var_as(&rhs_if, t.clone());
        let smt_else = self.var_as(&rhs_else, t.clone());

        self.solver.borrow_mut().assert(&format!("(= {}  (ite {} {} {})  )",
            smt_lhs,
            branch_smt,
            smt_if,
            smt_else,
        )).unwrap();

        self.checkpoint();
    }

    pub fn assign(&mut self, lhs: TemporalSymbol, rhs: TemporalSymbol, t: Type) {
        let smt_lhs = self.var_as(&lhs, t.clone());
        let smt_rhs = self.var_as(&rhs, t.clone());

        write!(self.solver.borrow_mut(), "(assert (! (= {} {}) :named A{}))",
            smt_lhs,
            smt_rhs,
            self.assert_counter,
        ).unwrap();
        self.assert_counter += 1;

        self.checkpoint();
    }

    pub fn literal(&mut self, tmp: Symbol, val: u64, typ: Type) {

        let smt_lhs = self.var(&(tmp,0));

        match typ {
            Type::Unsigned(size) | Type::Signed(size) => {
                self.solver.borrow_mut().assert(&format!("(= {} (_ bv{} {}))\n",
                    smt_lhs,
                    val,
                    size
                )).unwrap();
            }
            Type::Bool => {
                if val > 0 {
                    self.solver.borrow_mut().assert(&smt_lhs).unwrap();
                } else {
                    self.solver.borrow_mut().assert(&format!("(not {})", smt_lhs)).unwrap();
                }
            }
        }
        self.checkpoint();
    }

    pub fn infix_op_will_wrap(
        &self,
        _lhs:    TemporalSymbol,
        _rhs:    TemporalSymbol,
        _op:     crate::ast::InfixOperator,
        _t:      Type,
    ) -> bool {
        return false;

        // Int isn't supported anymore
        /*

        let smt_lhs = self.var_as(&lhs, t);
        let smt_rhs = self.var_as(&rhs, t);

        match op {
            crate::ast::InfixOperator::Add       => {
                self.solver.borrow_mut().push();
                self.solver.borrow_mut().assert(&capi_lhs.to_int(false).add(&[&capi_rhs.to_int(false)])
                     ._eq(&capi_lhs.bvadd(&capi_rhs).to_int(false)));
                let r = self.solve();
                self.solver.borrow_mut().pop(1);
                !r
            }
            crate::ast::InfixOperator::Subtract  => {
                self.solver.borrow_mut().push();
                self.solver.borrow_mut().assert(&capi_lhs.to_int(false).sub(&[&capi_rhs.to_int(false)])
                    ._eq(&capi_lhs.bvsub(&capi_rhs).to_int(false)));
                let r = self.solve();
                self.solver.borrow_mut().pop(1);
                !r
            }
            crate::ast::InfixOperator::Multiply  => {
                self.solver.borrow_mut().push();
                self.solver.borrow_mut().assert(&capi_lhs.to_int(false).mul(&[&capi_rhs.to_int(false)])
                    ._eq(&capi_lhs.bvmul(&capi_rhs).to_int(false)));
                let r = self.solve();
                self.solver.borrow_mut().pop(1);
                !r
            }
            _ => false,

        }
        */
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
        let smt_tmp  = self.var_as(&(tmp,0), t.clone());
        let smt_lhs  = self.var(&lhs);
        let smt_rhs  = self.var(&rhs);

        match op {
            crate::ast::InfixOperator::Equals    => {
                assert!(t == Type::Bool);
                self.solver.borrow_mut().assert(&format!("(=  {} (= {} {}))", smt_tmp, smt_lhs, smt_rhs)).unwrap();
            },
            crate::ast::InfixOperator::Nequals   => {
                assert!(t == Type::Bool);
                self.solver.borrow_mut().assert(&format!("(=  {} (not (= {} {})))", smt_tmp, smt_lhs, smt_rhs)).unwrap();
            }
            crate::ast::InfixOperator::Add       => {
                assert!(t != Type::Bool);
                self.solver.borrow_mut().assert(&format!(" (=  {} (bvadd {} {}))", smt_tmp, smt_lhs, smt_rhs)).unwrap();
            }
            crate::ast::InfixOperator::Subtract  => {
                assert!(t != Type::Bool);
                self.solver.borrow_mut().assert(&format!("(=  {} (bvsub {} {}))", smt_tmp, smt_lhs, smt_rhs)).unwrap();
            }
            crate::ast::InfixOperator::Multiply  => {
                assert!(t != Type::Bool);
                self.solver.borrow_mut().assert(&format!("(=  {} (bvmul {} {}))", smt_tmp, smt_lhs, smt_rhs)).unwrap();
            }
            crate::ast::InfixOperator::Divide    => {
                assert!(t != Type::Bool);

                if signed {
                    self.solver.borrow_mut().assert(&format!(" (=  {} (bvsdiv {} {}))", smt_tmp, smt_lhs, smt_rhs)).unwrap();
                } else {
                    self.solver.borrow_mut().assert(&format!("(=  {} (bvudiv {} {}))", smt_tmp, smt_lhs, smt_rhs)).unwrap();
                }
            }
            crate::ast::InfixOperator::Bitxor => {
                assert!(t != Type::Bool);
                self.solver.borrow_mut().assert(&format!("(=  {} (bvxnor {} {}))", smt_tmp, smt_lhs, smt_rhs)).unwrap();
            }
            crate::ast::InfixOperator::Booland => {
                assert!(t == Type::Bool);
                self.solver.borrow_mut().assert(&format!("(=  {} (and {} {}))", smt_tmp, smt_lhs, smt_rhs)).unwrap();
            }
            crate::ast::InfixOperator::Boolor => {
                assert!(t == Type::Bool);
                self.solver.borrow_mut().assert(&format!("(=  {} (or {} {}))", smt_tmp, smt_lhs, smt_rhs)).unwrap();
            }
            crate::ast::InfixOperator::Moreeq => {
                assert!(t == Type::Bool);
                if signed {
                    self.solver.borrow_mut().assert(&format!("(=  {} (bvsge {} {}))", smt_tmp, smt_lhs, smt_rhs)).unwrap();
                } else {
                    self.solver.borrow_mut().assert(&format!("(=  {} (bvuge {} {}))", smt_tmp, smt_lhs, smt_rhs)).unwrap();
                }
            }
            crate::ast::InfixOperator::Lesseq => {
                assert!(t == Type::Bool);
                if signed {
                    self.solver.borrow_mut().assert(&format!("(=  {} (bvsle {} {}))", smt_tmp, smt_lhs, smt_rhs)).unwrap();
                } else {
                    self.solver.borrow_mut().assert(&format!("(=  {} (bvule {} {}))", smt_tmp, smt_lhs, smt_rhs)).unwrap();
                }
            }
            crate::ast::InfixOperator::Lessthan => {
                assert!(t == Type::Bool);
                if signed {
                    self.solver.borrow_mut().assert(&format!("(= {} (bvslt {} {}))", smt_tmp, smt_lhs, smt_rhs)).unwrap();
                } else {
                    self.solver.borrow_mut().assert(&format!("(=  {} (bvult {} {}))", smt_tmp, smt_lhs, smt_rhs)).unwrap();
                }
            }
            crate::ast::InfixOperator::Morethan => {
                assert!(t == Type::Bool);
                if signed {
                    self.solver.borrow_mut().assert(&format!("(=  {} (bvsgt {} {}))", smt_tmp, smt_lhs, smt_rhs)).unwrap();
                } else {
                    self.solver.borrow_mut().assert(&format!("(=  {} (bvugt {} {}))", smt_tmp, smt_lhs, smt_rhs)).unwrap();
                }
            }
            crate::ast::InfixOperator::Shiftleft => {
                assert!(t != Type::Bool);
                self.solver.borrow_mut().assert(&format!("(=  {} (bvshl {} {}))", smt_tmp, smt_lhs, smt_rhs)).unwrap();
            }
            crate::ast::InfixOperator::Shiftright => {
                assert!(t != Type::Bool);
                self.solver.borrow_mut().assert(&format!("(=  {} (bvlshr {} {}))", smt_tmp, smt_lhs, smt_rhs)).unwrap();
            }
            crate::ast::InfixOperator::Modulo => {
                assert!(t != Type::Bool);
                self.solver.borrow_mut().assert(&format!("(=  {} (bvsmod {} {}))", smt_tmp, smt_lhs, smt_rhs)).unwrap();
            }
            crate::ast::InfixOperator::Bitand => {
                assert!(t != Type::Bool);
                self.solver.borrow_mut().assert(&format!("(=  {} (bvand {} {}))", smt_tmp, smt_lhs, smt_rhs)).unwrap();
            }
            crate::ast::InfixOperator::Bitor => {
                assert!(t != Type::Bool);
                self.solver.borrow_mut().assert(&format!("(=  {} (bvor {} {}))", smt_tmp, smt_lhs, smt_rhs)).unwrap();
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

        let smt_to    = self.var_as(&to, t.clone());
        let smt_from  = self.var_as(&from, t.clone());

        let smt_op = match op {
            crate::ast::PostfixOperator::Increment  => format!("(bvadd {} (_ bv1 {}))", smt_from, size),
            crate::ast::PostfixOperator::Decrement  => format!("(bvsub {} (_ bv1 {}))", smt_from, size),
        };
        self.solver.borrow_mut().assert(&format!("(= {} {} )", smt_to, smt_op)).unwrap();

        self.checkpoint();
    }

    pub fn prefix_op(&mut self,
        to:  TemporalSymbol,
        from:TemporalSymbol,
        op:  crate::ast::PrefixOperator,
        t:   Type,
    ) {

        let smt_to    = self.var_as(&to, t.clone());
        let smt_from  = self.var_as(&from, t.clone());

        match  t {
            Type::Signed(size) | Type::Unsigned(size) => {
                let smt_op = match op {
                    crate::ast::PrefixOperator::Boolnot    => format!("(not (= {} ))", smt_from),
                    crate::ast::PrefixOperator::Bitnot     => format!("(bvxnor {} #x{} )", smt_from, "ff".repeat(size as usize/8)),
                    crate::ast::PrefixOperator::Increment  => format!("(bvadd {} (_ bv1 {}))", smt_from, size),
                    crate::ast::PrefixOperator::Decrement  => format!("(bvsub {} (_ bv1 {}))", smt_from, size),
                    _ => unreachable!(),
                };
                self.solver.borrow_mut().assert(&format!("(= {} {} )", smt_to, smt_op)).unwrap();
            }
            Type::Bool => {
                assert!(op == crate::ast::PrefixOperator::Boolnot);
                self.solver.borrow_mut().assert(&format!("(= {} (not {} ))", smt_to, smt_from)).unwrap();
            }
        }
        self.checkpoint();
    }

    pub fn constrain_branch(&mut self, lhs: TemporalSymbol, positive: bool) {
        let smt_lhs  = self.var(&lhs);

        if positive {
            self.branches.last_mut().unwrap().push(smt_lhs);
        } else {
            self.branches.last_mut().unwrap().push(format!("(not {})", smt_lhs));
        };
    }

    pub fn attest(&mut self, lhs: TemporalSymbol, compare: bool) -> bool {
        let mut smt = self.var(&lhs);

        if !compare {
            smt = format!("(not {})", smt);
        }

        write!(self.solver.borrow_mut(), "(assert (! {} :named A{}))",
            smt,
            self.assert_counter,
        ).unwrap();
        self.assert_counter += 1;

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
    pub fn extract(&self, _model: &ModelRef, lhs: TemporalSymbol) -> Option<u64> {
        let smt_lhs  = self.var(&lhs);
        let value = self.solver.borrow_mut().get_values(&[smt_lhs]).unwrap().get(0).unwrap().1.clone();

        debug!("extracted: {}", value);
        if value == "false" {
            return Some(0);
        } else if value == "true" {
            return Some(1);
        } else  if value.starts_with("#x") {
            if let Ok(v) = u64::from_str_radix(&value[2..], 16) {
                return Some(v)
            }
        } else if value.starts_with("#b") {
            if let Ok(v) = u64::from_str_radix(&value[2..], 2) {
                return Some(v)
            }
        }
        None
    }

    // asserts are false if
    //  - we reached it due to branch flow
    //  - the opposite of the assert condition is solveable
    // assert(a) ->   !solveable(assert(branch && !a));
    pub fn assert<R, F> (&self, lhs: Vec<(Symbol, u64)>, with: F) -> R
        where F : Fn(bool, Option<ModelRef>) -> R,
              R : Sized,
    {
        assert!(lhs.len() > 0);

        let branch_smt = self.build_branch_bundle();

        let mut asserts_debug = Vec::new();
        for lhs in lhs {
            let smt_lhs  = self.var(&lhs);
            asserts_debug.push(format!("(not {} )", smt_lhs));
        }

        self.solver.borrow_mut().push(1).unwrap();
        self.solver.borrow_mut().assert(&format!("(and {} (or {} ))\n",
            branch_smt,
            asserts_debug.join(" ")
        )).unwrap();
        let rs = self.solve();

        let r = match rs {
            true => {
                write!(self.solver.borrow_mut(), "; sat / failed\n").unwrap();
                with(false, Some(ModelRef(())))
            }
            false => {
                write!(self.solver.borrow_mut(), "; unsat / pass\n").unwrap();
                with(true, None)
            }
        };
        self.solver.borrow_mut().pop(1).unwrap();
        r
    }

    pub fn value<R, F> (&self, lhs: TemporalSymbol, with: F) -> R
        where F : Fn(Assertion<u64>, Option<ModelRef>) -> R,
              R : Sized
    {
        let smt_lhs  = self.var(&lhs);

        if !self.solve() {
            warn!("model broke earlier");
            return with(Assertion::Unsolveable, None);
        }
        let value = self.solver.borrow_mut().get_values(&[smt_lhs.clone()]).unwrap().get(0).unwrap().1.clone();
        write!(self.solver.borrow_mut(), ";  = {:?}\n", value).unwrap();
        let val = if value.starts_with("#x") {
            if let Ok(v) = u64::from_str_radix(&value[2..], 16) {
                v
            } else {
                return with(Assertion::Unsolveable, None);
            }
        } else if value.starts_with("#b") {
            if let Ok(v) = u64::from_str_radix(&value[2..], 2) {
                v
            } else {
                return with(Assertion::Unsolveable, None);
            }
        } else {
            return with(Assertion::Unsolveable, None);
        };

        self.solver.borrow_mut().push(1).unwrap();
        self.solver.borrow_mut().assert(&format!("(not (= {} {}))", smt_lhs, value)).unwrap();
        let rr = match self.solve() {
            false => {
                self.solver.borrow_mut().pop(1).unwrap();
                self.solver.borrow_mut().push(1).unwrap();
                if self.solve() {
                    with(Assertion::Constrained(val), Some(ModelRef(())))
                } else {
                    with(Assertion::Constrained(val), None)
                }
            }
            true => {
                with(Assertion::Unconstrained(val), Some(ModelRef(())))
            }
        };
        self.solver.borrow_mut().pop(1).unwrap();
        rr
    }

    pub fn bool_value<R, F> (&self, lhs: (Symbol, u64), with: F) -> R
        where F : Fn(Assertion<bool>, Option<ModelRef>) -> R,
              R : Sized
    {
        let smt_lhs  = self.var(&lhs);

        if !self.solve() {
            warn!("model broke earlier");
            return with(Assertion::Unsolveable, None);
        }
        let value = self.solver.borrow_mut().get_values(&[smt_lhs.clone()]).unwrap().get(0).unwrap().1.clone();

        write!(self.solver.borrow_mut(), ";  = {:?}\n", value).unwrap();

        let val = if value.contains("true") {
            true
        } else if value.contains("false") {
            false
        } else {
            return with(Assertion::Unsolveable, None);
        };

        self.solver.borrow_mut().push(1).unwrap();
        self.solver.borrow_mut().assert(&format!("(not (= {} {}))", smt_lhs, value)).unwrap();
        let rr = match self.solve() {
            false => {
                self.solver.borrow_mut().pop(1).unwrap();
                self.solver.borrow_mut().push(1).unwrap();
                if self.solve() {
                    with(Assertion::Constrained(val), Some(ModelRef(())))
                } else {
                    with(Assertion::Constrained(val), None)
                }
            }
            true => {
                with(Assertion::Unconstrained(val), Some(ModelRef(())))
            }
        };
        self.solver.borrow_mut().pop(1).unwrap();
        rr
    }


    pub fn push(&mut self, reason: &str) {
        write!(self.solver.borrow_mut(), ";{}\n", reason).unwrap();
        self.solver.borrow_mut().push(1).unwrap();
        self.symbol_stack.borrow_mut().push(Vec::new());
    }
    pub fn pop(&mut self, reason: &str) {
        write!(self.solver.borrow_mut(), ";{}\n", reason).unwrap();
        self.solver.borrow_mut().pop(1).unwrap();

        // recover declarations done inside the scope. symbols are global, but z3 doesn't have a way to specify that
        let lfuckyou = self.symbol_stack.borrow_mut().pop();
        // unless we're done
        if self.symbol_stack.borrow().len() == 0 {
            return;
        }

        for (sym,name, t) in lfuckyou.unwrap() {
            if !self.vars.borrow().contains_key(&sym.0) {
                self.declare(sym.0, &name,t);
            }
            self.vars.borrow_mut().get_mut(&sym.0).unwrap().temp.remove(&sym.1);
            self.var(&sym);
        }
    }


    pub fn check_ded(&self, sym: Symbol, here: &crate::ast::Location) {
        if let Some(name) = self.ded_syms.get(&sym) {
            emit_warn(format!("ICE: reuse of dead symbol '{}'", name), &[
                (here.clone(), format!("reuse in this scope will lead to strange behaviour"))
            ]);
        }
    }


    pub fn solve(&self) -> bool {
        self.solver.borrow_mut().check_sat().unwrap()
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
        write!(self.solver.borrow_mut(), "; {}\n", m).unwrap();
    }
    pub fn debug_loc(&mut self, loc: &crate::ast::Location) {
        if &self.debug_loc != loc {
            self.debug_loc = loc.clone();

            let code = loc.span.as_str().replace("\n", "\n; ");
            write!(self.solver.borrow_mut(), "; : {}:{}\n", loc.file, loc.line()).unwrap();
            write!(self.solver.borrow_mut(), "; {}\n", code).unwrap();
        }
    }

    pub fn new(module_name: String) -> Self {

        //Config::set_global_param_value(":model.partial", "true");
        //Config::set_global_param_value(":parallel.enable", "true");
        //config.set_model_generation(true);



        let conf = if which::which("yices_smt2_mt").is_ok() {
            rsmt2::SmtConf::yices_2()
        } else if which::which("z3").is_ok() {
            rsmt2::SmtConf::z3()
        } else {
            panic!("z3 or yices_smt2_mt required in PATH")
        };


        std::fs::create_dir_all("./target/ssa/").unwrap();
        let outfile = format!("./target/ssa/{}.smt2", module_name);


        let mut solver = rsmt2::Solver::new(conf, Rsmt2Junk).unwrap();
        solver.path_tee(outfile).unwrap();

        write!(solver,"(set-option :produce-unsat-cores true)\n").unwrap();
        write!(solver,"(set-logic QF_UFBV)\n").unwrap();
        //write!(solver,"(set-option :parallel.enable true)\n").unwrap();
        //write!(solver,"(set-option :timeout {})\n", TIMEOUT.load(Ordering::Relaxed) as u64).unwrap();


        Self {
            solver:         RefCell::new(solver),
            vars:           RefCell::new(HashMap::new()),
            theories:       HashMap::new(),
            debug_loc:      super::ast::Location::builtin(),
            branches:       Vec::new(),
            symbol_stack:   RefCell::new(vec![Vec::new()]),
            ded_syms:       HashMap::new(),
            assert_counter: 0,
        }
    }

}





use rsmt2::{SmtRes, parse::ValueParser, parse::ExprParser};


#[derive(Clone, Copy)]
pub struct Rsmt2Junk;
impl<'a> ValueParser<String, &'a str> for Rsmt2Junk {
    fn parse_value(self, input: &'a str) -> SmtRes<String> {
        Ok(input.into())
    }
}
impl<'a> ExprParser<String, (), &'a str> for Rsmt2Junk {
    fn parse_expr(self, input: &'a str, _: ()) -> SmtRes<String> {
        Ok(input.into())
    }
}

