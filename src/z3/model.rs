use crate::z3::ast::Ast;
use std::ffi::CStr;
use std::fmt;
use z3_sys::*;
use crate::z3::Model;
use crate::z3::Optimize;
use crate::z3::Solver;
use crate::z3::Z3_MUTEX;

impl<'ctx> Model<'ctx> {
    pub fn of_solver(slv: &Solver<'ctx>) -> Model<'ctx> {
        Model {
            ctx: slv.ctx,
            z3_mdl: unsafe {
                let guard = Z3_MUTEX.lock().unwrap();
                let m = Z3_solver_get_model(slv.ctx.z3_ctx, slv.z3_slv);
                Z3_model_inc_ref(slv.ctx.z3_ctx, m);
                m
            },
        }
    }

    pub fn of_optimize(opt: &Optimize<'ctx>) -> Model<'ctx> {
        Model {
            ctx: opt.ctx,
            z3_mdl: unsafe {
                let guard = Z3_MUTEX.lock().unwrap();
                let m = Z3_optimize_get_model(opt.ctx.z3_ctx, opt.z3_opt);
                Z3_model_inc_ref(opt.ctx.z3_ctx, m);
                m
            },
        }
    }

    pub fn eval<T>(&self, ast: &T, model_completion: bool) -> Option<T>
    where
        T: Ast<'ctx>,
    {
        let mut tmp: Z3_ast = ast.get_z3_ast();
        let res = {
            let guard = Z3_MUTEX.lock().unwrap();
            unsafe {
                Z3_model_eval(
                    self.ctx.z3_ctx,
                    self.z3_mdl,
                    ast.get_z3_ast(),
                    model_completion,
                    &mut tmp,
                )
            }
        };
        if res {
            Some(T::new(self.ctx, tmp))
        } else {
            None
        }
    }
}

impl<'ctx> fmt::Display for Model<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let p = unsafe { Z3_model_to_string(self.ctx.z3_ctx, self.z3_mdl) };
        if p.is_null() {
            return Result::Err(fmt::Error);
        }
        match unsafe { CStr::from_ptr(p) }.to_str() {
            Ok(s) => write!(f, "{}", s),
            Err(_) => Result::Err(fmt::Error),
        }
    }
}

impl<'ctx> Drop for Model<'ctx> {
    fn drop(&mut self) {
        let guard = Z3_MUTEX.lock().unwrap();
        unsafe { Z3_model_dec_ref(self.ctx.z3_ctx, self.z3_mdl) };
    }
}
