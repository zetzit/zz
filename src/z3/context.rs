use z3_sys::*;
use crate::z3::Config;
use crate::z3::Context;
use crate::z3::Z3_MUTEX;

impl Context {
    pub fn new(cfg: &Config) -> Context {
        Context {
            z3_ctx: unsafe {
                let guard = Z3_MUTEX.lock().unwrap();
                let p = Z3_mk_context_rc(cfg.z3_cfg);
                debug!("new context {:p}", p);
                p
            },
        }
    }

    /// Interrupt a solver performing a satisfiability test, a tactic processing a goal, or simplify functions.
    ///
    /// This method can be invoked from a thread different from the one executing the
    /// interruptible procedure.
    pub fn interrupt(&self) {
        unsafe {
            Z3_interrupt(self.z3_ctx);
        }
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe { Z3_del_context(self.z3_ctx) };
    }
}
