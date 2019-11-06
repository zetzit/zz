use std::ffi::CString;
use z3_sys::*;
use crate::z3::Context;
use crate::z3::Symbol;

impl Symbol {
    pub fn as_z3_symbol(&self, ctx: &Context) -> Z3_symbol {
        match self {
            Symbol::Int(i) => unsafe { Z3_mk_int_symbol(ctx.z3_ctx, *i as ::std::os::raw::c_int) },
            Symbol::String(s) => {
                let ss = CString::new(s.clone()).unwrap();
                let p = ss.as_ptr();
                unsafe { Z3_mk_string_symbol(ctx.z3_ctx, p) }
            }
        }
    }
}

impl From<u32> for Symbol {
    fn from(val: u32) -> Self {
        Symbol::Int(val)
    }
}

impl From<String> for Symbol {
    fn from(val: String) -> Self {
        Symbol::String(val)
    }
}

impl From<&str> for Symbol {
    fn from(val: &str) -> Self {
        Symbol::String(val.to_owned())
    }
}
