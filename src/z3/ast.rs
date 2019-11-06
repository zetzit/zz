use std::cmp::{Eq, PartialEq};
use std::convert::{TryFrom, TryInto};
use std::ffi::{CStr, CString};
use std::fmt;
use std::hash::{Hash, Hasher};
use z3_sys::*;
use crate::z3::Context;
use crate::z3::Pattern;
use crate::z3::Sort;
use crate::z3::Symbol;
use crate::z3::Z3_MUTEX;

#[cfg(feature = "arbitrary-size-numeral")]
use num::bigint::BigInt;
#[cfg(feature = "arbitrary-size-numeral")]
use num::rational::BigRational;

/// [`Ast`](trait.Ast.html) node representing a boolean value.
pub struct Bool<'ctx> {
    pub(crate) ctx: &'ctx Context,
    pub(crate) z3_ast: Z3_ast,
}

/// [`Ast`](trait.Ast.html) node representing an integer value.
pub struct Int<'ctx> {
    pub(crate) ctx: &'ctx Context,
    pub(crate) z3_ast: Z3_ast,
}

/// [`Ast`](trait.Ast.html) node representing a real value.
pub struct Real<'ctx> {
    pub(crate) ctx: &'ctx Context,
    pub(crate) z3_ast: Z3_ast,
}

/// [`Ast`](trait.Ast.html) node representing a bitvector value.
pub struct BV<'ctx> {
    pub(crate) ctx: &'ctx Context,
    pub(crate) z3_ast: Z3_ast,
}

/// [`Ast`](trait.Ast.html) node representing an array value.
/// An array in Z3 is a mapping from indices to values.
pub struct Array<'ctx> {
    pub(crate) ctx: &'ctx Context,
    pub(crate) z3_ast: Z3_ast,
}

/// [`Ast`](trait.Ast.html) node representing a set value.
pub struct Set<'ctx> {
    pub(crate) ctx: &'ctx Context,
    pub(crate) z3_ast: Z3_ast,
}

/// [`Ast`](trait.Ast.html) node representing a datatype or enumeration value.
pub struct Datatype<'ctx> {
    pub(crate) ctx: &'ctx Context,
    pub(crate) z3_ast: Z3_ast,
}

/// A dynamically typed [`Ast`](trait.Ast.html) node.
pub struct Dynamic<'ctx> {
    pub(crate) ctx: &'ctx Context,
    pub(crate) z3_ast: Z3_ast,
}

macro_rules! unop {
    ( $f:ident, $z3fn:ident, $retty:ty ) => {
        pub fn $f(&self) -> $retty {
            <$retty>::new(self.ctx, unsafe {
                let guard = Z3_MUTEX.lock().unwrap();
                $z3fn(self.ctx.z3_ctx, self.z3_ast)
            })
        }
    };
}

macro_rules! binop {
    ( $f:ident, $z3fn:ident, $retty:ty ) => {
        pub fn $f(&self, other: &Self) -> $retty {
            <$retty>::new(self.ctx, unsafe {
                let guard = Z3_MUTEX.lock().unwrap();
                $z3fn(self.ctx.z3_ctx, self.z3_ast, other.z3_ast)
            })
        }
    };
}

/* We aren't currently using the trinop! macro for any of our trinops
macro_rules! trinop {
    ( $f:ident, $z3fn:ident, $retty:ty ) => {
        pub fn $f(&self, a: &Self, b: &Self) -> $retty {
            <$retty>::new(self.ctx, unsafe {
                let guard = Z3_MUTEX.lock().unwrap();
                $z3fn(self.ctx.z3_ctx, self.z3_ast, a.z3_ast, b.z3_ast)
            })
        }
    };
}
*/

macro_rules! varop {
    ( $f:ident, $z3fn:ident, $retty:ty ) => {
        pub fn $f(&self, other: &[&Self]) -> $retty {
            <$retty>::new(self.ctx, unsafe {
                let guard = Z3_MUTEX.lock().unwrap();
                let mut tmp = vec![self.z3_ast];
                for a in other {
                    tmp.push(a.z3_ast)
                }
                assert!(tmp.len() <= 0xffff_ffff);
                $z3fn(self.ctx.z3_ctx, tmp.len() as u32, tmp.as_ptr())
            })
        }
    };
}

/// Abstract syntax tree (AST) nodes represent terms, constants, or expressions.
/// The `Ast` trait contains methods common to all AST subtypes.
pub trait Ast<'ctx>: Sized + fmt::Debug {
    fn get_ctx(&self) -> &'ctx Context;
    fn get_z3_ast(&self) -> Z3_ast;

    // This would be great, but gives error E0071 "expected struct, variant or union type, found Self"
    // so I don't think we can write a generic constructor like this.
    // Instead we just require the method, and use the new_ast! macro defined below to implement it
    // on each Ast subtype.
    /*
    fn new(ctx: &'ctx Context, ast: Z3_ast) -> Self {
        assert!(!ast.is_null());
        Self {
            ctx,
            z3_ast: unsafe {
                debug!("new ast {:p}", ast);
                let guard = Z3_MUTEX.lock().unwrap();
                Z3_inc_ref(ctx.z3_ctx, ast);
                ast
            },
        }
    }
    */
    fn new(ctx: &'ctx Context, ast: Z3_ast) -> Self;

    /// Compare this `Ast` with another `Ast`, and get a [`Bool`](struct.Bool.html)
    /// representing the result.
    ///
    /// This operation works with all possible `Ast`s (int, real, BV, etc), but the two
    /// `Ast`s being compared must be the same type.
    //
    // Note that we can't use the binop! macro because of the `pub` keyword on it
    fn _eq(&self, other: &Self) -> Bool<'ctx> {
        Bool::new(self.get_ctx(), unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_eq(self.get_ctx().z3_ctx, self.get_z3_ast(), other.get_z3_ast())
        })
    }

    /// Compare this `Ast` with a list of other `Ast`s, and get a [`Bool`](struct.Bool.html)
    /// which is true only if all arguments (including Self) are pairwise distinct.
    ///
    /// This operation works with all possible `Ast`s (int, real, BV, etc), but the
    /// `Ast`s being compared must all be the same type.
    //
    // Note that we can't use the varop! macro because of the `pub` keyword on it
    fn distinct(&self, other: &[&Self]) -> Bool<'ctx> {
        Bool::new(self.get_ctx(), unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            let mut tmp = vec![self.get_z3_ast()];
            for a in other {
                tmp.push(a.get_z3_ast())
            }
            assert!(tmp.len() <= 0xffff_ffff);
            Z3_mk_distinct(self.get_ctx().z3_ctx, tmp.len() as u32, tmp.as_ptr())
        })
    }

    /// Get the [`Sort`](../struct.Sort.html) of the `Ast`
    fn get_sort(&self) -> Sort<'ctx> {
        Sort::new(self.get_ctx(), unsafe {
            Z3_get_sort(self.get_ctx().z3_ctx, self.get_z3_ast())
        })
    }

    /// Simplify the `Ast`. Returns a new `Ast` which is equivalent,
    /// but simplified using algebraic simplification rules, such as
    /// constant propagation.
    fn simplify(&self) -> Self {
        Self::new(self.get_ctx(), unsafe {
            Z3_simplify(self.get_ctx().z3_ctx, self.get_z3_ast())
        })
    }

    /// Performs substitution on the `Ast`. The slice `substitutions` contains a
    /// list of pairs with a "from" `Ast` that will be substituted by a "to" `Ast`.
    fn substitute<T: Ast<'ctx>>(&self, substitutions: &[(&T, &T)]) -> Self {
        Self::new(self.get_ctx(), unsafe {
            let guard = Z3_MUTEX.lock().unwrap();

            let this_ast = self.get_z3_ast();
            let num_exprs = substitutions.len() as ::std::os::raw::c_uint;
            let mut froms: Vec<_> = vec![];
            let mut tos: Vec<_> = vec![];

            for (from_ast, to_ast) in substitutions {
                froms.push(from_ast.get_z3_ast());
                tos.push(to_ast.get_z3_ast());
            }

            Z3_substitute(
                self.get_ctx().z3_ctx,
                this_ast,
                num_exprs,
                froms.as_ptr(),
                tos.as_ptr(),
            )
        })
    }
}

macro_rules! impl_ast {
    ($ast:ident) => {
        impl<'ctx> Ast<'ctx> for $ast<'ctx> {
            fn new(ctx: &'ctx Context, ast: Z3_ast) -> Self {
                assert!(!ast.is_null());
                Self {
                    ctx,
                    z3_ast: unsafe {
                        debug!("new ast {:p}", ast);
                        let guard = Z3_MUTEX.lock().unwrap();
                        Z3_inc_ref(ctx.z3_ctx, ast);
                        ast
                    },
                }
            }

            fn get_ctx(&self) -> &'ctx Context {
                self.ctx
            }

            fn get_z3_ast(&self) -> Z3_ast {
                self.z3_ast
            }
        }

        impl<'ctx> From<$ast<'ctx>> for Z3_ast {
            fn from(ast: $ast<'ctx>) -> Self {
                ast.z3_ast
            }
        }

        impl<'ctx> PartialEq for $ast<'ctx> {
            fn eq(&self, other: &$ast<'ctx>) -> bool {
                assert_eq!(self.ctx, other.ctx);
                unsafe { Z3_is_eq_ast(self.ctx.z3_ctx, self.z3_ast, other.z3_ast) }
            }
        }

        impl<'ctx> Eq for $ast<'ctx> {}

        impl<'ctx> Clone for $ast<'ctx> {
            fn clone(&self) -> Self {
                debug!("clone ast {:p}", self.z3_ast);
                Self::new(self.ctx, self.z3_ast)
            }
        }

        impl<'ctx> Drop for $ast<'ctx> {
            fn drop(&mut self) {
                debug!("drop ast {:p}", self.z3_ast);
                unsafe {
                    Z3_dec_ref(self.ctx.z3_ctx, self.z3_ast);
                }
            }
        }

        impl<'ctx> Hash for $ast<'ctx> {
            fn hash<H: Hasher>(&self, state: &mut H) {
                unsafe {
                    let u = Z3_get_ast_hash(self.ctx.z3_ctx, self.z3_ast);
                    u.hash(state);
                }
            }
        }

        impl<'ctx> fmt::Debug for $ast<'ctx> {
            fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
                let p = unsafe { Z3_ast_to_string(self.ctx.z3_ctx, self.z3_ast) };
                if p.is_null() {
                    return Result::Err(fmt::Error);
                }
                match unsafe { CStr::from_ptr(p) }.to_str() {
                    Ok(s) => write!(f, "{}", s),
                    Err(_) => Result::Err(fmt::Error),
                }
            }
        }

        impl<'ctx> fmt::Display for $ast<'ctx> {
            fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
                <Self as fmt::Debug>::fmt(self, f)
            }
        }
    };
}

macro_rules! impl_from_try_into_dynamic {
    ($ast:ident, $as_ast:ident) => {
        impl<'ctx> From<$ast<'ctx>> for Dynamic<'ctx> {
            fn from(ast: $ast<'ctx>) -> Self {
                Dynamic::new(ast.ctx, ast.z3_ast)
            }
        }

        impl<'ctx> TryFrom<Dynamic<'ctx>> for $ast<'ctx> {
            type Error = String;
            fn try_from(ast: Dynamic<'ctx>) -> Result<Self, String> {
                ast.$as_ast()
                    .ok_or_else(|| format!("Dynamic is not of requested type: {:?}", ast))
            }
        }
    };
}

impl_ast!(Bool);
impl_from_try_into_dynamic!(Bool, as_bool);
impl_ast!(Int);
impl_from_try_into_dynamic!(Int, as_int);
impl_ast!(Real);
impl_from_try_into_dynamic!(Real, as_real);
impl_ast!(BV);
impl_from_try_into_dynamic!(BV, as_bv);
impl_ast!(Array);
impl_from_try_into_dynamic!(Array, as_array);
impl_ast!(Set);
// Dynamic::as_set does not exist, so just implement one direction here
impl<'ctx> From<Set<'ctx>> for Dynamic<'ctx> {
    fn from(ast: Set<'ctx>) -> Self {
        Dynamic::new(ast.ctx, ast.z3_ast)
    }
}

impl<'ctx> Int<'ctx> {
    #[cfg(feature = "arbitrary-size-numeral")]
    pub fn from_big_int(ctx: &'ctx Context, value: &BigInt) -> Int<'ctx> {
        Int::from_str(ctx, &value.to_str_radix(10)).unwrap()
    }

    pub fn from_str(ctx: &'ctx Context, value: &str) -> Option<Int<'ctx>> {
        let sort = Sort::int(ctx);
        let ast = unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            let int_cstring = CString::new(value).unwrap();
            let numeral_ptr = Z3_mk_numeral(ctx.z3_ctx, int_cstring.as_ptr(), sort.z3_sort);
            if numeral_ptr.is_null() {
                return None;
            }

            numeral_ptr
        };
        Some(Int::new(ctx, ast))
    }
}

impl<'ctx> Real<'ctx> {
    #[cfg(feature = "arbitrary-size-numeral")]
    pub fn from_big_rational(ctx: &'ctx Context, value: &BigRational) -> Real<'ctx> {
        let num = value.numer();
        let den = value.denom();
        Real::from_real_str(ctx, &num.to_str_radix(10), &den.to_str_radix(10)).unwrap()
    }

    pub fn from_real_str(ctx: &'ctx Context, num: &str, den: &str) -> Option<Real<'ctx>> {
        let sort = Sort::real(ctx);
        let ast = unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            let fraction_cstring = CString::new(format!("{:} / {:}", num, den)).unwrap();
            let numeral_ptr = Z3_mk_numeral(ctx.z3_ctx, fraction_cstring.as_ptr(), sort.z3_sort);
            if numeral_ptr.is_null() {
                return None;
            }

            numeral_ptr
        };
        Some(Real::new(ctx, ast))
    }
}

impl_ast!(Datatype);
impl_from_try_into_dynamic!(Datatype, as_datatype);

impl_ast!(Dynamic);

impl<'ctx> Bool<'ctx> {
    pub fn new_const<S: Into<Symbol>>(ctx: &'ctx Context, name: S) -> Bool<'ctx> {
        let sort = Sort::bool(ctx);
        Self::new(ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_const(ctx.z3_ctx, name.into().as_z3_symbol(ctx), sort.z3_sort)
        })
    }

    pub fn fresh_const(ctx: &'ctx Context, prefix: &str) -> Bool<'ctx> {
        let sort = Sort::bool(ctx);
        Self::new(ctx, unsafe {
            let pp = CString::new(prefix).unwrap();
            let p = pp.as_ptr();
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_fresh_const(ctx.z3_ctx, p, sort.z3_sort)
        })
    }

    pub fn from_bool(ctx: &'ctx Context, b: bool) -> Bool<'ctx> {
        Self::new(ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            if b {
                Z3_mk_true(ctx.z3_ctx)
            } else {
                Z3_mk_false(ctx.z3_ctx)
            }
        })
    }

    pub fn as_bool(&self) -> Option<bool> {
        unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            match Z3_get_bool_value(self.ctx.z3_ctx, self.z3_ast) {
                Z3_L_TRUE => Some(true),
                Z3_L_FALSE => Some(false),
                _ => None,
            }
        }
    }

    // TODO: this should be on the Ast trait, but I don't know how to return Self<'dest_ctx>.
    // When I try, it gives the error E0109 "lifetime arguments are not allowed for this type".
    pub fn translate<'dest_ctx>(&self, dest: &'dest_ctx Context) -> Bool<'dest_ctx> {
        Bool::new(dest, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_translate(self.ctx.z3_ctx, self.z3_ast, dest.z3_ctx)
        })
    }

    // This doesn't quite fit the trinop! macro because of the generic argty
    pub fn ite<T>(&self, a: &T, b: &T) -> T
    where
        T: Ast<'ctx>,
    {
        T::new(self.ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_ite(self.ctx.z3_ctx, self.z3_ast, a.get_z3_ast(), b.get_z3_ast())
        })
    }

    varop!(and, Z3_mk_and, Self);
    varop!(or, Z3_mk_or, Self);
    binop!(xor, Z3_mk_xor, Self);
    unop!(not, Z3_mk_not, Self);
    binop!(iff, Z3_mk_iff, Self);
    binop!(implies, Z3_mk_implies, Self);

    pub fn pb_le(&self, other: &[&Bool<'ctx>], coeffs: Vec<i32>, k: i32) -> Bool<'ctx> {
        Bool::new(self.ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            let mut tmp = vec![self.z3_ast];
            for a in other {
                tmp.push(a.z3_ast)
            }
            assert!(tmp.len() <= 0xffffffff);
            let mut tmp_coeffs = coeffs.clone();
            Z3_mk_pble(
                self.ctx.z3_ctx,
                tmp.len() as u32,
                tmp.as_ptr(),
                tmp_coeffs.as_mut_ptr(),
                k,
            )
        })
    }
    pub fn pb_ge(&self, other: &[&Bool<'ctx>], coeffs: Vec<i32>, k: i32) -> Bool<'ctx> {
        Bool::new(self.ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            let mut tmp = vec![self.z3_ast];
            for a in other {
                tmp.push(a.z3_ast)
            }
            assert!(tmp.len() <= 0xffffffff);
            let mut tmp_coeffs = coeffs.clone();
            Z3_mk_pbge(
                self.ctx.z3_ctx,
                tmp.len() as u32,
                tmp.as_ptr(),
                tmp_coeffs.as_mut_ptr(),
                k,
            )
        })
    }
    pub fn pb_eq(&self, other: &[&Bool<'ctx>], coeffs: Vec<i32>, k: i32) -> Bool<'ctx> {
        Bool::new(self.ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            let mut tmp = vec![self.z3_ast];
            for a in other {
                tmp.push(a.z3_ast)
            }
            assert!(tmp.len() <= 0xffffffff);
            let mut tmp_coeffs = coeffs.clone();
            Z3_mk_pbeq(
                self.ctx.z3_ctx,
                tmp.len() as u32,
                tmp.as_ptr(),
                tmp_coeffs.as_mut_ptr(),
                k,
            )
        })
    }
}

impl<'ctx> Int<'ctx> {
    pub fn new_const<S: Into<Symbol>>(ctx: &'ctx Context, name: S) -> Int<'ctx> {
        let sort = Sort::int(ctx);
        Self::new(ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_const(ctx.z3_ctx, name.into().as_z3_symbol(ctx), sort.z3_sort)
        })
    }

    pub fn fresh_const(ctx: &'ctx Context, prefix: &str) -> Int<'ctx> {
        let sort = Sort::int(ctx);
        Self::new(ctx, unsafe {
            let pp = CString::new(prefix).unwrap();
            let p = pp.as_ptr();
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_fresh_const(ctx.z3_ctx, p, sort.z3_sort)
        })
    }

    pub fn from_i64(ctx: &'ctx Context, i: i64) -> Int<'ctx> {
        let sort = Sort::int(ctx);
        Self::new(ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_int64(ctx.z3_ctx, i, sort.z3_sort)
        })
    }

    pub fn from_u64(ctx: &'ctx Context, u: u64) -> Int<'ctx> {
        let sort = Sort::int(ctx);
        Self::new(ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_unsigned_int64(ctx.z3_ctx, u, sort.z3_sort)
        })
    }

    pub fn as_i64(&self) -> Option<i64> {
        unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            let mut tmp: ::std::os::raw::c_longlong = 0;
            if Z3_get_numeral_int64(self.ctx.z3_ctx, self.z3_ast, &mut tmp) {
                Some(tmp)
            } else {
                None
            }
        }
    }

    pub fn as_u64(&self) -> Option<u64> {
        unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            let mut tmp: ::std::os::raw::c_ulonglong = 0;
            if Z3_get_numeral_uint64(self.ctx.z3_ctx, self.z3_ast, &mut tmp) {
                Some(tmp)
            } else {
                None
            }
        }
    }

    pub fn from_real(ast: &Real<'ctx>) -> Int<'ctx> {
        Self::new(ast.ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_real2int(ast.ctx.z3_ctx, ast.z3_ast)
        })
    }

    /// Create a real from an integer.
    /// This is just a convenience wrapper around
    /// [`Real::from_int`](struct.Real.html#method.from_int); see notes there
    pub fn to_real(&self) -> Real<'ctx> {
        Real::from_int(self)
    }

    /// Create an integer from a bitvector.
    ///
    /// Signed and unsigned version.
    ///
    /// # Examples
    /// ```
    /// # use z3::{ast, Config, Context, SatResult, Solver};
    /// # use z3::ast::Ast;
    /// # let cfg = Config::new();
    /// # let ctx = Context::new(&cfg);
    /// # let solver = Solver::new(&ctx);
    /// let bv = ast::BV::new_const(&ctx, "x", 32);
    /// solver.assert(&bv._eq(&ast::BV::from_i64(&ctx, -3, 32)));
    ///
    /// let x = ast::Int::from_bv(&bv, true);
    ///
    /// assert_eq!(solver.check(), SatResult::Sat);
    /// let model = solver.get_model();
    ///
    /// assert_eq!(-3, model.eval(&x).unwrap().as_i64().unwrap());
    /// ```
    pub fn from_bv(ast: &BV<'ctx>, signed: bool) -> Int<'ctx> {
        Self::new(ast.ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_bv2int(ast.ctx.z3_ctx, ast.z3_ast, signed)
        })
    }

    /// Create a bitvector from an integer.
    /// This is just a convenience wrapper around
    /// [`BV::from_int`](struct.BV.html#method.from_int); see notes there
    pub fn to_ast(&self, sz: u32) -> BV<'ctx> {
        BV::from_int(self, sz)
    }

    // TODO: this should be on the Ast trait, but I don't know how to return Self<'dest_ctx>.
    // When I try, it gives the error E0109 "lifetime arguments are not allowed for this type".
    pub fn translate<'dest_ctx>(&self, dest: &'dest_ctx Context) -> Int<'dest_ctx> {
        Int::new(dest, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_translate(self.ctx.z3_ctx, self.z3_ast, dest.z3_ctx)
        })
    }

    varop!(add, Z3_mk_add, Self);
    varop!(sub, Z3_mk_sub, Self);
    varop!(mul, Z3_mk_mul, Self);
    binop!(div, Z3_mk_div, Self);
    binop!(rem, Z3_mk_rem, Self);
    binop!(modulo, Z3_mk_mod, Self);
    binop!(power, Z3_mk_power, Self);
    unop!(unary_minus, Z3_mk_unary_minus, Self);
    binop!(lt, Z3_mk_lt, Bool<'ctx>);
    binop!(le, Z3_mk_le, Bool<'ctx>);
    binop!(gt, Z3_mk_gt, Bool<'ctx>);
    binop!(ge, Z3_mk_ge, Bool<'ctx>);
    // Z3 does support mixing ints and reals in add(), sub(), mul(), div(), and power()
    //   (but not rem(), modulo(), lt(), le(), gt(), or ge()).
    // TODO: we could consider expressing this by having a Numeric trait with these methods.
    //    Int and Real would have the Numeric trait, but not the other Asts.
    // For example:
    //   fn add(&self, other: &impl Numeric<'ctx>) -> Dynamic<'ctx> { ... }
    // Note the return type would have to be Dynamic I think (?), as the exact result type
    //   depends on the particular types of the inputs.
    // Alternately, we could just have
    //   Int::add_real(&self, other: &Real<'ctx>) -> Real<'ctx>
    // and
    //   Real::add_int(&self, other: &Int<'ctx>) -> Real<'ctx>
    // This might be cleaner because we know exactly what the output type will be for these methods.
}

impl<'ctx> Real<'ctx> {
    pub fn new_const<S: Into<Symbol>>(ctx: &'ctx Context, name: S) -> Real<'ctx> {
        let sort = Sort::real(ctx);
        Self::new(ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_const(ctx.z3_ctx, name.into().as_z3_symbol(ctx), sort.z3_sort)
        })
    }

    pub fn fresh_const(ctx: &'ctx Context, prefix: &str) -> Real<'ctx> {
        let sort = Sort::real(ctx);
        Self::new(ctx, unsafe {
            let pp = CString::new(prefix).unwrap();
            let p = pp.as_ptr();
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_fresh_const(ctx.z3_ctx, p, sort.z3_sort)
        })
    }

    pub fn from_real(ctx: &'ctx Context, num: i32, den: i32) -> Real<'ctx> {
        Self::new(ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_real(
                ctx.z3_ctx,
                num as ::std::os::raw::c_int,
                den as ::std::os::raw::c_int,
            )
        })
    }

    pub fn as_real(&self) -> Option<(i64, i64)> {
        unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            let mut num: i64 = 0;
            let mut den: i64 = 0;
            if Z3_get_numeral_small(self.ctx.z3_ctx, self.z3_ast, &mut num, &mut den) {
                Some((num, den))
            } else {
                None
            }
        }
    }

    pub fn from_int(ast: &Int<'ctx>) -> Real<'ctx> {
        Self::new(ast.ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_int2real(ast.ctx.z3_ctx, ast.z3_ast)
        })
    }

    /// Create an integer from a real.
    /// This is just a convenience wrapper around
    /// [`Int::from_real`](struct.Int.html#method.from_real); see notes there
    pub fn to_int(&self) -> Int<'ctx> {
        Int::from_real(self)
    }

    unop!(is_int, Z3_mk_is_int, Bool<'ctx>);

    // TODO: this should be on the Ast trait, but I don't know how to return Self<'dest_ctx>.
    // When I try, it gives the error E0109 "lifetime arguments are not allowed for this type".
    pub fn translate<'dest_ctx>(&self, dest: &'dest_ctx Context) -> Real<'dest_ctx> {
        Real::new(dest, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_translate(self.ctx.z3_ctx, self.z3_ast, dest.z3_ctx)
        })
    }

    varop!(add, Z3_mk_add, Self);
    varop!(sub, Z3_mk_sub, Self);
    varop!(mul, Z3_mk_mul, Self);
    binop!(div, Z3_mk_div, Self);
    binop!(power, Z3_mk_power, Self);
    unop!(unary_minus, Z3_mk_unary_minus, Self);
    binop!(lt, Z3_mk_lt, Bool<'ctx>);
    binop!(le, Z3_mk_le, Bool<'ctx>);
    binop!(gt, Z3_mk_gt, Bool<'ctx>);
    binop!(ge, Z3_mk_ge, Bool<'ctx>);
}

macro_rules! bv_overflow_check_signed {
    ( $f:ident, $z3fn:ident) => {
        pub fn $f(&self, other: &BV<'ctx>, b: bool) -> Bool<'ctx> {
            Ast::new(self.ctx, unsafe {
                $z3fn(self.ctx.z3_ctx, self.z3_ast, other.z3_ast, b)
            })
    }
    };
}

impl<'ctx> BV<'ctx> {
    pub fn new_const<S: Into<Symbol>>(ctx: &'ctx Context, name: S, sz: u32) -> BV<'ctx> {
        let sort = Sort::bitvector(ctx, sz);
        Self::new(ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_const(ctx.z3_ctx, name.into().as_z3_symbol(ctx), sort.z3_sort)
        })
    }

    pub fn fresh_const(ctx: &'ctx Context, prefix: &str, sz: u32) -> BV<'ctx> {
        let sort = Sort::bitvector(ctx, sz);
        Self::new(ctx, unsafe {
            let pp = CString::new(prefix).unwrap();
            let p = pp.as_ptr();
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_fresh_const(ctx.z3_ctx, p, sort.z3_sort)
        })
    }

    pub fn from_i64(ctx: &'ctx Context, i: i64, sz: u32) -> BV<'ctx> {
        let sort = Sort::bitvector(ctx, sz);
        Self::new(ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_int64(ctx.z3_ctx, i, sort.z3_sort)
        })
    }

    pub fn from_u64(ctx: &'ctx Context, u: u64, sz: u32) -> BV<'ctx> {
        let sort = Sort::bitvector(ctx, sz);
        Self::new(ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_unsigned_int64(ctx.z3_ctx, u, sort.z3_sort)
        })
    }

    pub fn as_i64(&self) -> Option<i64> {
        unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            let mut tmp: ::std::os::raw::c_longlong = 0;
            if Z3_get_numeral_int64(self.ctx.z3_ctx, self.z3_ast, &mut tmp) {
                Some(tmp)
            } else {
                None
            }
        }
    }

    pub fn as_u64(&self) -> Option<u64> {
        unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            let mut tmp: ::std::os::raw::c_ulonglong = 0;
            if Z3_get_numeral_uint64(self.ctx.z3_ctx, self.z3_ast, &mut tmp) {
                Some(tmp)
            } else {
                None
            }
        }
    }

    /// Create a bit vector from an integer.
    ///
    /// The bit vector will have width `sz`.
    ///
    /// # Examples
    /// ```
    /// # use z3::{ast, Config, Context, SatResult, Solver};
    /// # use z3::ast::Ast;
    /// # let cfg = Config::new();
    /// # let ctx = Context::new(&cfg);
    /// # let solver = Solver::new(&ctx);
    /// let i = ast::Int::new_const(&ctx, "x");
    /// solver.assert(&i._eq(&ast::Int::from_i64(&ctx, -3)));
    ///
    /// let x = ast::BV::from_int(&i, 64);
    /// assert_eq!(64, x.get_size());
    ///
    /// assert_eq!(solver.check(), SatResult::Sat);
    /// let model = solver.get_model();
    ///
    /// assert_eq!(-3, model.eval(&x.to_int(true)).unwrap().as_i64().expect("as_i64() shouldn't fail"));
    /// ```
    pub fn from_int(ast: &Int<'ctx>, sz: u32) -> BV<'ctx> {
        Self::new(ast.ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_int2bv(ast.ctx.z3_ctx, sz, ast.z3_ast)
        })
    }

    /// Create an integer from a bitvector.
    /// This is just a convenience wrapper around
    /// [`Int::from_bv`](struct.Int.html#method.from_bv); see notes there
    pub fn to_int(&self, signed: bool) -> Int<'ctx> {
        Int::from_bv(self, signed)
    }

    /// Get the size of the bitvector (in bits)
    pub fn get_size(&self) -> u32 {
        let sort = self.get_sort();
        unsafe { Z3_get_bv_sort_size(self.ctx.z3_ctx, sort.z3_sort) }
    }

    // TODO: this should be on the Ast trait, but I don't know how to return Self<'dest_ctx>.
    // When I try, it gives the error E0109 "lifetime arguments are not allowed for this type".
    pub fn translate<'dest_ctx>(&self, dest: &'dest_ctx Context) -> BV<'dest_ctx> {
        BV::new(dest, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_translate(self.ctx.z3_ctx, self.z3_ast, dest.z3_ctx)
        })
    }

    // Bitwise ops
    /// Bitwise negation
    unop!(bvnot, Z3_mk_bvnot, Self);
    /// Two's complement negation
    unop!(bvneg, Z3_mk_bvneg, Self);
    /// Bitwise and
    binop!(bvand, Z3_mk_bvand, Self);
    /// Bitwise or
    binop!(bvor, Z3_mk_bvor, Self);
    /// Bitwise exclusive-or
    binop!(bvxor, Z3_mk_bvxor, Self);
    /// Bitwise nand
    binop!(bvnand, Z3_mk_bvnand, Self);
    /// Bitwise nor
    binop!(bvnor, Z3_mk_bvnor, Self);
    /// Bitwise xnor
    binop!(bvxnor, Z3_mk_bvxnor, Self);
    /// Conjunction of all the bits in the vector. Returns a BV with size (bitwidth) 1.
    unop!(bvredand, Z3_mk_bvredand, Self);
    /// Disjunction of all the bits in the vector. Returns a BV with size (bitwidth) 1.
    unop!(bvredor, Z3_mk_bvredor, Self);

    // Arithmetic ops
    /// Addition
    binop!(bvadd, Z3_mk_bvadd, Self);
    /// Subtraction
    binop!(bvsub, Z3_mk_bvsub, Self);
    /// Multiplication
    binop!(bvmul, Z3_mk_bvmul, Self);
    /// Unsigned division
    binop!(bvudiv, Z3_mk_bvudiv, Self);
    /// Signed division
    binop!(bvsdiv, Z3_mk_bvsdiv, Self);
    /// Unsigned remainder
    binop!(bvurem, Z3_mk_bvurem, Self);
    /// Signed remainder (sign follows dividend)
    binop!(bvsrem, Z3_mk_bvsrem, Self);
    /// Signed remainder (sign follows divisor)
    binop!(bvsmod, Z3_mk_bvsmod, Self);

    // Comparison ops
    /// Unsigned less than
    binop!(bvult, Z3_mk_bvult, Bool<'ctx>);
    /// Signed less than
    binop!(bvslt, Z3_mk_bvslt, Bool<'ctx>);
    /// Unsigned less than or equal
    binop!(bvule, Z3_mk_bvule, Bool<'ctx>);
    /// Signed less than or equal
    binop!(bvsle, Z3_mk_bvsle, Bool<'ctx>);
    /// Unsigned greater or equal
    binop!(bvuge, Z3_mk_bvuge, Bool<'ctx>);
    /// Signed greater or equal
    binop!(bvsge, Z3_mk_bvsge, Bool<'ctx>);
    /// Unsigned greater than
    binop!(bvugt, Z3_mk_bvugt, Bool<'ctx>);
    /// Signed greater than
    binop!(bvsgt, Z3_mk_bvsgt, Bool<'ctx>);

    // Shift ops
    /// Shift left
    binop!(bvshl, Z3_mk_bvshl, Self);
    /// Logical shift right (add zeroes in the high bits)
    binop!(bvlshr, Z3_mk_bvlshr, Self);
    /// Arithmetic shift right (sign-extend in the high bits)
    binop!(bvashr, Z3_mk_bvashr, Self);
    /// Rotate left
    binop!(bvrotl, Z3_mk_ext_rotate_left, Self);
    /// Rotate right
    binop!(bvrotr, Z3_mk_ext_rotate_left, Self);

    /// Concatenate two bitvectors
    binop!(concat, Z3_mk_concat, Self);

    // overflow checks
    /// Check if addition overflows
    bv_overflow_check_signed!(bvadd_no_overflow, Z3_mk_bvadd_no_overflow);
    /// Check if addition underflows
    binop!(bvadd_no_underflow, Z3_mk_bvadd_no_underflow, Bool<'ctx>);
    /// Check if subtraction overflows
    binop!(bvsub_no_overflow, Z3_mk_bvsub_no_overflow, Bool<'ctx>);
    /// Check if subtraction underflows
    bv_overflow_check_signed!(bvsub_no_underflow, Z3_mk_bvsub_no_underflow);
    /// Check if signed division overflows
    binop!(bvsdiv_no_overflow, Z3_mk_bvsdiv_no_overflow, Bool<'ctx>);
    /// Check if negation overflows
    unop!(bvneg_no_overflow, Z3_mk_bvneg_no_overflow, Bool<'ctx>);
    /// Check if multiplication overflows
    bv_overflow_check_signed!(bvmul_no_overflow, Z3_mk_bvmul_no_overflow);
    /// Check if multiplication underflows
    binop!(bvmul_no_underflow, Z3_mk_bvmul_no_underflow, Bool<'ctx>);

    /// Extract the bits `high` down to `low` from the bitvector.
    /// Returns a bitvector of size `n`, where `n = high - low + 1`.
    pub fn extract(&self, high: u32, low: u32) -> Self {
        Self::new(self.ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_extract(self.ctx.z3_ctx, high, low, self.z3_ast)
        })
    }

    /// Sign-extend the bitvector to size `m+i`, where `m` is the original size of the bitvector.
    /// That is, `i` bits will be added.
    pub fn sign_ext(&self, i: u32) -> Self {
        Self::new(self.ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_sign_ext(self.ctx.z3_ctx, i, self.z3_ast)
        })
    }

    /// Zero-extend the bitvector to size `m+i`, where `m` is the original size of the bitvector.
    /// That is, `i` bits will be added.
    pub fn zero_ext(&self, i: u32) -> Self {
        Self::new(self.ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_zero_ext(self.ctx.z3_ctx, i, self.z3_ast)
        })
    }
}

impl<'ctx> Array<'ctx> {
    /// Create an `Array` which maps from indices of the `domain` `Sort` to
    /// values of the `range` `Sort`.
    ///
    /// All values in the `Array` will be unconstrained.
    pub fn new_const<S: Into<Symbol>>(
        ctx: &'ctx Context,
        name: S,
        domain: &Sort<'ctx>,
        range: &Sort<'ctx>,
    ) -> Array<'ctx> {
        let sort = Sort::array(ctx, domain, range);
        Self::new(ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_const(ctx.z3_ctx, name.into().as_z3_symbol(ctx), sort.z3_sort)
        })
    }

    pub fn fresh_const(
        ctx: &'ctx Context,
        prefix: &str,
        domain: &Sort<'ctx>,
        range: &Sort<'ctx>,
    ) -> Array<'ctx> {
        let sort = Sort::array(ctx, domain, range);
        Self::new(ctx, unsafe {
            let pp = CString::new(prefix).unwrap();
            let p = pp.as_ptr();
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_fresh_const(ctx.z3_ctx, p, sort.z3_sort)
        })
    }

    /// Create a "constant array", that is, an `Array` initialized so that all of the
    /// indices in the `domain` map to the given value `val`
    pub fn const_array(
        ctx: &'ctx Context,
        domain: &Sort<'ctx>,
        val: &Dynamic<'ctx>,
    ) -> Array<'ctx> {
        Self::new(ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_const_array(ctx.z3_ctx, domain.z3_sort, val.z3_ast)
        })
    }

    // TODO: this should be on the Ast trait, but I don't know how to return Self<'dest_ctx>.
    // When I try, it gives the error E0109 "lifetime arguments are not allowed for this type".
    pub fn translate<'dest_ctx>(&self, dest: &'dest_ctx Context) -> Array<'dest_ctx> {
        Array::new(dest, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_translate(self.ctx.z3_ctx, self.z3_ast, dest.z3_ctx)
        })
    }

    /// Get the value at a given index in the array.
    ///
    /// Note that the `index` _must be_ of the array's `domain` sort.
    /// The return type will be of the array's `range` sort.
    //
    // We avoid the binop! macro because the argument has a non-Self type
    pub fn select(&self, index: &Dynamic<'ctx>) -> Dynamic<'ctx> {
        // TODO: We could validate here that the index is of the correct type.
        // This would require us either to keep around the original `domain` argument
        // from when the Array was constructed, or to do an additional Z3 query
        // to find the domain sort first.
        // But if we did this check ourselves, we'd just panic, so it doesn't seem
        // like a huge advantage over just letting Z3 panic itself when it discovers the
        // problem.
        // This way we also avoid the redundant check every time this method is called.
        Dynamic::new(self.ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_select(self.ctx.z3_ctx, self.z3_ast, index.get_z3_ast())
        })
    }

    /// Update the value at a given index in the array.
    ///
    /// Note that the `index` _must be_ of the array's `domain` sort,
    /// and the `value` _must be_ of the array's `range` sort.
    //
    // We avoid the trinop! macro because the arguments have non-Self types
    pub fn store(&self, index: &Dynamic<'ctx>, value: &Dynamic<'ctx>) -> Self {
        Self::new(self.ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_store(
                self.ctx.z3_ctx,
                self.z3_ast,
                index.get_z3_ast(),
                value.get_z3_ast(),
            )
        })
    }
}

impl<'ctx> Set<'ctx> {
    pub fn new_const<S: Into<Symbol>>(
        ctx: &'ctx Context,
        name: S,
        eltype: &Sort<'ctx>,
    ) -> Set<'ctx> {
        let sort = Sort::set(ctx, eltype);
        Self::new(ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_const(ctx.z3_ctx, name.into().as_z3_symbol(ctx), sort.z3_sort)
        })
    }

    pub fn fresh_const(ctx: &'ctx Context, prefix: &str, eltype: &Sort<'ctx>) -> Set<'ctx> {
        let sort = Sort::set(ctx, eltype);
        Self::new(ctx, unsafe {
            let pp = CString::new(prefix).unwrap();
            let p = pp.as_ptr();
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_fresh_const(ctx.z3_ctx, p, sort.z3_sort)
        })
    }

    // TODO: this should be on the Ast trait, but I don't know how to return Self<'dest_ctx>.
    // When I try, it gives the error E0109 "lifetime arguments are not allowed for this type".
    pub fn translate<'dest_ctx>(&self, dest: &'dest_ctx Context) -> Set<'dest_ctx> {
        Set::new(dest, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_translate(self.ctx.z3_ctx, self.z3_ast, dest.z3_ctx)
        })
    }

    /// Add an element to the set.
    ///
    /// Note that the `element` _must be_ of the `Set`'s `eltype` sort.
    //
    // We avoid the binop! macro because the argument has a non-Self type
    pub fn add(&self, element: &Dynamic<'ctx>) -> Set<'ctx> {
        Set::new(self.ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_set_add(self.ctx.z3_ctx, self.z3_ast, element.get_z3_ast())
        })
    }

    /// Remove an element from the set.
    ///
    /// Note that the `element` _must be_ of the `Set`'s `eltype` sort.
    //
    // We avoid the binop! macro because the argument has a non-Self type
    pub fn del(&self, element: &Dynamic<'ctx>) -> Set<'ctx> {
        Set::new(self.ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_set_add(self.ctx.z3_ctx, self.z3_ast, element.get_z3_ast())
        })
    }

    /// Check if an item is a member of the set.
    ///
    /// Note that the `element` _must be_ of the `Set`'s `eltype` sort.
    //
    // We avoid the binop! macro because the argument has a non-Self type
    pub fn member(&self, element: &Dynamic<'ctx>) -> Bool<'ctx> {
        Bool::new(self.ctx, unsafe {
            let guard = Z3_MUTEX.lock().unwrap();
            Z3_mk_set_add(self.ctx.z3_ctx, self.z3_ast, element.get_z3_ast())
        })
    }

    /// Take the intersection of a list of sets.
    varop!(intersect, Z3_mk_set_intersect, Self);
    /// Take the union of a list of sets.
    varop!(set_union, Z3_mk_set_union, Self);
    /// Check if the set is a subset of another set.
    binop!(set_subset, Z3_mk_set_subset, Bool<'ctx>);
    /// Take the complement of the set.
    unop!(complement, Z3_mk_set_complement, Self);
    /// Take the set difference between two sets.
    binop!(difference, Z3_mk_set_difference, Self);
}

impl<'ctx> Dynamic<'ctx> {
    pub fn from_ast(ast: &impl Ast<'ctx>) -> Self {
        Self::new(ast.get_ctx(), ast.get_z3_ast())
    }

    fn sort_kind(&self) -> SortKind {
        unsafe { Z3_get_sort_kind(self.ctx.z3_ctx, Z3_get_sort(self.ctx.z3_ctx, self.z3_ast)) }
    }

    /// Returns `None` if the `Dynamic` is not actually a `Bool`
    pub fn as_bool(&self) -> Option<Bool<'ctx>> {
        match self.sort_kind() {
            SortKind::Bool => Some(Bool::new(self.ctx, self.z3_ast)),
            _ => None,
        }
    }

    /// Returns `None` if the `Dynamic` is not actually an `Int`
    pub fn as_int(&self) -> Option<Int<'ctx>> {
        match self.sort_kind() {
            SortKind::Int => Some(Int::new(self.ctx, self.z3_ast)),
            _ => None,
        }
    }

    /// Returns `None` if the `Dynamic` is not actually a `Real`
    pub fn as_real(&self) -> Option<Real<'ctx>> {
        match self.sort_kind() {
            SortKind::Real => Some(Real::new(self.ctx, self.z3_ast)),
            _ => None,
        }
    }

    /// Returns `None` if the `Dynamic` is not actually a `BV`
    pub fn as_bv(&self) -> Option<BV<'ctx>> {
        match self.sort_kind() {
            SortKind::BV => Some(BV::new(self.ctx, self.z3_ast)),
            _ => None,
        }
    }

    /// Returns `None` if the `Dynamic` is not actually an `Array`
    pub fn as_array(&self) -> Option<Array<'ctx>> {
        match self.sort_kind() {
            SortKind::Array => Some(Array::new(self.ctx, self.z3_ast)),
            _ => None,
        }
    }

    pub fn as_datatype(&self) -> Option<Datatype<'ctx>> {
        match self.sort_kind() {
            SortKind::Datatype => Some(Datatype::new(self.ctx, self.z3_ast)),
            _ => None,
        }
    }

    // TODO as_set. SortKind::Set does not exist
}

impl<'ctx> Datatype<'ctx> {
    pub fn new_const<S: Into<Symbol>>(ctx: &'ctx Context, name: S, sort: &Sort<'ctx>) -> Self {
        assert_eq!(ctx, sort.ctx);
        assert_eq!(sort.kind(), SortKind::Datatype);

        Self::new(ctx, unsafe {
            Z3_mk_const(ctx.z3_ctx, name.into().as_z3_symbol(ctx), sort.z3_sort)
        })
    }

    pub fn fresh_const(ctx: &'ctx Context, prefix: &str, sort: &Sort<'ctx>) -> Self {
        assert_eq!(ctx, sort.ctx);
        assert_eq!(sort.kind(), SortKind::Datatype);

        Self::new(ctx, unsafe {
            let pp = CString::new(prefix).unwrap();
            let p = pp.as_ptr();
            Z3_mk_fresh_const(ctx.z3_ctx, p, sort.z3_sort)
        })
    }

    // TODO: this should be on the Ast trait, but I don't know how to return Self<'dest_ctx>.
    // When I try, it gives the error E0109 "lifetime arguments are not allowed for this type".
    pub fn translate<'dest_ctx>(&self, dest: &'dest_ctx Context) -> Datatype<'dest_ctx> {
        Datatype::new(dest, unsafe {
            Z3_translate(self.ctx.z3_ctx, self.z3_ast, dest.z3_ctx)
        })
    }
}

/// Create a universal quantifier.
///
/// # Examples
/// ```
/// # use z3::{ast, Config, Context, FuncDecl, Pattern, SatResult, Solver, Sort, Symbol};
/// # use z3::ast::Ast;
/// # use std::convert::TryInto;
/// # let cfg = Config::new();
/// # let ctx = Context::new(&cfg);
/// # let solver = Solver::new(&ctx);
/// let f = FuncDecl::new(&ctx, "f", &[&Sort::int(&ctx)], &Sort::int(&ctx));
///
/// let x = ast::Int::new_const(&ctx, "x");
/// let f_x: ast::Int = f.apply(&[&x.clone().into()]).try_into().unwrap();
/// let f_x_pattern: Pattern = Pattern::new(&ctx, &[ &f_x.clone().into() ]);
/// let forall: ast::Bool = ast::forall_const(
///     &ctx,
///     &[&x.clone().into()],
///     &[&f_x_pattern],
///     &x._eq(&f_x).into()
/// ).try_into().unwrap();
/// solver.assert(&forall);
///
/// assert_eq!(solver.check(), SatResult::Sat);
/// let model = solver.get_model();
///
/// let f_f_3: ast::Int = f.apply(&[&f.apply(&[&ast::Int::from_u64(&ctx, 3).into()])]).try_into().unwrap();
/// assert_eq!(3, model.eval(&f_f_3).unwrap().as_u64().unwrap());
/// ```
pub fn forall_const<'ctx>(
    ctx: &'ctx Context,
    bounds: &[&Dynamic<'ctx>],
    patterns: &[&Pattern<'ctx>],
    body: &Dynamic<'ctx>,
) -> Dynamic<'ctx> {
    assert!(bounds.iter().all(|a| a.get_ctx() == ctx));
    assert!(patterns.iter().all(|p| p.ctx == ctx));
    assert_eq!(ctx, body.get_ctx());

    if bounds.is_empty() {
        return body.clone();
    }

    let bounds: Vec<_> = bounds.iter().map(|a| a.get_z3_ast()).collect();
    let patterns: Vec<_> = patterns.iter().map(|p| p.z3_pattern).collect();

    Ast::new(ctx, unsafe {
        Z3_mk_forall_const(
            ctx.z3_ctx,
            0,
            bounds.len().try_into().unwrap(),
            bounds.as_ptr() as *const Z3_app,
            patterns.len().try_into().unwrap(),
            patterns.as_ptr() as *const Z3_pattern,
            body.get_z3_ast(),
        )
    })
}

/// Create an existential quantifier.
///
/// # Examples
/// ```
/// # use z3::{ast, Config, Context, FuncDecl, SatResult, Solver, Sort, Symbol, Pattern};
/// # use z3::ast::Ast;
/// # use std::convert::TryInto;
/// # let cfg = Config::new();
/// # let ctx = Context::new(&cfg);
/// # let solver = Solver::new(&ctx);
/// let f = FuncDecl::new(&ctx, "f", &[&Sort::int(&ctx)], &Sort::int(&ctx));
///
/// let x = ast::Int::new_const(&ctx, "x");
/// let f_x: ast::Int = f.apply(&[&x.clone().into()]).try_into().unwrap();
/// let f_x_pattern: Pattern = Pattern::new(&ctx, &[ &f_x.clone().into() ]);
/// let exists: ast::Bool = ast::exists_const(
///     &ctx,
///     &[&x.clone().into()],
///     &[&f_x_pattern],
///     &x._eq(&f_x).not().into()
/// ).try_into().unwrap();
/// solver.assert(&exists.not());
///
/// assert_eq!(solver.check(), SatResult::Sat);
/// let model = solver.get_model();
///
/// let f_f_3: ast::Int = f.apply(&[&f.apply(&[&ast::Int::from_u64(&ctx, 3).into()])]).try_into().unwrap();
/// assert_eq!(3, model.eval(&f_f_3).unwrap().as_u64().unwrap());
/// ```
pub fn exists_const<'ctx>(
    ctx: &'ctx Context,
    bounds: &[&Dynamic<'ctx>],
    patterns: &[&Pattern<'ctx>],
    body: &Dynamic<'ctx>,
) -> Dynamic<'ctx> {
    assert!(bounds.iter().all(|a| a.get_ctx() == ctx));
    assert!(patterns.iter().all(|p| p.ctx == ctx));
    assert_eq!(ctx, body.get_ctx());

    let bounds: Vec<_> = bounds.iter().map(|a| a.get_z3_ast()).collect();
    let patterns: Vec<_> = patterns.iter().map(|p| p.z3_pattern).collect();

    Ast::new(ctx, unsafe {
        Z3_mk_exists_const(
            ctx.z3_ctx,
            0,
            bounds.len().try_into().unwrap(),
            bounds.as_ptr() as *const Z3_app,
            patterns.len().try_into().unwrap(),
            patterns.as_ptr() as *const Z3_pattern,
            body.get_z3_ast(),
        )
    })
}
