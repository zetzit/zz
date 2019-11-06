use std::{convert::TryInto, ptr::null_mut};
use z3_sys::*;
use crate::z3::{Context, DatatypeBuilder, DatatypeSort, DatatypeVariant, FuncDecl, Sort, Symbol};

impl<'ctx> DatatypeBuilder<'ctx> {
    pub fn new(ctx: &'ctx Context) -> Self {
        Self {
            ctx,
            variants: Vec::new(),
        }
    }

    pub fn variant(mut self, name: &str, fields: &[(&str, &Sort)]) -> Self {
        let recognizer_name_sym = Symbol::String(format!("is-{}", name)).as_z3_symbol(self.ctx);
        let name_sym = Symbol::String(name.to_string()).as_z3_symbol(self.ctx);

        assert!(fields
            .iter()
            .all(|(name, sort)| sort.ctx.z3_ctx == self.ctx.z3_ctx));

        let mut field_names: Vec<Z3_symbol> = Vec::with_capacity(fields.len());
        let mut field_sorts = Vec::with_capacity(fields.len());

        for (name, sort) in fields {
            field_names.push(Symbol::String(name.to_string()).as_z3_symbol(self.ctx));
            field_sorts.push(sort.z3_sort);
        }

        // This is unused.
        // Z3 expects sort_refs in Z3_mk_constructor to be valid, so create it here.
        let mut sort_refs = Vec::new();
        sort_refs.resize(fields.len(), 0);

        let constructor = unsafe {
            Z3_mk_constructor(
                self.ctx.z3_ctx,
                name_sym,
                recognizer_name_sym,
                fields.len().try_into().unwrap(),
                field_names.as_ptr(),
                field_sorts.as_ptr(),
                sort_refs.as_mut_ptr(),
            )
        };

        self.variants.push((fields.len(), constructor));
        self
    }

    pub fn finish(self, name: impl Into<Symbol>) -> DatatypeSort<'ctx> {
        let mut constructors: Vec<_> = self.variants.iter().map(|i| i.1).collect();
        let name_sym = name.into().as_z3_symbol(self.ctx);

        let sort = unsafe {
            let s = Z3_mk_datatype(
                self.ctx.z3_ctx,
                name_sym,
                constructors.len().try_into().unwrap(),
                constructors.as_mut_ptr(),
            );
            Z3_inc_ref(self.ctx.z3_ctx, Z3_sort_to_ast(self.ctx.z3_ctx, s));
            Sort {
                ctx: self.ctx,
                z3_sort: s,
            }
        };

        // create independent fields
        let (ctx, variants) = (self.ctx, self.variants);

        let variants = variants
            .into_iter()
            .map(|(num_fields, constructor)| {
                let mut constructor_func: Z3_func_decl = null_mut();
                let mut tester: Z3_func_decl = null_mut();
                let mut accessors: Vec<Z3_func_decl> = Vec::new();
                accessors.resize(num_fields, null_mut());

                unsafe {
                    // fill fields
                    Z3_query_constructor(
                        ctx.z3_ctx,
                        constructor,
                        num_fields.try_into().unwrap(),
                        &mut constructor_func,
                        &mut tester,
                        accessors.as_mut_ptr(),
                    );

                    // convert to Rust types
                    let constructor = FuncDecl::from_raw(ctx, constructor_func);
                    let tester = FuncDecl::from_raw(ctx, tester);
                    let accessors = accessors
                        .iter()
                        .map(|f| FuncDecl::from_raw(ctx, *f))
                        .collect();

                    DatatypeVariant {
                        constructor,
                        tester,
                        accessors,
                    }
                }
            })
            .collect();

        DatatypeSort {
            ctx: self.ctx,
            sort,
            variants,
        }
    }
}
