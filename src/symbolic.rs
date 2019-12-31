use crate::flatten;
use crate::ast;
use crate::name::Name;
use std::collections::HashMap;
use super::parser::{self, emit_error, emit_warn, emit_debug};
use ast::Tags;
use crate::smt::{Solver, self};
use super::Error;

pub type Symbol = usize;
pub type TemporalSymbol = (Symbol, u64);

#[derive(Clone, Debug)]
enum Value{
    Void,
    Uninitialized,
    PostfixOp {
        lhs:    TemporalSymbol,
        op:     ast::PostfixOperator,
    },
    PrefixOp {
        rhs:    TemporalSymbol,
        op:     ast::PrefixOperator,
    },
    InfixOp {
        lhs:    TemporalSymbol,
        rhs:    TemporalSymbol,
        op:     ast::InfixOperator,
    },
    Theory {
        args:   Vec<ast::NamedArg>,
        ret:    ast::Typed,
    },
    Function {
        loc:    ast::Location,
        args:   Vec<ast::NamedArg>,
        vararg: bool,
        ret:    Option<ast::Typed>,
        callsite_assert: Vec<ast::Expression>,
        callsite_effect: Vec<ast::Expression>,
    },
    SelfCall{
        selfarg: Box<ast::Expression>,
        name:    ast::Expression,
    },
    Address(Symbol),
    Struct {
        members:  HashMap<String, Symbol>,
    },
    Array {
        len:    usize,
        array:  HashMap<usize, Symbol>,
    },
    Unconstrained(String),
    Integer(u64),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Void             => write!(f, "void"),
            Value::Uninitialized    => write!(f, "uninitialized"),
            Value::Integer(s)       => write!(f, "integer ({})", s),
            Value::InfixOp{..}      => write!(f, "op"),
            Value::PrefixOp{..}     => write!(f, "op"),
            Value::PostfixOp{..}    => write!(f, "op"),
            Value::Struct{..}       => write!(f, "struct"),
            Value::Array{len,..}    => write!(f, "address to {} memory locations", len),
            Value::Address(_)       => write!(f, "address to memory location"),
            Value::Unconstrained(s) => write!(f, "unconstrained : {}", s),
            Value::Theory{..}       => write!(f, "theory"),
            Value::Function{..}     => write!(f, "function"),
            Value::SelfCall{..}     => write!(f, "self call"),
        }
    }
}


#[derive(Clone)]
struct Storage {
    name:           Name,
    typed:          ast::Typed,
    t:              crate::smt::Type,
    declared:       ast::Location,
    value:          Value,
    tags:           ast::Tags,
    temporal:       u64,
    assignments:    HashMap<u64, ast::Location>,

    //TODO not actually implemented
    borrows:        Vec<(Symbol, ast::Location)>,
}

#[derive(Clone)]
struct Scope{
    name:   String,
    locals: HashMap<Name, usize>,
    trace:  Vec<(TemporalSymbol, ast::Location, bool /*only demonstrace if true*/)>,
}

pub struct Symbolic {
    stack:      Vec<Scope>,
    memory:     Vec<Storage>,
    ssa:        Solver,
    builtin:    HashMap<String, Symbol>,
    defs:       HashMap<Name, ast::Def>,
    current_module_name:    String,
    current_function_name:  String,
    current_function_ret:   Option<Symbol>,
    current_function_model: Vec<ast::Expression>,
    in_loop:    bool,
    in_model:   bool,
}


pub enum ScopeReturn {
    NoReturn,
    Return(ast::Location),
}

impl Symbolic {
    fn execute_module(&mut self, module: &mut flatten::Module, fun: usize) -> Result<(), Error> {
        self.current_module_name = module.name.human_name();

        // built in len theory
        let sym = self.alloc(Name::from("len"), ast::Typed{
            t:      ast::Type::Other(Name::from("theory")),
            ptr:    Vec::new(),
            loc:    ast::Location::builtin(),
            tail:   ast::Tail::None,
        },
        ast::Location::builtin(), Tags::new()
        )?;
        self.memory[sym].value = Value::Theory{args: vec![ast::NamedArg{
            typed: ast::Typed{
                t:      ast::Type::Other("void".into()),
                ptr:    vec![ast::Pointer{
                    tags: ast::Tags::new(),
                    loc:  ast::Location::builtin(),
                }],
                loc:    ast::Location::builtin(),
                tail:   ast::Tail::None,
            },
            name:   "array".to_string(),
            tags:   ast::Tags::new(),
            loc:    ast::Location::builtin(),
        }], ret: ast::Typed{
            t:      ast::Type::USize,
            ptr:    Vec::new(),
            loc:    ast::Location::builtin(),
            tail:   ast::Tail::None,
        }};
        self.ssa.theory(sym, vec![smt::Type::Unsigned(64)], "len", smt::Type::Unsigned(64));
        self.builtin.insert("len".to_string(), sym);

        // built in safe theory
        let sym = self.alloc(Name::from("safe"), ast::Typed{
            t:      ast::Type::Other(Name::from("theory")),
            ptr:    Vec::new(),
            loc:    ast::Location::builtin(),
            tail:   ast::Tail::None,
        },
        ast::Location::builtin(), Tags::new()
        )?;
        self.memory[sym].value = Value::Theory{args: vec![ast::NamedArg{
            typed: ast::Typed{
                t:      ast::Type::Other("void".into()),
                ptr:    vec![ast::Pointer{
                    tags: ast::Tags::new(),
                    loc:  ast::Location::builtin(),
                }],
                loc:    ast::Location::builtin(),
                tail:   ast::Tail::None,
            },
            name:   "pointer".to_string(),
            tags:   ast::Tags::new(),
            loc:    ast::Location::builtin(),
        }], ret: ast::Typed{
            t:      ast::Type::Bool,
            ptr:    Vec::new(),
            loc:    ast::Location::builtin(),
            tail:   ast::Tail::None,
        }};
        self.ssa.theory(sym, vec![smt::Type::Unsigned(64)], "safe", smt::Type::Bool);
        self.builtin.insert("safe".to_string(), sym);

        // built in nullterm theory
        let sym = self.alloc(Name::from("nullterm"), ast::Typed{
            t:      ast::Type::Other(Name::from("theory")),
            ptr:    Vec::new(),
            loc:    ast::Location::builtin(),
            tail:   ast::Tail::None,
        },
        ast::Location::builtin(), Tags::new()
        )?;
        self.memory[sym].value = Value::Theory{args: vec![ast::NamedArg{
            typed: ast::Typed{
                t:      ast::Type::Other("::ext::<stddef.h>::char".into()),
                ptr:    vec![ast::Pointer{
                    tags: ast::Tags::new(),
                    loc:  ast::Location::builtin(),
                }],
                loc:    ast::Location::builtin(),
                tail:   ast::Tail::None,
            },
            name:   "cstr".to_string(),
            tags:   ast::Tags::new(),
            loc:    ast::Location::builtin(),
        }], ret: ast::Typed{
            t:      ast::Type::Bool,
            ptr:    Vec::new(),
            loc:    ast::Location::builtin(),
            tail:   ast::Tail::None,
        }};
        self.ssa.theory(sym, vec![smt::Type::Unsigned(64)], "nullterm", smt::Type::Bool);
        self.builtin.insert("nullterm".to_string(), sym);



        for (name,loc) in &module.c_names {
            let sym = self.alloc(
                name.clone(),
                ast::Typed{
                    t:      ast::Type::Other(name.clone()),
                    ptr:    Vec::new(),
                    loc:    loc.clone(),
                    tail:   ast::Tail::None,
                },
                loc.clone(), Tags::new()
            )?;
            self.memory[sym].value = Value::Unconstrained(format!("c name {}", name))
        }


        // declaration run
        for (d,_,defined_here) in &mut module.d {
            self.defs.insert(Name::from(&d.name), d.def.clone());
            match &mut d.def {
                ast::Def::Theory{args, ret, attr} => {
                    let sym = self.alloc(
                        Name::from(&d.name),
                        ast::Typed{
                            t:      ast::Type::Other(Name::from(&d.name.clone())),
                            ptr:    Vec::new(),
                            loc:    d.loc.clone(),
                            tail:   ast::Tail::None,
                        },
                        d.loc.clone(), Tags::new()
                    )?;

                    let ret = if let Some(ret) = ret {
                        if let ast::Type::Other(_) = ret.typed.t {
                            return Err(Error::new(format!("theory is unprovable"), vec![
                                (ret.typed.loc.clone(), format!("theory must return builtin sized type"))
                            ]));
                        } else {
                            ret.typed.clone()
                        }
                    } else {
                        return Err(Error::new(format!("theory needs a return value"), vec![
                            (d.loc.clone(), format!("theory must return builtin sized type"))
                        ]));
                    };

                    self.memory[sym].value = Value::Theory{args: args.clone(), ret: ret.clone()};

                    let ssa_args = args.iter().map(|t|{
                        Self::smt_type(&t.typed)
                    }).collect::<Vec<_>>();

                    self.ssa.theory(sym, ssa_args, &d.name, Self::smt_type(&ret));
                },
                ast::Def::Function{args, body, vararg, ret, callassert, calleffect, ..} => {

                    let sym = self.alloc(Name::from(&d.name), ast::Typed{
                        t:      ast::Type::Other(Name::from(&d.name.clone())),
                        ptr:    Vec::new(),
                        loc:    d.loc.clone(),
                        tail:   ast::Tail::None,
                    },
                    d.loc.clone(), Tags::new()
                    )?;

                    self.memory[sym].value = Value::Function {
                        loc:    d.loc.clone(),
                        args:   args.clone(),
                        vararg: *vararg,
                        ret:    ret.as_ref().map(|r|r.typed.clone()),
                        callsite_assert: callassert.clone(),
                        callsite_effect: calleffect.clone(),
                    };
                    self.ssa_mark_safe(sym, &d.loc)?;

                },
                ast::Def::Static {tags, typed, expr, array, ..} => {

                    let mut typed = typed.clone();
                    if array.is_some() {
                        typed.ptr.push(ast::Pointer{
                            loc:  d.loc.clone(),
                            tags: Tags::new(),
                        });
                    }
                    let sym = self.alloc(
                        Name::from(&d.name),
                        typed.clone(),
                        d.loc.clone(),
                        tags.clone(),
                    )?;

                    if let Some(array) = array {
                        if let Some(expr) = array {
                            let asym = self.execute_expr(expr)?;
                            let val = self.ssa.value((asym, self.memory[asym].temporal), |a,_| match a {
                                smt::Assertion::Constrained(i) => {
                                    Ok(i)
                                },
                                _ => {
                                    Err(Error::new("array size must be static".to_string(), vec![
                                        (expr.loc().clone(), format!("expression cannot be reduced to a constrained value at compile time"))
                                    ]))
                                }
                            })?;

                            self.memory[sym].value = Value::Array {
                                len:    val as usize,
                                array:  HashMap::new(),
                            };
                            self.len_into_ssa(sym, expr.loc(), val as usize)?;

                        } else {
                            self.memory[sym].value = Value::Array {
                                len:    0,
                                array:  HashMap::new(),
                            };
                        }
                    }

                    let esym = self.execute_expr(expr)?;
                    self.copy(sym, esym, &d.loc)?;
                    self.tail_into_ssa(sym, &d.loc)?;
                },
                ast::Def::Const { typed, expr} => {
                    let sym = self.alloc(
                        Name::from(&d.name),
                        typed.clone(),
                        d.loc.clone(), Tags::new()
                    )?;
                    let esym = self.execute_expr(expr)?;
                    self.copy(sym, esym, &d.loc)?;
                },
                ast::Def::Fntype {ret,args,attr,vararg,..} => {
                    let sym = self.alloc(
                        Name::from(&d.name),
                        ast::Typed{
                            t:      ast::Type::Other(Name::from(&d.name.clone())),
                            ptr:    Vec::new(),
                            loc:    d.loc.clone(),
                            tail:   ast::Tail::None,
                        },
                        d.loc.clone(), Tags::new()
                    )?;
                },
                ast::Def::Struct {fields, packed, tail, union, impls} => {
                    let sym = self.alloc(
                        Name::from(&d.name),
                        ast::Typed{
                            t:      ast::Type::Other(Name::from(&d.name.clone())),
                            ptr:    Vec::new(),
                            loc:    d.loc.clone(),
                            tail:   ast::Tail::None,
                        },
                        d.loc.clone(), Tags::new()
                    )?;
                },
                ast::Def::Enum{names} => {
                    let sym = self.alloc(
                        Name::from(&d.name),
                        ast::Typed{
                            t:      ast::Type::Other(Name::from(&d.name.clone())),
                            ptr:    Vec::new(),
                            loc:    d.loc.clone(),
                            tail:   ast::Tail::None,
                        },
                        d.loc.clone(), Tags::new()
                    )?;
                    let mut value = 0;
                    for (name, val) in names {
                        let mut localname = Name::from(&d.name);
                        localname.push(name.clone());

                        if let Some(val) = val {
                            value = *val;
                        }

                        let t = ast::Typed {
                            t:      ast::Type::ULiteral,
                            loc:    d.loc.clone(),
                            ptr:    Vec::new(),
                            tail:   ast::Tail::None,
                        };
                        let sym = self.alloc(
                            localname,
                            t,
                            d.loc.clone(),
                            ast::Tags::new(),
                        )?;
                        self.memory[sym].value = Value::Integer(value);

                        self.ssa.literal(sym, value, self.memory[sym].t.clone());

                        value += 1;
                    }
                },
                ast::Def::Macro{args, body} => {
                    let sym = self.alloc(
                        Name::from(&d.name),
                        ast::Typed{
                            t:      ast::Type::Other(Name::from(&d.name.clone())),
                            ptr:    Vec::new(),
                            loc:    d.loc.clone(),
                            tail:   ast::Tail::None,
                        },
                        d.loc.clone(), Tags::new()
                    )?;
                    self.memory[sym].value = Value::Unconstrained("macro".to_string());
                },
                ast::Def::Testcase {..} => {},
                ast::Def::Include {..} => {},
            }
        }

        let (fun,_,_) = &mut module.d[fun];

        match &mut fun.def {
            ast::Def::Function{args, body, vararg, ret, callassert, calleffect, ..} => {
                self.execute_function(&fun.name, args, ret.as_ref(), body, callassert, calleffect)?;
                if !self.ssa.solve() {
                    return Err(Error::new(format!("function is unprovable"), vec![
                        (fun.loc.clone(), format!("this function body is impossible to prove"))
                    ]));
                }
            },
            _ => unreachable!(),
        }


        Ok(())
    }

    fn execute_function(
        &mut self,
        name: &String,
        args: &Vec<ast::NamedArg>,
        ret:  Option<&ast::AnonArg>,
        body: &mut ast::Block,
        callassert: &mut Vec<ast::Expression>,
        calleffect: &mut Vec<ast::Expression>,
    ) -> Result<(), Error> {

        self.push(format!("function {}", name));
        self.ssa.push(&format!("\n\n\n\
                                ;----------------------------------------------\n\
                                ;function {}\n\
                                ;----------------------------------------------\n\
                                ", name));
        self.ssa.branch();
        self.current_function_name  = name.clone();
        self.current_function_model = calleffect.clone();

        let mut prev : Option<Symbol> =  None;
        for i in 0..args.len() {
            let argname = Name::from(&args[i].name);
            let sym = self.alloc(argname.clone(), args[i].typed.clone(), args[i].loc.clone(), args[i].tags.clone())?;
            self.memory[sym].value = Value::Unconstrained(format!("passed by value as {}", argname));

            if args[i].tags.contains("tail") {
                let prev = match prev {
                    Some(v) => v,
                    None => {
                        return Err(Error::new(format!("tail tag without previous arg"), vec![
                            (args[i].loc.clone(), format!("something went wrong here"))
                        ]));
                    }
                };

                if self.memory[sym].typed.t != ast::Type::USize {
                    return Err(Error::new(format!("ICE: tail binding not emitted as usize"), vec![
                        (args[i].loc.clone(), format!("this is a bug"))
                    ]));
                }

                self.ssa.debug_loc(&args[i].loc);

                let field_name = match &self.memory[prev].typed.t {
                    ast::Type::Other(n) => match self.defs.get(&n) {
                        Some(ast::Def::Struct{fields, ..}) => {
                            if fields.len() < 1 {
                                return Err(Error::new(format!("tail binding on struct with no members"), vec![
                                    (self.memory[prev].declared.clone(), format!("this struct must have members"))
                                ]));
                            }
                            fields.last().unwrap().name.clone()
                        },
                        _ => {
                            return Err(Error::new(format!("tail value on non struct"), vec![
                                (self.memory[prev].declared.clone(), format!("cannot use tail binding"))
                            ]));
                        }
                    },
                    _ => {
                        return Err(Error::new(format!("tail value on non struct"), vec![
                            (self.memory[prev].declared.clone(), format!("cannot use tail binding"))
                        ]));
                    }
                };


                if self.memory[prev].typed.ptr.len() != 1 {
                    return Err(Error::new(format!("tail passed as non pointer"), vec![
                        (self.memory[prev].declared.clone(), format!("tails can only be passed if the type is passed as pointer depth 1"))
                    ]));
                }

                let mut nutype = self.memory[prev].typed.clone();
                nutype.ptr.pop();
                let sym2 = self.temporary(
                    format!("deref(S{}_{})", prev, self.memory[prev].name),
                    nutype.clone(),
                    args[i].loc.clone(),
                    self.memory[prev].tags.clone(),
                )?;
                self.memory[prev].value = Value::Address(sym2);

                let member_sym = self.member_access(sym2, &field_name, &args[i].loc)?;
                let tmp = self.temporary(
                    format!("len({})", self.memory[sym2].name),
                    ast::Typed{
                        t:      ast::Type::USize,
                        ptr:    Vec::new(),
                        loc:    args[i].loc.clone(),
                        tail:   ast::Tail::None,
                    },
                    args[i].loc.clone(),
                    Tags::new(),
                )?;
                let lensym = self.builtin.get("len").expect("ICE: len theory not built in");
                self.ssa.invocation(*lensym, vec![(member_sym, self.memory[member_sym].temporal)], (tmp, 0));
                self.ssa.assign(
                    (tmp, self.memory[tmp].temporal),
                    (sym, self.memory[sym].temporal),
                    self.memory[tmp].t.clone(),
                );

            }

            prev = Some(sym);
        }

        for callassert in callassert {
            let sym = self.execute_expr(callassert)?;
            if self.memory[sym].t != smt::Type::Bool {
                return Err(Error::new(format!("expected boolean, got {}", self.memory[sym].typed), vec![
                    (callassert.loc().clone(), format!("coercion to boolean is not well defined"))
                ]));
            }
            if !self.ssa.attest((sym, self.memory[sym].temporal), true) {
                return Err(Error::new(format!("callsite assert broke ssa solution"), vec![
                    (callassert.loc().clone(), format!("there may be conflicting constraints"))
                ]));
            }
        }

        if let Some(ret) = ret {
            self.current_function_ret = Some(self.alloc(Name::from("return"), ret.typed.clone(), ret.typed.loc.clone(), ast::Tags::new())?);
        } else {
            self.current_function_ret = None;
        }

        self.execute_scope(&mut body.statements)?;

        self.check_function_model(&body.end)?;


        self.ssa.unbranch(false);
        self.ssa.pop(&format!("end of function {}\n\n", name));
        self.pop();
        Ok(())
    }


    fn check_function_model(&mut self, end: &ast::Location) -> Result<(), Error> {
        if self.current_function_model.len() < 1 {
            return Ok(());
        }

        self.push("model check".into());
        self.ssa.push("model check");

        let mut syms = Vec::new();
        let mut locs = Vec::new();

        for callsite_effect in &mut self.current_function_model.clone() {

            self.in_model = true;
            let casym = self.execute_expr(callsite_effect)?;
            self.in_model = false;

            if self.memory[casym].t != smt::Type::Bool {
                return Err(Error::new(format!("expected boolean, got {}", self.memory[casym].typed), vec![
                    (callsite_effect.loc().clone(), format!("model expression must be boolean"))
                ]));
            }

            syms.push((casym, self.memory[casym].temporal));
            locs.push(callsite_effect.loc().clone());
        }


        // try the fast path
        let ok = self.ssa.assert(syms.clone(), |val,_|val);


        // one assertion broke, try them individually
        if !ok {
            for (sym,loc) in syms.into_iter().zip(locs.into_iter()) {
                self.ssa.assert(vec![sym], |a,model| match a {
                    false  => {
                        let mut estack = vec![
                            (loc.clone(), format!("function does not behave like this model")),
                            (end.clone(), format!("when returning here")),
                        ];
                        if let Some(model) = &model {
                            estack.extend(self.demonstrate(model, sym, 0));
                        }
                        Err(Error::new(format!("unproven model"), estack))
                    }
                    true => {
                        Ok(())
                    }
                })?;
            }
        };

        self.ssa.pop("end of model check");
        self.pop();
        Ok(())

    }


    fn type_coersion(&mut self, a: Symbol, b: Symbol, here: &ast::Location) -> Result<(ast::Typed, Symbol, Symbol), Error>  {


        if let Value::Theory{..} =  self.memory[a].value {
            return Err(Error::new(format!("theory '{}' is not a real world object", self.memory[a].name ), vec![
                (here.clone(), format!("cannot use theory in this instance"))
            ]));
        }
        if let Value::Theory{..} =  self.memory[b].value {
            return Err(Error::new(format!("theory '{}' is not a real world object", self.memory[b].name ), vec![
                (here.clone(), format!("cannot use theory in this instance"))
            ]));
        }

        if self.memory[a].typed == self.memory[b].typed {
            return Ok((self.memory[a].typed.clone(), a,b));
        }

        // if one is an unsigned literal, cast it into the other type
        if self.memory[a].typed.t == ast::Type::ULiteral {
            let tmp = self.temporary(
                format!("implicit coercion of {}", self.memory[a].name),
                self.memory[b].typed.clone(),
                self.memory[a].declared.clone(),
                self.memory[b].tags.clone(),
            )?;

            self.memory[tmp].value = self.memory[a].value.clone();
            self.ssa.assign(
                (tmp,   self.memory[tmp].temporal),
                (a,     self.memory[a].temporal),
                Self::smt_type(&self.memory[tmp].typed),
            );

            return Ok((self.memory[b].typed.clone(), tmp, b));
        }

        if self.memory[b].typed.t == ast::Type::ULiteral {
            let tmp = self.temporary(
                format!("implicit coercion of {}", self.memory[b].name),
                self.memory[a].typed.clone(),
                self.memory[b].declared.clone(),
                self.memory[a].tags.clone(),
            )?;

            self.memory[tmp].value = self.memory[b].value.clone();
            self.ssa.assign(
                (tmp,   self.memory[tmp].temporal),
                (b,     self.memory[b].temporal),
                Self::smt_type(&self.memory[tmp].typed),
            );

            return Ok((self.memory[a].typed.clone(), a, tmp));
        }


        // TODO if the lhs is a pointer, do an implicit cast
        if self.memory[a].typed.ptr.len() > 0 {
            let tmp = self.temporary(
                format!("implicit cast of {}", self.memory[b].name),
                self.memory[a].typed.clone(),
                here.clone(),
                self.memory[a].tags.clone(),
            )?;

            self.memory[tmp].value = self.memory[b].value.clone();
            self.ssa.assign(
                (tmp,   self.memory[tmp].temporal),
                (b,     self.memory[b].temporal),
                Self::smt_type(&self.memory[tmp].typed),
            );
            return Ok((self.memory[a].typed.clone(), a, tmp));
        }

        if let (ast::Type::Other(at), ast::Type::Other(bt)) = (&self.memory[a].typed.t, &self.memory[b].typed.t) {
            if let (Some(ast::Def::Fntype{..}), Some(ast::Def::Function{..})) = (self.defs.get(at), self.defs.get(bt)) {
                return Ok((self.memory[b].typed.clone(), a, b));
            }
        }

        // object copy with rhs having no tail
        // TODO in theory the C compiler should init the tail with zeroes, but copy traits would be better
        if self.memory[a].typed.t == self.memory[b].typed.t && self.memory[b].typed.tail == ast::Tail::None {
            return Ok((self.memory[a].typed.clone(), a, b));
        }

        return Err(Error::new(format!("incompatible types {} and {}", self.memory[a].typed, self.memory[b].typed), vec![
            (here.clone(), format!("this expression is unprovable over incompatible types")),
            (self.memory[a].declared.clone(), format!("{} := {} {:?}", self.memory[a].name, self.memory[a].typed, self.memory[a].value)),
            (self.memory[b].declared.clone(), format!("{} := {} {:?}", self.memory[b].name, self.memory[b].typed, self.memory[a].value))
        ]));
    }


    fn execute_scope(&mut self, body: &mut Vec<Box<ast::Statement>>) -> Result<ScopeReturn, Error> {
        for i in 0..body.len() {
            let (body, rest) = body.split_at_mut(i + 1);

            match body[i].as_mut() {
                ast::Statement::Var{loc, typed, tags, name, array, assign} => {

                    let mut typed = typed.clone();
                    if array.is_some() {
                        typed.ptr.push(ast::Pointer{
                            loc: loc.clone(),
                            tags: Tags::new(),
                        });
                    }

                    let sym = self.alloc(Name::from(name.as_str()), typed, loc.clone(), tags.clone())?;

                    if let Some(array) = array {
                        self.ssa_mark_safe(sym, loc)?;
                        if let Some(expr) = array {
                            let asym = self.execute_expr(expr)?;
                            let val = self.ssa.value((asym, self.memory[asym].temporal), |a,_| match a {
                                smt::Assertion::Constrained(i) => {
                                    Ok(i)
                                },
                                _ => {
                                    Err(Error::new("array size must be static".to_string(), vec![
                                        (expr.loc().clone(), format!("expression cannot be reduced to a constrained value at compile time"))
                                    ]))
                                }
                            })?;

                            self.memory[sym].value = Value::Array {
                                len:    val as usize,
                                array:  HashMap::new(),
                            };
                            self.len_into_ssa(sym, expr.loc(), val as usize)?;
                        } else {
                            self.memory[sym].value = Value::Array {
                                len:    0,
                                array:  HashMap::new(),
                            };
                        }
                    }

                    self.tail_into_ssa(sym, loc)?;

                    if let Some(assign) = assign {
                        let sym2 = self.execute_expr(assign)?;
                        self.copy(sym, sym2, loc)?;
                    };

                },
                ast::Statement::If{branches} => {
                    /*
                    // create branch expansions
                    let freeze = self.memory.clone();
                    self.push("branch".to_string());
                    for (loc, expr, body2) in branches {




                        expanded.push((negative_stack.clone(), expr.clone(), loc.clone(), body2));
                        if let Some(expr) = expr {
                            // we have to execute all conditionals once, because there might be translations
                            // TODO this should be in expand, not there
                            self.ssa.push("dummy execution");
                            self.execute_expr(expr)?;
                            self.ssa.pop("end of dummy");
                            negative_stack.push((loc.clone(), expr.clone()));
                        }

                    }
                    self.pop();
                    self.memory = freeze;
                    */


                    let mut previous_ifs : Vec<(TemporalSymbol, ast::Location)> = Vec::new();

                    for (branch_loc, ref mut branch_expr, branch_body) in branches {

                        let positive_sym = if let Some(branch_expr) = branch_expr {
                            let sym = self.execute_expr(branch_expr)?;

                            if self.memory[sym].typed.t != ast::Type::Bool {
                                return Err(Error::new(format!("expected boolean, got {}", self.memory[sym].typed), vec![
                                    (branch_expr.loc().clone(), format!("coercion to boolean is difficult to prove"))
                                ]));
                            }
                            let sym = (sym, self.memory[sym].temporal);


                            self.ssa.bool_value(sym, |a,_model| match a {
                                smt::Assertion::Constrained(val) => {
                                    emit_warn("unnecessary branch condition", &[
                                        (branch_expr.loc().clone(), format!("expression is always {}", val))
                                    ]);
                                }
                                _ => {}
                            });

                            self.cur().trace.push((sym, branch_expr.loc().clone(), false));

                            self.ssa.debug_loc(&branch_expr.loc());
                            Some((sym, branch_expr.loc().clone()))
                        } else {
                            None
                        };

                        self.push("branch".to_string());
                        self.ssa.debug_loc(&branch_loc);
                        self.ssa.branch();

                        if let Some((sym, _)) = &positive_sym{
                            self.ssa.constrain_branch(*sym, true);
                        }

                        // all previous expressions are therefor false
                        for (sym, loc) in &previous_ifs {
                            self.ssa.constrain_branch(*sym, false);
                            self.cur().trace.push((*sym, loc.clone(), false));
                        }

                        if let Some((sym, loc)) = &positive_sym{
                            previous_ifs.push((sym.clone(), loc.clone()));
                        }

                        let rere = self.execute_scope(&mut branch_body.statements)?;
                        self.ssa.debug("end branch");

                        if let ScopeReturn::Return(_) = rere {
                            self.ssa.unbranch(true);
                        } else {
                            self.ssa.unbranch(false);
                        }
                        self.pop();
                    }

                    // continue execution as if no condition was met
                }
                ast::Statement::Expr{expr, ..} => {
                    self.execute_expr(expr)?;
                }
                ast::Statement::Return{loc, expr} => {
                    if let Some(expr) = expr  {
                        let e = self.execute_expr(expr)?;
                        if let Some(retsym) =  self.current_function_ret {
                            self.copy(retsym, e, expr.loc())?;
                        }
                    }
                    self.check_function_model(loc)?;
                    // stop. do not execute anything behind return
                    return Ok(ScopeReturn::Return(loc.clone()));
                }
                ast::Statement::Label{..} => {
                },
                ast::Statement::Mark{..} => {
                },
                ast::Statement::Switch{expr, cases, default, ..} => {


                    let switchsym = self.execute_expr(expr)?;

                    for (conds, body) in cases {
                        for expr2 in conds {

                            let compsym = self.execute_expr(expr2)?;

                            let (_, switchsym, compsym) = self.type_coersion(switchsym, compsym, expr2.loc())?;

                            let switchmatch = self.temporary(
                                format!("switch branch ({}=={})",
                                self.memory[switchsym].name,
                                self.memory[compsym].name),
                                ast::Typed{
                                    t:      ast::Type::Bool,
                                    ptr:    Vec::new(),
                                    loc:    expr2.loc().clone(),
                                    tail:   ast::Tail::None,
                                },
                                expr2.loc().clone(),
                                ast::Tags::new(),
                            )?;


                            self.ssa.infix_op(
                                switchmatch,
                                (switchsym , self.memory[switchsym].temporal),
                                (compsym, self.memory[compsym].temporal),
                                ast::InfixOperator::Equals,
                                self.memory[switchmatch].t.clone(),
                                false,
                            );

                            let switchmatch = (switchmatch, self.memory[switchmatch].temporal);

                            self.push("case".into());
                            self.ssa.branch();
                            self.ssa.constrain_branch(switchmatch, true);

                            let rere = self.execute_scope(&mut body.statements)?;
                            if let ScopeReturn::Return(_) = rere {
                                self.ssa.unbranch(true);
                            } else {
                                self.ssa.unbranch(false);
                            }

                            self.pop();
                            self.cur().trace.push((switchmatch, expr2.loc().clone(), true));
                        }
                    }

                    if let Some(default) = default {
                        self.push("case".into());
                        self.ssa.branch();

                        let rere = self.execute_scope(&mut default.statements)?;
                        if let ScopeReturn::Return(_) = rere {
                            self.ssa.unbranch(true);
                        } else {
                            self.ssa.unbranch(false);
                        }

                        self.pop();
                    }
                }
                ast::Statement::Assign{loc, lhs, op, rhs} => {
                    let lhs = self.execute_expr(lhs)?;
                    let rhs = self.execute_expr(rhs)?;

                    let (newtype, lhs, rhs) = self.type_coersion(lhs, rhs, loc)?;


                    if let ast::AssignOperator::Eq = op {
                        if self.in_loop {
                            self.memory[lhs].temporal += 1;
                        } else {
                            self.copy(lhs, rhs, loc)?;
                        }
                    } else {
                        let tmp = self.temporary("assign inter".to_string(),
                            newtype.clone(),
                            loc.clone(),
                            self.memory[lhs].tags.clone(),
                        )?;

                        if newtype.ptr.len() > 0 {
                            return Err(Error::new(format!("assign arithmetic is not yet implemented"), vec![
                                (loc.clone(), format!("use a=a+n instead of a+=n"))
                            ]));
                        }


                        let infix = match op {
                            ast::AssignOperator::Bitor  => ast::InfixOperator::Bitor,
                            ast::AssignOperator::Bitand => ast::InfixOperator::Bitand,
                            ast::AssignOperator::Add    => ast::InfixOperator::Add,
                            ast::AssignOperator::Sub    => ast::InfixOperator::Subtract,
                            ast::AssignOperator::Eq     => {
                                unreachable!();
                            }
                        };
                        let value = Value::InfixOp {
                            lhs:    (lhs, self.memory[lhs].temporal),
                            rhs:    (rhs, self.memory[rhs].temporal),
                            op:     infix.clone(),
                        };
                        self.memory[tmp].value = value;

                        self.ssa.infix_op(
                            tmp,
                            (lhs, self.memory[lhs].temporal),
                            (rhs, self.memory[rhs].temporal),
                            infix,
                            self.memory[tmp].t.clone(),
                            newtype.t.signed(),
                         );

                        if self.in_loop {
                            self.memory[lhs].temporal += 1;
                        } else {
                            self.copy(lhs, tmp, loc)?;
                        }
                    }
                }
                ast::Statement::Continue{loc} => {
                    return Ok(ScopeReturn::Return(loc.clone()));
                }
                ast::Statement::Break{loc} => {
                    return Ok(ScopeReturn::Return(loc.clone()));
                }
                ast::Statement::Block(block) => {
                    self.push("block".to_string());
                    self.ssa.push("block");

                    self.execute_scope(&mut block.statements)?;

                    self.pop();
                    self.ssa.pop("end of block");
                }
                ast::Statement::For{e1,e2,e3,body} => {
                    self.push("for loop".to_string());
                    //self.ssa.push("for loop");

                    let prev_loop = self.in_loop;
                    self.in_loop = false;
                    self.execute_scope(e1)?;
                    self.in_loop = true;

                    self.execute_scope(e3)?;

                    if let Some(expr) = e2 {
                        let sym = self.execute_expr(expr)?;
                        if self.memory[sym].t != smt::Type::Bool {
                            return Err(Error::new(format!("expected boolean, got {}", self.memory[sym].typed), vec![
                                                  (expr.loc().clone(), format!("must be boolean"))
                            ]));
                        }
                        let sym = (sym, self.memory[sym].temporal);
                        self.cur().trace.push((sym.clone(), expr.loc().clone(),false));
                        if !self.ssa.attest(sym, true) {
                            return Err(Error::new(format!("condition breaks ssa"), vec![
                                (expr.loc().clone(), format!("there may be conflicting constraints"))
                            ]));
                        }
                    }


                    self.execute_scope(&mut body.statements)?;
                    self.in_loop = prev_loop;
                    //self.ssa.pop("end of for loop");
                    self.pop();
                }
                ast::Statement::While{expr, body} => {
                    self.push("while loop".to_string());
                    //self.ssa.push("while loop");

                    let sym = self.execute_expr(expr)?;
                    if self.memory[sym].t != smt::Type::Bool {
                        return Err(Error::new(format!("expected boolean, got {}", self.memory[sym].typed), vec![
                            (expr.loc().clone(), format!("must be boolean"))
                        ]));
                    }
                    let sym = (sym, self.memory[sym].temporal);
                    self.cur().trace.push((sym.clone(), expr.loc().clone(),false));

                    if !self.ssa.attest(sym, true) {
                        return Err(Error::new(format!("condition breaks ssa"), vec![
                            (expr.loc().clone(), format!("there may be conflicting constraints"))
                        ]));
                    }

                    let prev_loop = self.in_loop;
                    self.in_loop = true;

                    self.execute_scope(&mut body.statements)?;

                    self.in_loop = prev_loop;
                    //self.ssa.pop("end of while loop");
                    self.pop();
                }
                ast::Statement::Unsafe{..} => {
                }
                ast::Statement::CBlock{..} => {
                }
            }
        }
        Ok(ScopeReturn::NoReturn)
    }

    fn expand_callargs(
        &mut self,
        defined: &Vec<ast::NamedArg>,
        called: &mut Vec<Box<ast::Expression>>,
        callloc: &ast::Location,
    ) -> Result<(), Error>
    {
        let mut callargs = std::mem::replace(called, Vec::new());
        // generated arguments
        for i in 0..defined.len() {
            if let Some(cs) = defined[i].tags.get("callsite_source") {
                let (v, loc) = cs.iter().next().unwrap();
                let lit = match v.as_str() {
                    "file" => {
                        ast::Expression::LiteralString {
                            loc: loc.clone(),
                            v: callloc.file.as_bytes().to_vec(),
                        }
                    },
                    "line" => {
                        ast::Expression::Literal{
                            loc: loc.clone(),
                            v:   format!("{}", callloc.line())
                        }
                    },
                    "module" => {
                        ast::Expression::LiteralString {
                            loc: loc.clone(),
                            v: self.current_module_name.as_bytes().to_vec(),
                        }
                    }
                    "function" => {
                        ast::Expression::LiteralString {
                            loc: loc.clone(),
                            v: self.current_function_name.as_bytes().to_vec(),
                        }
                    }
                    _ => {
                        return Err(Error::new(format!("invalid callsite_source"), vec![
                            (loc.clone(), format!("")),
                        ]));
                    }
                };
                let genarg = Box::new(lit);
                called.push(genarg);
            } else if let Some(_) = defined[i].tags.get("tail") {
                let mut prev = match called.get_mut(i-1) {
                    Some(v) => v,
                    None => {
                        return Err(Error::new(format!("tail tag without previous arg"), vec![
                            (callloc.clone(), format!("something went wrong here"))
                        ]));
                    }
                };

                let callptr = self.execute_expr(&mut prev)?;

                let prev_loc = prev.loc().clone();
                let mut into_type = self.memory[callptr].typed.clone();

                // cast to unsized type for the C compiler to be happy
                if into_type.tail != ast::Tail::None {
                    into_type.tail = ast::Tail::Dynamic;
                }
                *prev = Box::new(ast::Expression::Cast {
                    into: into_type,
                    expr: prev.clone(),
                    loc:  prev.loc().clone(),
                });

                match &self.memory[callptr].typed.tail.clone() {
                    ast::Tail::Bind(name,loc) => {
                        let genarg = Box::new(ast::Expression::Name(ast::Typed{
                            t: ast::Type::Other(Name::from(name)),
                            ptr: Vec::new(),
                            loc: loc.clone(),
                            tail: ast::Tail::None,
                        }));
                        called.push(genarg);
                    }
                    ast::Tail::Dynamic | ast::Tail::None  => {
                        return Err(Error::new(format!("tail size of {} not bound", self.memory[callptr].name), vec![
                            (prev_loc.clone(), format!("tail len required here")),
                            (defined[i].loc.clone(), format!("required by this tail binding")),
                        ]));
                    },
                    ast::Tail::Static(v,loc) => {
                        let genarg = Box::new(ast::Expression::Literal{
                            loc: loc.clone(),
                            v:   format!("{}", v),
                        });
                        called.push(genarg);
                    }
                }
            } else {

                if callargs.len() > 0 {
                    let mut calledarg = callargs.remove(0);
                    let callptr = self.execute_expr(&mut calledarg)?;


                    if self.memory[callptr].typed !=  defined[i].typed {

                        let mut into_type = self.memory[callptr].typed.clone();

                        // pointers to structs can be used as pointers to their first field
                        if let ast::Type::Other(n) = &self.memory[callptr].typed.t {
                            if let Some(ast::Def::Struct {fields, ..}) = self.defs.get(&n) {
                                if let Some(field) = fields.get(0) {
                                    if field.typed.t == defined[i].typed.t {
                                        into_type.t = field.typed.t.clone();
                                        into_type.tail = ast::Tail::None;
                                    }
                                }
                            }
                        }

                        // pointers with tail can be used as pointer without tail
                        match (&defined[i].typed.tail, &self.memory[callptr].typed.tail) {
                            (ast::Tail::None, ast::Tail::Bind(_,_)) |
                            (ast::Tail::None, ast::Tail::Static(_,_)) => {
                                into_type.tail = ast::Tail::None;
                            }
                            _ => {
                            }
                        }

                        if into_type != self.memory[callptr].typed {
                            *calledarg = ast::Expression::Cast {
                                into: into_type,
                                expr: calledarg.clone(),
                                loc:  calledarg.loc().clone(),
                            };
                        }
                    }
                    called.push(calledarg);
                }
            }
        }
        called.extend(callargs);
        Ok(())
    }



    fn member_access(&mut self, lhs_sym: Symbol, rhs: &str, loc: &ast::Location) -> Result<Symbol, Error> {

        let mut struct_def = None;
        if let ast::Type::Other(n) = &self.memory[lhs_sym].typed.t {
            if let Some(ast::Def::Struct {fields, tail, ..}) = self.defs.get(&n) {
                struct_def = Some((fields.clone(), tail));
            }
        };

        let struct_def = match struct_def {
            Some(v) => v,
            None => {
                return Err(Error::new(
                    format!("{} is not accessible as struct. it is {}", self.memory[lhs_sym].name, self.memory[lhs_sym].typed), vec![
                    (loc.clone(), format!("cannot use as struct here"))
                ]));
            }
        };

        let mut field = None;
        for f in struct_def.0.iter() {
            if &f.name == rhs {
                field = Some((f.name.clone(), f));
            }
        }

        let field = match field  {
            Some(f) => f,
            None => {
                return Err(Error::new(format!("{} does not a have a field named {}", self.memory[lhs_sym].typed, rhs), vec![
                    (loc.clone(), format!("cannot access struct here"))
                ]));
            }
        };

        match &self.memory[lhs_sym].value {
            Value::Struct{members,..} => {
                if let Some(sym) = members.get(&field.0) {
                    return Ok(*sym);
                }
            },
            Value::Uninitialized | Value::Unconstrained(_) => {
                self.memory[lhs_sym].value = Value::Struct{
                    members:  HashMap::new(),
                };
            },
            o => {
                return Err(Error::new(
                        format!("{} is not accessible as struct. it is {}", self.memory[lhs_sym].name, o), vec![
                        (loc.clone(), format!("cannot use as struct here"))
                        ]));
            }
        }


        let mut fieldvalue = Value::Uninitialized;
        let mut fieldtyped = field.1.typed.clone();


        if let Some(array) = &field.1.array {

            // sized array
            if let Some(expr) = array {
                let mut expr = expr.clone();
                let asym = self.execute_expr(&mut expr)?;
                let val = self.ssa.value((asym, self.memory[asym].temporal), |a,_| match a {
                    smt::Assertion::Constrained(i) => {
                        Ok(i)
                    },
                    _ => {
                        Err(Error::new("array size must be static".to_string(), vec![
                            (expr.loc().clone(), format!("expression cannot be reduced to a constrained value at compile time"))
                        ]))
                    }
                })?;
                fieldvalue = Value::Array {
                    len:    val as usize,
                    array:  HashMap::new(),
                };

            // unsized array, but container has a tail
            } else if let ast::Tail::Static(val,_loc) = &self.memory[lhs_sym].typed.tail {
                fieldvalue = Value::Array {
                    len:    *val as usize,
                    array:  HashMap::new(),
                };
            }

            fieldtyped.ptr.push(ast::Pointer{
                loc:  field.1.loc.clone(),
                tags: Tags::new(),
            });
        }


        //nested tail
        match fieldtyped.tail {
            ast::Tail::Dynamic => {
                fieldtyped.tail = self.memory[lhs_sym].typed.tail.clone();
                //return Err(Error::new(
                //    format!("TODO: implement access to nested tail member"), vec![
                //    (loc.clone(), format!("here"))
                //]));
            }
            _ => {},
        }

        let tmp = self.temporary(
            format!("{}.{}", self.memory[lhs_sym].name, rhs),
            fieldtyped,
            loc.clone(),
            field.1.tags.clone(),
        )?;


        match (&fieldvalue, &field.1.array) {
            (Value::Array{len,..}, _) if *len > 0 => {
                self.len_into_ssa(tmp, loc, *len)?;
                self.ssa_mark_safe(tmp, loc)?;
            },
            (_, Some(_)) => {
                // access to an array member as pointer is always safe, because its part of the struct mem
                self.ssa_mark_safe(tmp, loc)?;
            },
            _ => (),
        }
        self.memory[tmp].value = fieldvalue;




        match &mut self.memory[lhs_sym].value {
            Value::Struct{members, ..} => {
                members.insert(field.0, tmp);
            }
            _ => unreachable!(),
        }
        Ok(tmp)
    }

    fn execute_expr(&mut self, expr: &mut ast::Expression) -> Result<Symbol, Error> {
        self.ssa.debug_loc(expr.loc());
        match expr {
            ast::Expression::Name(name) => {
                match &name.t {
                    ast::Type::Other(n) => self.name(&n, &name.loc),
                    _ => {
                        return Err(Error::new(format!("builtin type '{}' is  not an object", name), vec![
                            (name.loc.clone(), format!("cannot use builtin here"))
                        ]));
                    }
                }
            },
            ast::Expression::MemberAccess {loc, lhs, rhs, op, ..} => {
                let mut lhs_sym = self.execute_expr(lhs)?;
                if op == "->" {
                    lhs_sym = self.deref(lhs_sym, loc)?;
                }

                match self.member_access(lhs_sym, rhs, loc) {
                    Ok(v) => Ok(v),
                    Err(e) => {
                        if let ast::Type::Other(name) = &self.memory[lhs_sym].typed.t {
                            let mut name = name.clone();
                            name.pop();
                            name.push(rhs.to_string());
                            if let Ok(sym) = self.name(&name, loc) {
                                if let Value::Function{..} = &self.memory[sym].value {
                                    let tmp = self.temporary(format!("desugar of self call {}", name),
                                        ast::Typed{
                                            t:      ast::Type::USize,
                                            ptr:    Vec::new(),
                                            loc:    loc.clone(),
                                            tail:   ast::Tail::None,
                                        },
                                        loc.clone(),
                                        Tags::new(),
                                    )?;


                                    let loc = loc.clone();
                                    let op  = op.clone();

                                    let mut selfarg = lhs.clone();

                                    if op == "." {
                                        selfarg = Box::new(ast::Expression::UnaryPre{
                                            loc:    loc.clone(),
                                            op:     ast::PrefixOperator::AddressOf,
                                            expr:   selfarg,
                                        });
                                    }


                                    self.memory[tmp].value = Value::SelfCall {
                                        name: ast::Expression::Name(ast::Typed{
                                            t:      ast::Type::Other(name),
                                            loc:    loc.clone(),
                                            ptr:    Vec::new(),
                                            tail:   ast::Tail::None,
                                        }),
                                        selfarg,
                                    };
                                    return Ok(tmp);
                                }
                            }
                        }
                        return Err(e)
                    }
                }

            },
            ast::Expression::ArrayAccess {lhs, rhs, loc} => {
                let lhs_sym = self.execute_expr(lhs)?;
                let rhs_sym = self.execute_expr(rhs)?;

                if self.memory[rhs_sym].typed.t.signed() {
                    return Err(Error::new(format!("array access with signed index is not well defined"), vec![
                        (rhs.loc().clone(), format!("array index must be of type usize"))
                    ]))
                }

                if self.memory[rhs_sym].typed.t != ast::Type::USize && self.memory[rhs_sym].typed.t != ast::Type::ULiteral {
                    return Err(Error::new(format!("array access with something not a usize"), vec![
                        (rhs.loc().clone(), format!("array index must be of type usize"))
                    ]))
                }

                let static_index = if let smt::Assertion::Constrained(i)  = self.ssa.value((rhs_sym, self.memory[rhs_sym].temporal), |a,_|a) {
                    Some(i as usize)
                } else {
                    None
                };
                match &self.memory[lhs_sym].value {
                    Value::Array{array, ..} => {
                        if let Some(i) = &static_index {
                            if let Some(sym) = array.get(i) {
                                return Ok(*sym);
                            }
                        }
                    },
                    _ => ()
                }

                if self.memory[lhs_sym].t != smt::Type::Unsigned(64) {
                    return Err(Error::new(format!("cannot prove memory access due to unexpected type"), vec![
                        (lhs.loc().clone(), format!("lhs of array expression appears to be not a pointer or array"))
                    ]))
                }

                self.ssa.debug("begin array bounds");
                let tmp1 = self.temporary(format!("len({})", self.memory[lhs_sym].name),
                ast::Typed{
                    t:      ast::Type::USize,
                    ptr:    Vec::new(),
                    loc:    loc.clone(),
                    tail:   ast::Tail::None,
                },
                loc.clone(),
                Tags::new(),
                )?;
                let lensym = self.builtin.get("len").expect("ICE: len theory not built in");
                self.ssa.invocation(*lensym, vec![(lhs_sym, self.memory[lhs_sym].temporal)], (tmp1, 0));

                let tmp2 = self.temporary(format!("{} < len({})", self.memory[rhs_sym].name, self.memory[lhs_sym].name),
                ast::Typed{
                    t:      ast::Type::Bool,
                    ptr:    Vec::new(),
                    loc:    loc.clone(),
                    tail:   ast::Tail::None,
                },
                loc.clone(),
                Tags::new(),
                )?;

                self.memory[tmp2].value = Value::InfixOp {
                    lhs:    (rhs_sym, self.memory[rhs_sym].temporal),
                    rhs:    (tmp1, 0),
                    op:     ast::InfixOperator::Lessthan,
                };
                self.ssa.infix_op(
                    tmp2,
                    (rhs_sym, self.memory[rhs_sym].temporal),
                    (tmp1, 0),
                    ast::InfixOperator::Lessthan,
                    smt::Type::Bool,
                    false,
                    );

                self.ssa.debug("assert that length less than index is true");
                self.ssa.assert(vec![(tmp2, self.memory[tmp2].temporal)], |a,model| match a {
                    false => {
                        let mut estack = Vec::new();
                        estack.extend(self.demonstrate(model.as_ref().unwrap(), (tmp2, self.memory[tmp2].temporal), 0));
                        Err(Error::new(format!("possible out of bounds array access"), estack))
                    }
                    true => {
                        Ok(())
                    }
                })?;



                let mut newtype = self.memory[lhs_sym].typed.clone();
                newtype.ptr.pop();

                let tmp = self.temporary(
                    format!("array member {}[{}]", self.memory[lhs_sym].name, self.memory[rhs_sym].name),
                    newtype,
                    loc.clone(),
                    self.memory[lhs_sym].tags.clone(),
                )?;
                self.memory[tmp].value = Value::Unconstrained("array content".to_string());

                match &mut self.memory[lhs_sym].value {
                    Value::Array{array, ..} => {
                        if let Some(i) = &static_index {
                            array.insert(*i, tmp);
                        }
                    },
                    _ => ()
                }

                Ok(tmp)
            },
            ast::Expression::LiteralString{ loc, v } => {
                let tmp = self.temporary(
                    format!("literal string \"{}\"", String::from_utf8_lossy(&v)),
                    ast::Typed {
                        t: ast::Type::U8,
                        loc:    loc.clone(),
                        ptr:    vec![ast::Pointer{
                            tags: ast::Tags::new(),
                            loc:    loc.clone(),
                        }],
                        tail:   ast::Tail::None,
                    },
                    loc.clone(),
                    Tags::new(),
                )?;
                self.memory[tmp].value = Value::Array{
                    array:  HashMap::new(),
                    len:    v.len(),
                };
                self.ssa_mark_safe(tmp, loc)?;
                self.ssa_mark_nullterm(tmp, loc)?;
                Ok(tmp)
            }
            ast::Expression::LiteralChar{ loc, v } => {
                let tmp = self.temporary(
                    format!("literal char '{}'", *v as char),
                    ast::Typed {
                        t: ast::Type::Other("::ext::<stddef.h>::char".into()),
                        loc:    loc.clone(),
                        ptr:    Vec::new(),
                        tail:   ast::Tail::None,
                    },
                    loc.clone(),
                    Tags::new(),
                )?;
                self.memory[tmp].value = Value::Integer(*v as u64);
                Ok(tmp)
            }

            ast::Expression::Literal { loc, v } => {
                self.ssa.debug("literal expr");
                if v == "true" {
                    let t = ast::Typed {
                        t:      ast::Type::Bool,
                        loc:    loc.clone(),
                        ptr:    Vec::new(),
                        tail:   ast::Tail::None,
                    };
                    self.literal(loc, Value::Integer(0xffffffff), t)
                } else if v == "false" {
                    let t = ast::Typed {
                        t:      ast::Type::Bool,
                        loc:    loc.clone(),
                        ptr:    Vec::new(),
                        tail:   ast::Tail::None,
                    };
                    self.literal(loc, Value::Integer(0), t)
                } else if let Some(v) = parser::parse_u64(&v) {
                    let t = ast::Typed {
                        t:      ast::Type::ULiteral,
                        loc:    loc.clone(),
                        ptr:    Vec::new(),
                        tail:   ast::Tail::None,
                    };
                    self.literal(loc, Value::Integer(v), t)
                } else {
                    let t = ast::Typed {
                        t:      ast::Type::ULiteral,
                        loc:    loc.clone(),
                        ptr:    Vec::new(),
                        tail:   ast::Tail::None,
                    };
                    self.literal(loc, Value::Unconstrained(format!("literal {}", v)), t)
                }
            }
            ast::Expression::Call { name, ref mut args, loc, ref mut expanded, ref mut emit, .. } => {


                let mut static_name = None;
                if let ast::Expression::Name(ref t) =  name.as_ref() {
                    if let ast::Type::Other(name) = &t.t {
                        static_name = Some(format!("{}", name));
                    }
                }

                if let Some(static_name) = &static_name {
                    self.ssa.debug(&format!("call of {}", static_name));
                } else {
                    self.ssa.debug(&format!("call"));
                }

                match static_name.as_ref().map(|s|s.as_str()) {
                    Some("typeid") => {
                        if args.len() != 1 {
                            return Err(Error::new("call argument count mismatch".to_string(), vec![
                                (name.loc().clone(), format!("builtin needs 1 argument, but you passed {}", args.len()))
                            ]));
                        }
                        let sym = self.execute_expr(&mut args[0])?;



                        let r = self.literal(loc, Value::Integer(1), ast::Typed {
                            t:      ast::Type::ULiteral,
                            loc:    loc.clone(),
                            ptr:    Vec::new(),
                            tail:   ast::Tail::None,
                        });

                        *expr = ast::Expression::Literal{
                            loc: loc.clone(),
                            v:  format!("\"{}\"", self.memory[sym].typed),
                        };
                        return r;
                    },
                    Some("len") => {
                        if args.len() != 1 {
                            return Err(Error::new("call argument count mismatch".to_string(), vec![
                                (name.loc().clone(), format!("builtin needs 1 argument, but you passed {}", args.len()))
                            ]));
                        }
                        // shortcut, as calls to z3 are expensive and can pile up to unsolveable mess
                        let sym = self.execute_expr(&mut args[0])?;
                        if let Value::Array{len,.. } = self.memory[sym].value {
                            if len > 0 {
                                let r = self.literal(loc, Value::Integer(len as u64), ast::Typed {
                                    t:      ast::Type::ULiteral,
                                    loc:    loc.clone(),
                                    ptr:    Vec::new(),
                                    tail:   ast::Tail::None,
                                });
                                return r;
                            }
                        }
                        // no shortcut found, continue
                    }
                    Some("static_attest") => {
                        *emit = ast::EmitBehaviour::Skip;
                        self.ssa.debug_loc(loc);
                        self.ssa.debug("static_attest");
                        if args.len() != 1 {
                            return Err(Error::new("call argument count mismatch".to_string(), vec![
                                (name.loc().clone(), format!("builtin needs 1 argument, but you passed {}", args.len()))
                            ]));
                        }
                        let sym = self.execute_expr(&mut args[0])?;
                        if self.memory[sym].t != smt::Type::Bool {
                            return Err(Error::new(format!("expected boolean, got {}", self.memory[sym].typed), vec![
                                (args[0].loc().clone(), format!("argument must be boolean"))
                            ]));
                        }
                        if !self.ssa.attest((sym, self.memory[sym].temporal), true) {
                            return Err(Error::new(format!("function is unprovable"), vec![
                                (expr.loc().clone(), format!("static_attest leads to conflicting constraints"))
                            ]));
                        }

                        let r = self.literal(loc, Value::Integer(1), ast::Typed {
                            t:      ast::Type::ULiteral,
                            loc:    loc.clone(),
                            ptr:    Vec::new(),
                            tail:   ast::Tail::None,
                        });
                        return r;
                    },
                    Some("static_assert") => {
                        *emit = ast::EmitBehaviour::Skip;
                        self.ssa.debug_loc(loc);
                        self.ssa.debug("static_assert");
                        if args.len() != 1 {
                            return Err(Error::new("call argument count mismatch".to_string(), vec![
                                (name.loc().clone(), format!("builtin needs 1 argument, but you passed {}", args.len()))
                            ]));
                        }
                        let sym = self.execute_expr(&mut args[0])?;
                        if self.memory[sym].t != smt::Type::Bool {
                            return Err(Error::new(format!("expected boolean, got {}", self.memory[sym].typed), vec![
                                (args[0].loc().clone(), format!("coercion to boolean is difficult to prove"))
                            ]));
                        }
                        self.ssa.assert(vec![(sym, self.memory[sym].temporal)], |a,model|match a{
                            false => {
                                let mut estack = vec![(loc.clone(),
                                    format!("you may need an if condition or callsite_assert to increase confidence"))];
                                if let Some(model) = &model {
                                    estack.extend(self.demonstrate(model, (sym, self.memory[sym].temporal), 0));
                                }
                                Err(Error::new(format!("theory is unproven"), estack))
                            }
                            true => {
                                Ok(())
                            }
                        })?;

                        let r = self.literal(loc, Value::Integer(1), ast::Typed {
                            t:      ast::Type::ULiteral,
                            loc:    loc.clone(),
                            ptr:    Vec::new(),
                            tail:   ast::Tail::None,
                        });
                        *expr = ast::Expression::Literal{
                            loc: loc.clone(),
                            v:  "".to_string(),
                        };
                        return r;
                    },
                    Some("static") => {
                        if args.len() != 1 {
                            return Err(Error::new("call argument count mismatch".to_string(), vec![
                                (name.loc().clone(), format!("builtin needs 1 argument, but you passed {}", args.len()))
                            ]));
                        }
                        let sym = self.execute_expr(&mut args[0])?;
                        let val = self.ssa.value((sym, self.memory[sym].temporal), |a,model|match a{
                            smt::Assertion::Unsolveable => {
                                Err(Error::new(format!("static is not solveable"), vec![
                                    (loc.clone(), format!("there may be conflicting constraints"))
                                ]))
                            }
                            smt::Assertion::Unconstrained(_) => {
                                let mut estack = vec![(loc.clone(),
                                format!("you may need an if condition or callsite_assert to increase confidence"))];
                                estack.extend(self.demonstrate(model.as_ref().unwrap(), (sym, self.memory[sym].temporal), 0));
                                Err(Error::new(format!("static is unconstrained"), estack))
                            }
                            smt::Assertion::Constrained(val) => {
                                Ok(val)
                            }
                        })?;

                        let r = self.literal(loc, Value::Integer(val), ast::Typed {
                            t:      ast::Type::ULiteral,
                            loc:    loc.clone(),
                            ptr:    Vec::new(),
                            tail:   ast::Tail::None,
                        });
                        *expr = ast::Expression::Literal{
                            loc: loc.clone(),
                            v:  format!("{}",val),
                        };
                        return r;



                    }
                    _ => (),
                }

                let name_sym = self.execute_expr(name)?;


                match &self.memory[name_sym].value.clone() {
                    Value::Unconstrained(_) | Value::Uninitialized => {
                        if let ast::Type::Other(n) = &self.memory[name_sym].typed.t {
                            if let Some(ast::Def::Fntype {ret,args,attr,vararg,nameloc}) = self.defs.get(&n).cloned() {
                                self.deref(name_sym, loc)?;
                                self.memory[name_sym].value = Value::Function {
                                    loc: nameloc,
                                    args,
                                    vararg: vararg,
                                    ret:    ret.as_ref().map(|r|r.typed.clone()),
                                    callsite_assert: Vec::new(),
                                    callsite_effect: Vec::new(),
                                };

                            }
                        }
                    }
                    _ => ()
                }

                match &self.memory[name_sym].value {
                    Value::Theory{args: fargs, ret} => {
                        *emit = ast::EmitBehaviour::Error {
                            loc:        loc.clone(),
                            message:    format!("assertion of theory {} outside static()", self.memory[name_sym].name),
                        };
                        let fargs = fargs.clone();
                        let ret = ret.clone();
                        if !*expanded {
                            *expanded = true;
                            self.expand_callargs(&fargs, args, loc, )?;
                        }
                        if args.len() != fargs.len() {
                            return Err(Error::new("call argument count mismatch".to_string(), vec![
                                (name.loc().clone(), format!("theory '{}' is defined over {} arguments, but you passed {}",
                                    &self.memory[name_sym].name, fargs.len(), args.len()))
                            ]));
                        }
                        let mut syms = Vec::new();
                        let mut debug_arg_names = Vec::new();
                        for (i, arg) in args.into_iter().enumerate() {
                            let s = self.execute_expr(arg)?;
                            syms.push((s, self.memory[s].temporal));

                            debug_arg_names.push(format!("{}", self.memory[s].name));
                            if Self::smt_type(&fargs[i].typed) != self.memory[s].t
                            {
                                return Err(Error::new(format!("incompatible arguments to theory {}", self.memory[name_sym].name), vec![
                                    (arg.loc().clone(), format!("expected {} got {}", fargs[i].typed , self.memory[s].typed))
                                    ]));
                            }
                        }

                        let tmp = self.temporary(
                            format!("interpretation of theory {} over {}", self.memory[name_sym].name, debug_arg_names.join(" , ")),
                            ret.clone(),
                            loc.clone(),
                            Tags::new(),
                        )?;
                        let value = Value::Unconstrained("interpretation of theory".to_string());
                        self.memory[tmp].value = value;

                        self.ssa.invocation(name_sym, syms, (tmp,0));

                        Ok(tmp)
                    },

                    Value::SelfCall{selfarg, name} => {

                        let mut args = args.clone();
                        args.insert(0, selfarg.clone());

                        *expr = ast::Expression::Call {
                            loc: loc.clone(),
                            name: Box::new(name.clone()),
                            args,
                            expanded: *expanded,
                            emit: emit.clone(),
                        };

                        return self.execute_expr(expr);
                    }
                    Value::Function{args: fargs, ret, vararg, callsite_assert, callsite_effect, loc: functionlloc} => {

                        // borrochecker stupidity
                        let mut callsite_effect = callsite_effect.clone();
                        let vararg = *vararg;
                        let functionlloc = functionlloc.clone();

                        let ret = ret.clone();
                        let fargs = fargs.clone();
                        let mut callsite_assert = callsite_assert.clone();

                        if !*expanded {
                            *expanded = true;
                            self.expand_callargs(&fargs, args, loc)?;
                        }

                        if (args.len() > fargs.len() && !vararg) || args.len() < fargs.len() {
                            return Err(Error::new("call argument count mismatch".to_string(), vec![
                                (name.loc().clone(), format!("function '{}' is defined over {} arguments, but you passed {}",
                                    &self.memory[name_sym].name, fargs.len(), args.len()))
                            ]));
                        }
                        let mut syms = Vec::new();
                        for arg in args.iter_mut() {
                            let s = self.execute_expr(arg)?;
                            syms.push((s,self.memory[s].temporal));
                        }



                        //dont expose any symbols during callsite assert
                        let global_only = vec![self.stack[0].clone()];
                        let stack_original = std::mem::replace(&mut self.stack, global_only);

                        if callsite_assert.len() > 0 {
                            let mut ca_syms = Vec::new();
                            let mut ca_locs = Vec::new();


                            self.push("callsite_assert".to_string());
                            self.ssa.push("callsite_assert");

                            // callsite assert evaluated in the callsite
                            // so we expose the symbol as a different name

                            for (i, farg) in fargs.iter().enumerate() {
                                self.cur().locals.insert(Name::from(&farg.name), syms[i].0);
                            }

                            for callsite_assert in &mut callsite_assert {
                                let casym = self.execute_expr(callsite_assert)?;
                                ca_syms.push((casym, self.memory[casym].temporal));
                                ca_locs.push(callsite_assert.loc().clone());
                            }

                            // try the fast path
                            let ok = self.ssa.assert(ca_syms.clone(), |val,_|val);


                            // one assertion broke, try them individually
                            if !ok {
                                for (sym,loc2) in ca_syms.into_iter().zip(ca_locs.into_iter()) {
                                    self.ssa.assert(vec![sym], |a,model| match a {
                                        false => {
                                            let mut estack = vec![
                                                (loc.clone(),format!("in this callsite")),
                                                (loc2.clone(), format!("function call requires these conditions")),
                                                (functionlloc.clone(), format!("for this function")),
                                            ];
                                            if let Some(model) = &model {
                                                estack.extend(self.demonstrate(model, sym, 0));
                                            }
                                            Err(Error::new(format!("unproven callsite assert for {}", self.memory[sym.0].name), estack))
                                        }
                                        true => {
                                            Ok(())
                                        }
                                    })?;
                                }

                            }
                            self.ssa.pop("end of callsite_assert");
                            self.pop();
                        }

                        self.stack = stack_original;

                        // TODO for now mark all pointer call args as untrackable in the callsite
                        // this should become a full borrow/aliasing checker
                        self.ssa.debug("borrows after call");
                        for (i,(s,_)) in syms.iter().enumerate() {
                            let mut borrow = false;
                            if let Some(farg) = fargs.get(i) {
                                if farg.tags.contains("mut") || farg.tags.contains("alias"){
                                    borrow = true;
                                }
                                for ptr in &farg.typed.ptr {
                                    if ptr.tags.contains("mut") || ptr.tags.contains("alias"){
                                        borrow = true;
                                    }
                                }
                            } else {
                                borrow = true;
                            }
                            if borrow {
                                self.borrow_away(*s);
                            }
                        }
                        self.ssa.debug("end of borrows after call");

                        let return_sym = self.temporary(
                            format!("return value of {}", self.memory[name_sym].name),
                            ret.clone().unwrap_or(ast::Typed{
                                t:      ast::Type::Other(Name::from("void")),
                                loc:    loc.clone(),
                                ptr:    Vec::new(),
                                tail:   ast::Tail::None,
                            }),
                            loc.clone(),
                            Tags::new(),
                        )?;
                        let value = Value::Unconstrained("return value".to_string());
                        self.memory[return_sym].value = value;




                        //dont expose any symbols during callsite effect
                        let global_only = vec![self.stack[0].clone()];
                        let stack_original = std::mem::replace(&mut self.stack, global_only);

                        self.ssa.debug("callsite effects");
                        for callsite_effect in &mut callsite_effect {
                            self.push("callsite_effect".to_string());

                            let return_sym_inner = self.alloc(
                                Name::from("return"),
                                ret.clone().unwrap_or(ast::Typed{
                                    t:      ast::Type::Other(Name::from("void")),
                                    loc:    loc.clone(),
                                    ptr:    Vec::new(),
                                    tail:   ast::Tail::None,
                                }),
                                loc.clone(),
                                Tags::new(),
                            )?;
                            self.copy(return_sym_inner, return_sym, loc)?;

                            // callsite effects happen in the callsite, but the names are from
                            // the function declaration, so we expose the symbol as a different name
                            for (i, farg) in fargs.iter().enumerate() {
                                self.cur().locals.insert(Name::from(&farg.name), syms[i].0);
                            }

                            let casym = self.execute_expr(callsite_effect)?;

                            if !self.ssa.attest((casym, self.memory[casym].temporal), true) {
                                return Err(Error::new(format!("callsite effect would break SSA"), vec![
                                    (expr.loc().clone(), format!("there might be conflicting constraints"))
                                ]));
                            }


                            self.copy(return_sym, return_sym_inner, loc)?;

                            self.pop();
                        }
                        self.ssa.debug("end of callsite effects");

                        self.stack = stack_original;


                        Ok(return_sym)
                    },
                    Value::Unconstrained(s) => {
                        emit_debug(format!("call expression on {} is unprovable", s), &[
                            (loc.clone(), format!("consider using an unsafe block"))
                        ]);
                        for arg in args {
                            self.execute_expr(arg)?;
                        }
                        let tmp = self.temporary(
                            format!("return value of {}", self.memory[name_sym].name),
                            self.memory[name_sym].typed.clone(),
                            loc.clone(),
                            Tags::new(),
                            )?;
                        Ok(tmp)

                    }
                    o => {
                        return Err(Error::new(format!("call expression on {} not safe", o), vec![
                            (loc.clone(), format!("call requires a function"))
                        ]));
                    }
                }
            }
            ast::Expression::Infix { lhs, rhs, loc, op} => {
                let lhs_sym = self.execute_expr(lhs)?;
                let rhs_sym = self.execute_expr(rhs)?;

                let (mut newtype, lhs_sym, rhs_sym) = self.type_coersion(lhs_sym, rhs_sym, loc)?;

                if !op.takes_boolean() && newtype.t == ast::Type::Bool{
                    return Err(Error::new(format!("invalid types for integer operator"), vec![
                        (loc.clone(), format!("not defined for type {}", newtype))
                    ]))
                } else if !op.takes_integer() && newtype.t != ast::Type::Bool{
                    return Err(Error::new(format!("invalid types for boolean operator"), vec![
                        (loc.clone(), format!("not defined for type {}", newtype))
                    ]))
                }

                /* TODO too noisy
                if let ast::Type::Other(o) = &newtype.t {
                    match format!("{}", o).as_str() {
                        "::ext::<stddef.h>::char" => (),
                        _ => if newtype.ptr.len() == 0 {
                            return Err(Error::new(format!("unprovable types for expression"), vec![
                                (loc.clone(), format!("not defined for type {}. consider casting to a builtin type", newtype))
                            ]))
                        }
                    }
                }
                */
                let signed  = newtype.t.signed();
                if op.returns_boolean() {
                    newtype = ast::Typed{
                        t:      ast::Type::Bool,
                        ptr:    Vec::new(),
                        loc:    loc.clone(),
                        tail:   ast::Tail::None,
                    };
                };


                if newtype.t.signed() && *op == crate::ast::InfixOperator::Shiftright {
                    return Err(Error::new(format!("shift right of signed value is unprovable"), vec![
                        (loc.clone(), format!("compiler specific behaviour is not allowed because it is not provable"))
                    ]))
                }

                let tmp = self.temporary(format!("infix expression"),
                    newtype.clone(),
                    loc.clone(),
                    Tags::new(),
                )?;


                // pointer arithmetic
                if newtype.ptr.len() > 0 {
                    self.ssa.debug("begin pointer arithmetic");
                    let len_of_lhs = self.temporary(format!("len({})", self.memory[lhs_sym].name),
                        ast::Typed{
                            t:      ast::Type::USize,
                            ptr:    Vec::new(),
                            loc:    loc.clone(),
                            tail:   ast::Tail::None,
                        },
                        loc.clone(),
                        Tags::new(),
                    )?;
                    let lensym = self.builtin.get("len").expect("ICE: len theory not built in");
                    self.ssa.invocation(*lensym, vec![(lhs_sym, self.memory[lhs_sym].temporal)], (len_of_lhs, 0));



                    let len_assert = self.temporary(format!("{} < len({})", self.memory[rhs_sym].name, self.memory[lhs_sym].name),
                        ast::Typed{
                            t:      ast::Type::Bool,
                            ptr:    Vec::new(),
                            loc:    loc.clone(),
                            tail:   ast::Tail::None,
                        },
                        loc.clone(),
                        Tags::new(),
                    )?;
                    self.memory[len_assert].value = Value::InfixOp {
                        lhs:    (rhs_sym, self.memory[rhs_sym].temporal),
                        rhs:    (len_of_lhs, self.memory[len_of_lhs].temporal),
                        op:     ast::InfixOperator::Lessthan,
                    };
                    self.ssa.infix_op(
                        len_assert,
                        (rhs_sym, self.memory[rhs_sym].temporal),
                        (len_of_lhs, 0),
                        ast::InfixOperator::Lessthan,
                        smt::Type::Bool,
                        false,
                    );
                    self.ssa.debug("assert that length less than index is true");
                    self.ssa.assert(vec![(len_assert, self.memory[len_assert].temporal)], |a, model| match a {
                        false => {
                            let mut estack = Vec::new();
                            estack.extend(self.demonstrate(model.as_ref().unwrap(), (len_assert, self.memory[len_assert].temporal), 0));
                            Err(Error::new(format!("possible out of bounds pointer arithmetic"), estack))
                        }
                        true => {
                            Ok(())
                        }
                    })?;
                    self.ssa_mark_safe(tmp, loc)?;

                    let len_of_opresult = self.temporary(format!("len({})", self.memory[lhs_sym].name),
                        ast::Typed{
                            t:      ast::Type::USize,
                            ptr:    Vec::new(),
                            loc:    loc.clone(),
                            tail:   ast::Tail::None,
                        },
                        loc.clone(),
                        Tags::new(),
                    )?;
                    let lensym = self.builtin.get("len").expect("ICE: len theory not built in");
                    self.ssa.invocation(*lensym, vec![(tmp, self.memory[tmp].temporal)], (len_of_opresult, 0));


                    let opposite_op =  match op {
                        ast::InfixOperator::Add      => ast::InfixOperator::Subtract,

                        //TODO need to check for wrap
                        //ast::InfixOperator::Subtract => ast::InfixOperator::Add,
                        _ => {
                            return Err(Error::new(format!("unprovable pointer arithmetic"), vec![
                                (expr.loc().clone(), format!("only + is possible"))
                            ]));
                        }
                    };

                    self.ssa.infix_op(
                        len_of_opresult,
                        (len_of_lhs, self.memory[len_of_lhs].temporal),
                        (rhs_sym, self.memory[rhs_sym].temporal),
                        opposite_op,
                        self.memory[tmp].t.clone(),
                        signed,
                    );


                    let mut value = Value::Unconstrained("pointer arithmetic".into());
                    if let Value::Array{..} = &self.memory[lhs_sym].value {
                        if let Some(nuval) = self.ssa.value((len_of_opresult, self.memory[len_of_opresult].temporal), |a,_|match a{
                            smt::Assertion::Constrained(val) => {
                                let mut nuarray = HashMap::new();
                                Some(Value::Array{len: val as usize, array: nuarray})
                            }
                            _ => None,
                        }) {
                            value = nuval;
                        }
                    };
                    self.memory[tmp].value = value;

                    return Ok(tmp);
                }



                let value = Value::InfixOp {
                    lhs:    (lhs_sym, self.memory[lhs_sym].temporal),
                    rhs:    (rhs_sym, self.memory[rhs_sym].temporal),
                    op:     op.clone(),
                };
                self.memory[tmp].value = value;


                self.ssa.infix_op(
                    tmp,
                    (lhs_sym, self.memory[lhs_sym].temporal),
                    (rhs_sym, self.memory[rhs_sym].temporal),
                    op.clone(),
                    self.memory[tmp].t.clone(),
                    signed,
                );
                Ok(tmp)
            }
            ast::Expression::Cast { expr, into, loc } => {
                let rhs = self.execute_expr(expr)?;
                let tmp = self.temporary(format!("cast of {}", self.memory[rhs].name),
                    into.clone(),
                    loc.clone(),
                    Tags::new(),
                )?;

                self.memory[tmp].value = self.memory[rhs].value.clone();
                self.ssa.assign(
                    (tmp, self.memory[tmp].temporal),
                    (rhs, self.memory[rhs].temporal),
                    Self::smt_type(into),
                );
                Ok(tmp)
            }
            ast::Expression::UnaryPost {expr, op, loc} => {
                let lhs_sym = self.execute_expr(expr)?;

                let tmp = self.temporary(format!("previous value of {}", self.memory[lhs_sym].name),
                self.memory[lhs_sym].typed.clone(),
                loc.clone(),
                self.memory[lhs_sym].tags.clone(),
                )?;

                if self.in_loop {
                    self.memory[tmp].temporal += 1;
                } else {
                    self.copy(tmp, lhs_sym, loc)?;
                }

                let value = Value::PostfixOp {
                    lhs:    (tmp, self.memory[tmp].temporal),
                    op:     op.clone(),
                };
                self.memory[lhs_sym].value = value;

                self.memory[lhs_sym].temporal += 1;
                let tt = self.memory[lhs_sym].temporal;
                self.memory[lhs_sym].assignments.insert(tt, expr.loc().clone());



                self.ssa.postfix_op(
                    (lhs_sym, self.memory[lhs_sym].temporal),
                    (tmp, self.memory[tmp].temporal),
                    op.clone(),
                    self.memory[lhs_sym].t.clone(),
                );


                Ok(tmp)
            }
            ast::Expression::UnaryPre {expr, op, loc }=> {
                match op {
                    ast::PrefixOperator::Deref => {
                        let lhs_sym = self.execute_expr(expr)?;
                        Ok(self.deref(lhs_sym, loc)?)
                    }
                    ast::PrefixOperator::AddressOf => {
                        let lhs_sym = self.execute_expr(expr)?;
                        let mut typed =  self.memory[lhs_sym].typed.clone();
                        typed.ptr.push(ast::Pointer{
                            tags:   self.memory[lhs_sym].tags.clone(),
                            loc:    loc.clone(),
                        });
                        let tmp = self.temporary(
                            format!("addressof({})", self.memory[lhs_sym].name),
                            typed,
                            loc.clone(),
                            ast::Tags::new(),
                        )?;
                        self.memory[tmp].value = Value::Address(lhs_sym);
                        self.len_into_ssa(tmp, loc, 1)?;

                        // TODO this is semi wrong
                        // it must be coming from a virtual stack
                        // because if we want to prove pointer arithmetic
                        // this value is meaningless
                        self.ssa.literal(tmp, lhs_sym as u64, smt::Type::Unsigned(64));

                        self.ssa_mark_safe(tmp, loc)?;
                        Ok(tmp)
                    }
                    crate::ast::PrefixOperator::Boolnot | crate::ast::PrefixOperator::Bitnot => {
                        let rhs_sym = self.execute_expr(expr)?;


                        if *op == crate::ast::PrefixOperator::Boolnot {
                            if self.memory[rhs_sym].t != smt::Type::Bool {
                                return Err(Error::new(format!("expected boolean, got {}", self.memory[rhs_sym].typed), vec![
                                    (expr.loc().clone(), format!("coercion to boolean is difficult to prove"))
                                ]));
                            }
                        } else if *op == crate::ast::PrefixOperator::Bitnot {
                            if self.memory[rhs_sym].t == smt::Type::Bool {
                                return Err(Error::new(format!("expected integer , got {}", self.memory[rhs_sym].typed), vec![
                                    (expr.loc().clone(), format!("invalid operand on boolean"))
                                ]));
                            }
                        }
                        let tmp = self.temporary(
                            format!("unary expression"),
                            self.memory[rhs_sym].typed.clone(),
                            loc.clone(),
                            self.memory[rhs_sym].tags.clone(),
                            )?;
                        let value = Value::PrefixOp {
                            rhs:    (rhs_sym, self.memory[rhs_sym].temporal),
                            op:     op.clone(),
                        };
                        self.memory[tmp].value = value;

                        self.ssa.prefix_op(
                            (tmp, self.memory[tmp].temporal),
                            (rhs_sym, self.memory[rhs_sym].temporal),
                            op.clone(),
                            self.memory[rhs_sym].t.clone(),
                        );

                        Ok(tmp)
                    }
                    crate::ast::PrefixOperator::Increment | crate::ast::PrefixOperator::Decrement => {
                        let rhs_sym = self.execute_expr(expr)?;
                        if self.memory[rhs_sym].t == smt::Type::Bool {
                            return Err(Error::new(format!("expected integer, got {}", self.memory[rhs_sym].typed), vec![
                                (expr.loc().clone(), format!("invalid operand on boolean"))
                            ]));
                        }
                        let tmp = self.temporary(
                            format!("copy of {} before unary", self.memory[rhs_sym].name),
                            self.memory[rhs_sym].typed.clone(),
                            loc.clone(),
                            Tags::new(),
                        )?;
                        self.copy(tmp, rhs_sym, loc)?;


                        let value = Value::PrefixOp {
                            rhs:    (rhs_sym, self.memory[rhs_sym].temporal),
                            op:     op.clone(),
                        };
                        self.memory[rhs_sym].value = value;
                        self.ssa.prefix_op(
                            (rhs_sym, self.memory[rhs_sym].temporal + 1),
                            (rhs_sym, self.memory[rhs_sym].temporal),
                            op.clone(),
                            self.memory[rhs_sym].t.clone(),
                        );

                        Ok(tmp)

                    }
                }
            }
            ast::Expression::StructInit {loc, typed, fields} => {
                let aptr = self.alloc(Name::from(&format!("literal struct {}", self.memory.len())),
                    typed.clone(),
                    loc.clone(),
                    Tags::new()
                )?;
                let mut members = HashMap::new();
                for (name, expr) in fields.iter_mut() {
                    let to = self.execute_expr(expr)?;
                    members.insert(name.clone(), to);
                }

                self.memory[aptr].value = Value::Struct{
                    members,
                };

                Ok(aptr)
            }
            ast::Expression::ArrayInit {fields, loc} => {
                if fields.len() < 1 {
                    return Err(Error::new(format!("empty literal array not possible"), vec![
                        (loc.clone(), format!("here"))
                    ]));
                }

                let mut array = HashMap::new();
                for (i, expr) in fields.iter_mut().enumerate() {
                    let to = self.execute_expr(expr)?;
                    array.insert(i, to);
                }

                let mut typed = self.memory[array[&0]].typed.clone();
                typed.ptr.push(ast::Pointer{
                    loc:  loc.clone(),
                    tags: ast::Tags::new(),
                });
                let aptr = self.alloc(
                    Name::from(&format!("literal array {}", self.memory.len())),
                    typed,
                    loc.clone(),
                    Tags::new()
                )?;

                self.memory[aptr].value = Value::Array{
                    len:    array.len(),
                    array,
                };
                self.ssa_mark_safe(aptr, loc)?;

                Ok(aptr)
            }
        }
    }



    // a pointer value has been borrowed, so everything it points to might have been tampered with
    fn borrow_away(&mut self, sym: Symbol) {
        match &self.memory[sym].value.clone() {
            Value::Array{array,..} => {
                for (_,s2) in array.clone() {
                    self.borrow_away(s2);
                }
            }
            Value::Address(to) => {
                self.ssa.debug(&format!("{} to temporal +1 because of function borrow", to));
                self.memory[*to].temporal += 1;
                //self.memory[*to].value     = Value::Unconstrained("borrowed in function call".to_string());

                // but if the current branch never happens, the  alue remains the same
                self.ssa.assign_branch(
                    (*to, self.memory[*to].temporal),
                    (*to, self.memory[*to].temporal),
                    (*to, self.memory[*to].temporal -1),
                    self.memory[*to].t.clone(),
                );

            }
            _ => ()
        }
    }


    fn deref(&mut self, lhs_sym: Symbol, loc: &ast::Location) -> Result<Symbol, Error> {

        if let Value::Address(to) = self.memory[lhs_sym].value.clone() {
            return Ok(to);
        }

        let mut nutype = self.memory[lhs_sym].typed.clone();
        let popped_tags = match nutype.ptr.pop() {
            Some(v) => v.tags.clone(),
            None => {
                // this can happen when we deref an fntype
                Tags::new()
            }
        };

        let member = self.temporary(
            format!("deref(S{}_{})", lhs_sym, self.memory[lhs_sym].name),
            nutype,
            loc.clone(),
            popped_tags,
        )?;


        if self.memory[lhs_sym].t != smt::Type::Unsigned(64) {
            return Err(Error::new(format!("cannot prove memory access due to unexpected type"), vec![
                (loc.clone(), format!("deref expression appears to be not a pointer or array"))
            ]))
        }


        // this is because the optimization cheat in check check_function_model doesn't apply
        // anything in between model calls
        if self.in_model {
            return Ok(member);
        }

        self.ssa.debug("begin safe ptr check");
        let tmp1 = self.temporary(
            format!("safe({})", self.memory[lhs_sym].name),
            ast::Typed{
                t:      ast::Type::Bool,
                ptr:    Vec::new(),
                loc:    loc.clone(),
                tail:   ast::Tail::None,
            },
            loc.clone(),
            Tags::new(),
        )?;
        let theosym = self.builtin.get("safe").expect("ICE: safe theory not built in");
        self.ssa.invocation(*theosym, vec![(lhs_sym, self.memory[lhs_sym].temporal)], (tmp1, 0));

        self.ssa.assert(vec![(tmp1, self.memory[tmp1].temporal)], |a,model| match a {
            false  => {
                let mut estack = vec![(loc.clone(),
                format!("you may need an if condition or callsite_assert to prove it is safe"))];
                estack.extend(self.demonstrate(model.as_ref().unwrap(), (tmp1, self.memory[tmp1].temporal), 0));
                Err(Error::new(format!("deref of unsafe pointer"), estack))
            }
            true => {
                Ok(())
            }
        })?;



        match &mut self.memory[lhs_sym].value {
            // we're assuming this is ok because odf the above satefy checks
            Value::Uninitialized | Value::Unconstrained(_) => {
                self.memory[lhs_sym].value = Value::Address(member);
            },
            o => {
                return Err(Error::new(format!("deref of {} is not possible", o), vec![
                    (loc.clone(), format!("this must be a pointer"))
                ]))
            }
        }

        Ok(member)

    }


    fn smt_type(t: &ast::Typed) ->  crate::smt::Type {
        if t.ptr.len() > 0 {
            return crate::smt::Type::Unsigned(64);
        }
        match t.t {
            ast::Type::Bool     => crate::smt::Type::Bool,
            ast::Type::Other(_) => crate::smt::Type::Unsigned(64),
            ast::Type::U8       => crate::smt::Type::Unsigned(8),
            ast::Type::U16      => crate::smt::Type::Unsigned(16),
            ast::Type::U32      => crate::smt::Type::Unsigned(32),
            ast::Type::U64      => crate::smt::Type::Unsigned(64),
            ast::Type::U128     => crate::smt::Type::Unsigned(128),
            ast::Type::I8       => crate::smt::Type::Signed(8),
            ast::Type::I16      => crate::smt::Type::Signed(16),
            ast::Type::I32      => crate::smt::Type::Signed(32),
            ast::Type::I64      => crate::smt::Type::Signed(64),
            ast::Type::I128     => crate::smt::Type::Signed(128),

            ast::Type::UInt     => crate::smt::Type::Unsigned(64),
            ast::Type::Int      => crate::smt::Type::Signed(64),

            ast::Type::USize    => crate::smt::Type::Unsigned(64),
            ast::Type::ISize    => crate::smt::Type::Signed(64),

            ast::Type::F64      => crate::smt::Type::Unsigned(64),
            ast::Type::F32      => crate::smt::Type::Unsigned(64),


            // these are actually jist pollution in smt. they're casted before use
            ast::Type::ULiteral => crate::smt::Type::Unsigned(64),
            ast::Type::ILiteral => crate::smt::Type::Signed(64),
        }
    }

    // new stack variable
    fn alloc(&mut self, name: Name, typed: ast::Typed, loc: ast::Location, tags: ast::Tags) -> Result<Symbol, Error> {
        self.ssa.debug_loc(&loc);

        if let Some(prev) = self.cur().locals.get(&name).cloned() {
            return Err(Error::new(format!("ICE: redeclation of local name '{}' should have failed in expand", name), vec![
                (loc.clone(), "this declaration would shadow a previous name".to_string()),
                (self.memory[prev].declared.clone(), "previous declaration".to_string())
            ]));
        }

        let t = Self::smt_type(&typed);
        let symbol = self.memory.len();
        self.memory.push(Storage{
            typed:      typed.clone(),
            t:          t.clone(),
            name:       name.clone(),
            declared:   loc.clone(),
            value:      Value::Uninitialized,
            tags,
            temporal:   0,
            assignments: HashMap::new(),
            borrows:    Vec::new(),
        });
        debug!("{} := {}", name, symbol);
        self.cur().locals.insert(name.clone(), symbol);
        self.ssa.declare(symbol, &format!("{}", name), t);


        if let ast::Type::Other(name) = &typed.t {
            if let Some(def) = self.defs.get(name) {
                if let ast::Def::Struct{..} = def {
                }
            }
        }



        Ok(symbol)
    }


    fn tail_into_ssa(&mut self, sym: Symbol, loc: &ast::Location) -> Result<(), Error> {
        self.ssa.debug_loc(loc);
        let tailval = match self.memory[sym].typed.tail.clone() {
            ast::Tail::None     => return Ok(()),
            ast::Tail::Dynamic  => {
                return Err(Error::new(format!("tail size must be known for stack variables"), vec![
                    (self.memory[sym].typed.loc.clone(), format!("cannot use dynamic tail size here"))
                ]));
            },
            ast::Tail::Static(val,_)   => {
                val
            },
            ast::Tail::Bind(name, loc)   => {
                self.ssa.debug_loc(&loc);
                self.ssa.debug("tail_into_ssa");
                let sym = self.name(&Name::from(&name), &loc)?;
                let val = self.ssa.value((sym, self.memory[sym].temporal), |a,model|match a{
                    smt::Assertion::Unsolveable => {
                        Err(Error::new(format!("tail size is not solveable"), vec![
                            (loc.clone(), format!("there may be conflicting constraints"))
                        ]))
                    }
                    smt::Assertion::Unconstrained(_) => {
                        let mut estack = vec![(loc.clone(),
                        format!("you may need an if condition or callsite_assert to increase confidence"))];
                        estack.extend(self.demonstrate(model.as_ref().unwrap(), (sym, self.memory[sym].temporal), 0));
                        Err(Error::new(format!("tail size is unconstrained"), estack))
                    }
                    smt::Assertion::Constrained(val) => {
                        Ok(val)
                    }
                })?;
                val
            }
        };

        self.ssa.debug_loc(loc);

        let field_name = match &self.memory[sym].typed.t {
            ast::Type::Other(n) => match self.defs.get(n) {
                Some(ast::Def::Struct{fields, ..}) => {
                    if fields.len() < 1 {
                        return Err(Error::new(format!("tail binding on struct with no members"), vec![
                            (loc.clone(), format!("this struct must have members"))
                        ]));
                    }
                    fields.last().unwrap().name.clone()
                },
                _ => {
                    return Err(Error::new(format!("tail value on non struct"), vec![
                        (loc.clone(), format!("cannot use tail binding"))
                    ]));
                }
            },
            _ => {
                return Err(Error::new(format!("tail value on non struct"), vec![
                    (loc.clone(), format!("cannot use tail binding"))
                ]));
            }
        };

        let member_sym = self.member_access(sym, &field_name, loc)?;
        self.len_into_ssa(member_sym, loc, tailval as usize)?;


        Ok(())
    }


    fn temporary(&mut self, name: String, typed: ast::Typed, loc: ast::Location, tags: ast::Tags) -> Result<Symbol, Error> {
        self.ssa.debug_loc(&loc);
        let t = Self::smt_type(&typed);
        let symbol = self.memory.len();
        self.memory.push(Storage{
            t:          t.clone(),
            typed:      typed.clone(),
            name:       Name::from(&name),
            declared:   loc.clone(),
            value:      Value::Uninitialized,
            tags,
            temporal:   0,
            assignments: HashMap::new(),
            borrows:    Vec::new(),
        });
        debug!("{} {} := {}", name, typed, symbol);
        self.ssa.declare(symbol, &format!("{}", name), t);
        Ok(symbol)
    }


    //                  cpy here   from here
    fn copy(&mut self, lhs: Symbol, rhs: Symbol, used_here: &ast::Location) -> Result<(), Error> {

        // transfer theories of pointers
        // TODO: nah thats shitty. they should automatically transfer in smt
        if self.memory[rhs].t == smt::Type::Unsigned(64) && self.memory[lhs].t == smt::Type::Unsigned(64) {
            let tmp_safe_transfer = self.temporary(
                format!("safe({}) == safe({})", self.memory[rhs].name, self.memory[lhs].name),
                ast::Typed{
                    t:      ast::Type::Bool,
                    ptr:    Vec::new(),
                    loc:    used_here.clone(),
                    tail:   ast::Tail::None,
                },
                used_here.clone(),
                Tags::new(),
                )?;
            let theosym = self.builtin.get("safe").expect("ICE: safe theory not built in");
            self.ssa.invocation(*theosym, vec![(rhs, self.memory[rhs].temporal)], (tmp_safe_transfer, 0));
            self.ssa.invocation(*theosym, vec![(lhs, self.memory[lhs].temporal + 1)], (tmp_safe_transfer, 0));

            let tmp_nullterm_transfer = self.temporary(
                format!("nullterm({}) == nullterm({})", self.memory[rhs].name, self.memory[lhs].name),
                ast::Typed{
                    t:      ast::Type::Bool,
                    ptr:    Vec::new(),
                    loc:    used_here.clone(),
                    tail:   ast::Tail::None,
                },
                used_here.clone(),
                Tags::new(),
                )?;
            let theosym = self.builtin.get("nullterm").expect("ICE: nullterm theory not built in");
            self.ssa.invocation(*theosym, vec![(rhs, self.memory[rhs].temporal)], (tmp_nullterm_transfer, 0));
            self.ssa.invocation(*theosym, vec![(lhs, self.memory[lhs].temporal + 1)], (tmp_nullterm_transfer, 0));
        }

        self.memory[lhs].temporal += 1;
        let tt = self.memory[lhs].temporal;
        self.memory[lhs].assignments.insert(tt, used_here.clone());



        match self.memory[rhs].value.clone() {
            Value::Void => {
                return Err(Error::new(format!("void is not a value: '{}'",  self.memory[rhs].name), vec![
                    (used_here.clone(), "used here".to_string())
                ]));
            }
            Value::Uninitialized => {
                // FIXME for now this is too noisy.
                //return Err(Error::new(format!("unsafe read access to uninitialized local '{}'", self.memory[rhs].name), vec![
                //    (used_here.clone(), "used here".to_string())
                //]));
            }
            Value::Theory{..} => {
                return Err(Error::new(format!("taking the value of a theory is not a thing"), vec![
                    (used_here.clone(), "used here".to_string())
                ]));
            },
            Value::Array{array, len, ..} => {
               let mut value = self.memory[lhs].value.clone();
               match &mut value {
                   Value::Array{array: ref mut prev, len: ref mut prev_len, ..} => {
                       if *prev_len == 0 {
                           *prev_len = len;
                       } else if len > *prev_len {
                           return Err(Error::new(format!("assigning arrays of different len"), vec![
                                (used_here.clone(), format!("lhs is len {} but rhs is len {}", prev.len(), array.len()))
                           ]));
                       }


                       //initialization is defined to be complete in C
                       for i in 0..*prev_len {
                           if !prev.contains_key(&i) {
                               let mut typed = self.memory[lhs].typed.clone();
                               typed.ptr.pop();
                               let tmp = self.temporary(
                                   format!("array member {}[{}]", self.memory[lhs].name, i),
                                   typed,
                                   self.memory[lhs].declared.clone(),
                                   self.memory[lhs].tags.clone(),
                                   )?;
                               self.memory[tmp].value = Value::Integer(0);
                               prev.insert(i, tmp);
                           }
                       }

                       for i in 0..array.len() {
                           self.memory[prev[&i]].value = self.memory[array[&i]].value.clone();
                       }

                       self.len_into_ssa(lhs, used_here, prev.len())?;
                   }
                   _ => {
                       self.len_into_ssa(lhs, used_here, len)?;
                   }
               }
               self.memory[lhs].value = value;
               return Ok(());
            },
            _ => {
            },
        };

        if self.memory[lhs].typed != self.memory[rhs].typed {
            emit_debug("assignment of incompatible types", &[
                (used_here.clone(), format!("lhs is {} but rhs is {}", self.memory[lhs].typed, self.memory[rhs].typed))
            ]);
        }

        let (newtype, lhs, rhs) = self.type_coersion(lhs, rhs, used_here)?;

        if self.memory[lhs].typed.ptr.len() != self.memory[rhs].typed.ptr.len() {
           match self.memory[rhs].value {
               Value::Unconstrained(_) => (),
               Value::Integer(f) if f == 0 => (),
               _ => return Err(Error::new("assignment of incompatible pointer depth".to_string(), vec![
                  (used_here.clone(), format!("lhs is {} but rhs is {}", self.memory[lhs].typed, self.memory[rhs].typed))
                ])),
            }
        }

        if self.memory[lhs].t != self.memory[rhs].t {
            return Err(Error::new("assignment of incompatible types".into(), vec![
                (used_here.clone(), format!("lhs is {} but rhs is {}", self.memory[lhs].typed, self.memory[rhs].typed))
            ]));
        }

        self.memory[lhs].value = self.memory[rhs].value.clone();


        self.ssa.assign_branch(
            (lhs, self.memory[lhs].temporal),
            (rhs, self.memory[rhs].temporal),
            (lhs, self.memory[lhs].temporal-1),
            Self::smt_type(&newtype),
        );

        Ok(())
    }


    fn ssa_mark_valid(&mut self, sym: Symbol, loc: &ast::Location) -> Result<(), Error> {
        //TODO
        Ok(())
    }

    fn ssa_mark_safe(&mut self, sym: Symbol, loc: &ast::Location) -> Result<(), Error> {
        if self.memory[sym].t != smt::Type::Unsigned(64) {
            panic!("ICE: ssa_mark_safe on non pointer");
        }
        let tmp = self.temporary(
            format!("true"),
            ast::Typed{
                t:      ast::Type::Bool,
                ptr:    Vec::new(),
                loc:    loc.clone(),
                tail:   ast::Tail::None,
            },
            loc.clone(),
            Tags::new(),
        )?;
        let thsym = self.builtin.get("safe").expect("ICE: safe theory not built in");
        self.ssa.invocation(*thsym, vec![(sym, self.memory[sym].temporal)], (tmp, 0));
        self.ssa.literal(tmp, 1, smt::Type::Bool);
        Ok(())
    }

    fn ssa_mark_nullterm(&mut self, sym: Symbol, loc: &ast::Location) -> Result<(), Error> {
        if self.memory[sym].t != smt::Type::Unsigned(64) {
            panic!("ICE: nullterm on non pointer");
        }
        let tmp = self.temporary(
            format!("true"),
            ast::Typed{
                t:      ast::Type::Bool,
                ptr:    Vec::new(),
                loc:    loc.clone(),
                tail:   ast::Tail::None,
            },
            loc.clone(),
            Tags::new(),
        )?;
        let thsym = self.builtin.get("nullterm").expect("ICE: nullterm theory not built in");
        self.ssa.invocation(*thsym, vec![(sym, self.memory[sym].temporal)], (tmp,0));
        self.ssa.literal(tmp, 1, smt::Type::Bool);
        Ok(())
    }

    fn len_into_ssa(&mut self, sym: Symbol, loc: &ast::Location, len: usize) -> Result<(), Error> {
        let tmp = self.temporary(
            format!("len({})", self.memory[sym].name),
            ast::Typed{
                t:      ast::Type::USize,
                ptr:    Vec::new(),
                loc:    loc.clone(),
                tail:   ast::Tail::None,
            },
            loc.clone(),
            Tags::new(),
        )?;
        let lensym = self.builtin.get("len").expect("ICE: len theory not built in");
        self.ssa.invocation(*lensym, vec![(sym, self.memory[sym].temporal)], (tmp, 0));
        self.ssa.literal(tmp, len as u64, smt::Type::Unsigned(64));
        Ok(())
    }

    fn name(&mut self, name: &Name, used_here: &ast::Location) -> Result<Symbol, Error> {
        for scope in self.stack.iter().rev() {
            if let Some(v) = scope.locals.get(name) {
                return Ok(*v);
            }
        }


        if name.is_absolute()
            && name.0.len() == 4
            && name.0[1].as_str() == "ext"
        {
            let sym = self.temporary(
                format!("{}", name),
                ast::Typed{
                    t:      ast::Type::Other(name.clone()),
                    ptr:    Vec::new(),
                    loc:    used_here.clone(),
                    tail:   ast::Tail::None,
                },
                used_here.clone(),
                Tags::new(),
            )?;
            self.memory[sym].value = Value::Unconstrained(format!("c name {}", name));

            self.ssa.check_ded(sym, used_here);

            return Ok(sym);
        }

        return Err(Error::new(format!("undefined symbol '{}'", name), vec![
            (used_here.clone(), format!("'{}' is not defined in this scope", name))
        ]));
    }

    fn new(module_name: &Name) -> Self {
        Symbolic {
            stack:  vec![
                Scope {
                    name:   "global".to_string(),
                    locals: Default::default(),
                    trace:  Vec::new(),
                }
            ],
            memory:  Default::default(),
            ssa:     Solver::new(module_name.to_string()),
            builtin: Default::default(),
            defs:    HashMap::new(),
            current_module_name:    module_name.human_name(),
            current_function_name:  String::new(),
            current_function_ret:   None,
            current_function_model: Vec::new(),
            in_loop: false,
            in_model:false,
        }
    }

    fn push(&mut self, name: String) {
        debug!("  scope {}", name);
        self.stack.push(Scope{
            name,
            locals: HashMap::new(),
            trace:  Vec::new(),
        });
    }

    fn pop(&mut self) {
        self.stack.pop();
    }

    fn cur(&mut self) -> &mut Scope {
        self.stack.last_mut().unwrap()
    }


    fn literal(&mut self, loc: &ast::Location, value: Value, t: ast::Typed) -> Result<Symbol, Error> {
        self.ssa.debug_loc(loc);
        match value {
            Value::Integer(v) => {
                let sym = self.temporary(
                    format!("literal {}", v),
                    t.clone(),
                    loc.clone(),
                    Tags::new(),
                )?;
                self.memory[sym].value = value;
                self.ssa.literal(sym, v as u64, self.memory[sym].t.clone());
                self.ssa_mark_valid(sym, loc)?;
                Ok(sym)
            },
            _ => {
                let sym = self.alloc(
                    Name::from(&format!("literal {}", self.memory.len())),
                    t.clone(),
                    loc.clone(),
                    Tags::new(),
                )?;
                self.memory[sym].value = value;
                self.ssa_mark_valid(sym, loc)?;
                Ok(sym)
            }
        }
    }

    fn demonstrate(&self, model: &smt::ModelRef, sym: TemporalSymbol, depth: usize) -> Vec<(ast::Location, String)> {
        let mut estack  = Vec::new();


        let valloc = match self.memory[sym.0].assignments.get(&sym.1) {
            Some(loc) => loc.clone(),
            None => self.memory[sym.0].declared.clone(),
        };

        if depth > 5 {
            estack.push((valloc, "this expression is too complex to drill down further".to_string()));
            return estack;
        }

        match &self.memory[sym.0].value {
            Value::InfixOp{lhs, rhs, op} => {
                let overflow = self.ssa.infix_op_will_wrap(
                    *lhs,
                    *rhs,
                    op.clone(),
                    self.memory[sym.0].t.clone(),
                );

                if let Some(v) = self.ssa.extract(model, sym) {
                    let v = if self.memory[sym.0].t == smt::Type::Bool {
                        if v > 0 { "true".to_string() } else { "false".to_string() }
                    } else {
                        format!("0x{:x}", v)
                    };

                    if overflow {
                        estack.push((valloc, format!("for OVERFLOW of {} |{}| = {}", self.memory[sym.0].name, sym.1, v)));
                    } else {
                        estack.push((valloc, format!("for {} |{}| = {}", self.memory[sym.0].name, sym.1, v)));
                    }
                }

                if *lhs != sym {
                    estack.extend(self.demonstrate(model, *lhs, depth + 1));
                }
                if *rhs != sym {
                    estack.extend(self.demonstrate(model, *rhs, depth + 1));
                }
            },
            //Value::Integer(_) => return estack,
            _ => {
                if let Some(v) = self.ssa.extract(model, sym) {
                    let v = if self.memory[sym.0].t == smt::Type::Bool {
                        if v > 0 { "true".into() } else { "false".into() }
                    } else {
                        format!("0x{:x}", v)
                    };
                    estack.push((valloc, format!("for {} |{}| = {}", self.memory[sym.0].name, sym.1, v)));
                } else {
                    estack.push((valloc, format!("for {} |{}| = ??", self.memory[sym.0].temporal, sym.1)));
                }
            }
        };


        if depth == 0 {
            for stack in self.stack.iter().rev() {
                for (sym, loc, onlyiftrue) in &stack.trace {
                    match self.ssa.extract(model, *sym) {
                        Some(f) if f > 0 => {
                            estack.push((loc.clone(), format!("reached because this branch condition was true")));
                            estack.extend(self.demonstrate(model, *sym, depth + 1));
                        },
                        Some(_)  => {
                            if !onlyiftrue {
                                estack.push((loc.clone(), format!("reached because this branch condition was false")));
                                estack.extend(self.demonstrate(model, *sym, depth + 1));
                            }
                        },
                        None => {}
                    };
                }
            }
        }
        estack
    }


    fn stmname(&self, sym: Symbol) -> String {
        let name =
            self.memory[sym].name.to_string()
            .replace(|c: char| !c.is_ascii_alphanumeric(), "_");

        format!("{}_{}", sym, name)
    }



}


pub fn execute(module: &mut flatten::Module) -> bool {
    use rayon::prelude::*;

    let mut defs        = Vec::new();
    let mut function_at = Vec::new();
    for (i, (d,_,defined_here)) in module.d.clone().into_iter().enumerate() {
        if let ast::Def::Function{..} = d.def {
            if defined_here {
                function_at.push((i, d.name.clone(), module.clone()));
            }
        }
        defs.push(d.clone());
    }



    // execute one in serial on the borrowed module to get modifications to globals
    if let Some((at, name, _)) = function_at.pop() {
        let mut sym = Symbolic::new(&Name::from(&name));
        if let Err(e) = sym.execute_module(module, at) {
            parser::emit_error(e.message.clone(), &e.details);
            return false;
        }
    }

    let repl = function_at.into_par_iter().map(|(at, name, mut module)|{
        let mut sym = Symbolic::new(&Name::from(&name));
        match sym.execute_module(&mut module, at) {
            Err(e) => {
                parser::emit_error(e.message.clone(), &e.details);
                None
            }
            Ok(_)  => {
                Some((at, module.d.remove(at).0))
            }
        }
    }).collect::<Vec<Option<(usize, ast::Local)>>>();

    for r in repl {
        if let Some((at,l)) = r {
            module.d[at].0 = l;
        } else {
            return false;
        }
    }

    true
}
