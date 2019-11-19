use crate::flatten;
use crate::ast;
use crate::name::Name;
use std::collections::HashMap;
use super::parser::{self, emit_error, emit_warn, emit_debug};
use ast::Tags;
use crate::smt::{Solver, self};


pub enum Error {
    Error {
        message:    String,
        details:    Vec<(ast::Location, String)>,
    },
}

impl Error {
    pub fn new(message: String, details:    Vec<(ast::Location, String)>) -> Self {
        Self::Error {
            message,
            details,
        }
    }
}


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
        lhs:    TemporalSymbol,
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
        args:   Vec<ast::NamedArg>,
        vararg: bool,
        ret:    Option<ast::Typed>,
        callsite_assert: Vec<ast::Expression>,
        callsite_effect: Vec<ast::Expression>,
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
}

struct Scope{
    name:   String,
    locals: HashMap<Name, usize>,
    trace:  Vec<TemporalSymbol>,
}

pub struct Symbolic {
    stack:      Vec<Scope>,
    memory:     Vec<Storage>,
    ssa:        Solver,
    builtin:    HashMap<String, Symbol>,
    defs:       HashMap<Name, ast::Def>,
    current_function_name: String,
}

impl Symbolic {
    fn execute_module(&mut self, module: &mut flatten::Module) -> Result<(), Error> {

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

        for (d,_,_) in &mut module.d {
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

                    // safe is implicit unless the arg is marked unsafe
                    for farg in args.iter() {
                        if farg.typed.ptr.len() > 0 {
                            if !farg.tags.contains("unsafe") {
                                let loc = farg.typed.ptr[0].loc.clone();
                                let ast_safe = ast::Expression::Name(ast::Typed{
                                    t:      ast::Type::Other(Name::from("safe")),
                                    ptr:    Vec::new(),
                                    loc:    loc.clone(),
                                    tail:   ast::Tail::None,
                                });
                                let ast_argname = ast::Expression::Name(ast::Typed{
                                    t:      ast::Type::Other(Name::from(&farg.name)),
                                    ptr:    Vec::new(),
                                    loc:    loc.clone(),
                                    tail:   ast::Tail::None,
                                });
                                let ast_call = ast::Expression::Call{
                                    loc:    loc.clone(),
                                    name:   Box::new(ast_safe),
                                    args:   vec![Box::new(ast_argname)],
                                    expanded:   true,
                                    emit:       ast::EmitBehaviour::Default,
                                };
                                callassert.insert(0, ast_call.clone());


                                //TODO this is very confusing, see tests/mustpass/callsite_safe_effect
                                //calleffect.insert(0, ast_call);
                            }
                        }
                    }


                    self.memory[sym].value = Value::Function {
                        args:   args.clone(),
                        vararg: *vararg,
                        ret:    ret.as_ref().map(|r|r.typed.clone()),
                        callsite_assert: callassert.clone(),
                        callsite_effect: calleffect.clone(),
                    };
                    self.ssa_mark_safe(sym, &d.loc)?;

                    self.execute_function(&d.name, args, body, callassert, calleffect)?;
                    if !self.ssa.solve() {
                        return Err(Error::new(format!("function is unprovable"), vec![
                            (d.loc.clone(), format!("this function body is impossible to prove"))
                        ]));
                    }
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
                ast::Def::Fntype {ret,args,attr,vararg} => {
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
                ast::Def::Struct {fields, packed, tail } => {
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
                ast::Def::Testcase {..} => {
                },
            }
        }
        Ok(())
    }

    fn execute_function(
        &mut self,
        name: &String,
        args: &Vec<ast::NamedArg>,
        body: &mut ast::Block,
        callassert: &mut Vec<ast::Expression>,
        calleffect: &mut Vec<ast::Expression>,
    ) -> Result<(), Error> {

        self.push(format!("function {}", name));
        self.ssa.push(&format!("\n\n;start of function {}", name));
        self.current_function_name = name.clone();

        let mut prev : Option<Symbol> =  None;
        for i in 0..args.len() {
            let argname = Name::from(&args[i].name);
            let sym = self.alloc(argname.clone(), args[i].typed.clone(), args[i].loc.clone(), args[i].tags.clone())?;
            self.memory[sym].value = Value::Unconstrained(format!("passed by value as {}", argname));

            if args[i].tags.contains("tail") {
                let prev = prev.expect("ICE: tail tag without previous arg");

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
                    format!("*{}", self.memory[prev].name),
                    nutype.clone(),
                    args[i].loc.clone(),
                    Tags::new(),
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
                self.ssa.invocation(*lensym, vec![(member_sym, self.memory[member_sym].temporal)], tmp);
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
            if !self.ssa.constrain((sym, self.memory[sym].temporal), true) {
                return Err(Error::new(format!("callsite assert broke ssa solution"), vec![
                    (callassert.loc().clone(), format!("there may be conflicting constrains"))
                ]));
            }
        }

        self.execute_scope(&mut (body.statements.iter_mut().map(|v|v).collect::<Vec<&mut ast::Statement>>()))?;

        for callsite_effect in calleffect {
            let casym = self.execute_expr(callsite_effect)?;
            self.ssa.assert((casym, self.memory[casym].temporal), |a,model| match a {
                false  => {
                    let mut estack = vec![
                        (callsite_effect.loc().clone(), format!("function must behave like this model")),
                    ];
                    estack.extend(self.demonstrate(model.as_ref().unwrap(), (casym, self.memory[casym].temporal), 0));
                    Err(Error::new(format!("unproven model"), estack))
                }
                true => {
                    Ok(())
                }
            })?;
        };

        self.ssa.pop(&format!("end of function {}\n\n", name));
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

        // if one is an unsigned literal, coerce it into the other type
        if self.memory[a].typed.t == ast::Type::ULiteral {
            let tmp = self.literal(
                here,
                self.memory[a].value.clone(),
                self.memory[b].typed.clone(),
            )?;
            return Ok((self.memory[b].typed.clone(), tmp, b));
        }

        if self.memory[b].typed.t == ast::Type::ULiteral {
            let tmp = self.literal(
                here,
                self.memory[b].value.clone(),
                self.memory[a].typed.clone(),
            )?;
            return Ok((self.memory[a].typed.clone(), a, tmp));
        }


        // TODO if the lhs is a pointer, do an implicit cast
        if self.memory[a].typed.ptr.len() > 0 {
            let tmp = self.temporary(
                format!("implicit cast of {}", self.memory[b].name),
                self.memory[a].typed.clone(),
                here.clone(),
                Tags::new(),
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

        return Err(Error::new(format!("incompatible types {} and {}", self.memory[a].typed, self.memory[b].typed), vec![
            (here.clone(), format!("this expression is unprovable over incompatible types")),
            (self.memory[a].declared.clone(), format!("{} := {} {:?}", self.memory[a].name, self.memory[a].typed, self.memory[a].value)),
            (self.memory[b].declared.clone(), format!("{} := {} {:?}", self.memory[b].name, self.memory[b].typed, self.memory[a].value))
        ]));
    }


    fn execute_scope(&mut self, body: &mut [&mut ast::Statement]) -> Result<(), Error> {
        for i in 0..body.len() {
            let (body, rest) = body.split_at_mut(i + 1);

            // this gets modified in place. must not do that for each branch
            let mut rest = rest.iter().map(|stm|(*stm).clone()).collect::<Vec<ast::Statement>>();

            match &mut body[i] {
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
                    let mut expanded = Vec::new();
                    let mut negative_stack : Vec<(ast::Location, ast::Expression)> = Vec::new();


                    // create branch expansions
                    let freeze = self.memory.clone();
                    self.push("branch".to_string());

                    for (loc, expr, body2) in branches {
                        expanded.push((negative_stack.clone(), expr.clone(), loc.clone(), body2));
                        if let Some(expr) = expr {


                            // we have to execute all conditionals once, because there might be translations
                            self.ssa.push("dummy execution");
                            self.execute_expr(expr)?;
                            self.ssa.pop("end of dummy");


                            negative_stack.push((loc.clone(), expr.clone()));
                        }
                    }

                    self.pop();
                    self.memory = freeze;

                    // execute each expansion with all negative preconditions
                    for (mut negative, mut positive, branchloc, body2) in expanded {

                        let positive_constrain = if let Some(expr) = &mut positive {
                            let sym = self.execute_expr(expr)?;

                            if self.memory[sym].typed.t != ast::Type::Bool {
                                return Err(Error::new(format!("expected boolean, got {}", self.memory[sym].typed), vec![
                                    (expr.loc().clone(), format!("coercion to boolean is difficult to prove"))
                                ]));
                            }

                            let sym = (sym, self.memory[sym].temporal);
                            /*
                            self.ssa.assert(sym, |a,model|match a {
                                smt::Assertion::Unsolveable => {
                                    Err(Error::new(format!("condition is not solveable"), vec![
                                        (expr.loc().clone(), format!("there may be conflicting constraints"))
                                    ]))
                                }
                                smt::Assertion::Unconstrained(_) => {Ok(())}
                                smt::Assertion::Constrained(true) => {
                                    let mut estack = vec![(expr.loc().clone(), format!("condition can never be false"))];
                                    estack.extend(self.demonstrate(&model.as_ref().unwrap(), sym, 0));
                                    emit_warn("unnecessary conditional", &estack);
                                    Ok(())
                                }
                                smt::Assertion::Constrained(false) => {
                                    let mut estack = vec![(expr.loc().clone(), format!("condition can never be true"))];
                                    estack.extend(self.demonstrate(&model.as_ref().unwrap(), sym, 0));
                                    emit_warn("unnecessary conditional", &estack);
                                    Ok(())
                                }
                            })?;
                            */

                            self.cur().trace.push(sym);
                            self.ssa.debug_loc(&expr.loc());

                            Some((sym, expr.loc().clone()))
                        } else {
                            None
                        };

                        let freeze = self.memory.clone();
                        self.push("branch".to_string());

                        // exexute a branch including the conditional body
                        self.ssa.push("branch at");
                        self.ssa.debug_loc(&branchloc);
                        self.ssa.push("positive branch at");
                        self.ssa.debug_loc(&branchloc);

                        if let Some((sym, loc)) = positive_constrain {
                            if !self.ssa.constrain(sym, true) {
                                return Err(Error::new(format!("positive condition breaks ssa"), vec![
                                    (loc.clone(), format!("there may be conflicting constraints"))
                                ]));
                            }
                        }

                        // all previous expressions are therefor false
                        for (loc, expr) in &mut negative{
                            let sym = self.execute_expr(expr)?;
                            let sym = (sym, self.memory[sym].temporal);
                            if !self.ssa.constrain(sym, false) {
                                return Err(Error::new(format!("negative condition breaks ssa"), vec![
                                    (expr.loc().clone(), format!("there may be conflicting constraints")),
                                    (loc.clone(), format!("during execution of this branch"))
                                ]));
                            }
                            self.cur().trace.push(sym);
                        }



                        self.execute_scope(&mut (body2.statements.iter_mut().map(|v|v).collect::<Vec<&mut ast::Statement>>()))?;
                        self.ssa.pop("end of possitive branch at / rest following after positive branch");
                        self.ssa.debug_loc(&branchloc);
                        self.execute_scope(&mut (rest.iter_mut().map(|v|v).collect::<Vec<&mut ast::Statement>>()))?;
                        self.ssa.pop("end of branch at / rest following after negative branch");
                        self.ssa.debug_loc(&branchloc);

                        self.pop();
                        self.memory = freeze;
                    }

                    // continue execution as if no condition was met
                }
                ast::Statement::Expr{expr, ..} => {
                    self.execute_expr(expr)?;
                }
                ast::Statement::Return{loc, expr} => {
                    if let Some(expr) = expr  {
                        self.execute_expr(expr)?;
                    }
                    // stop. do not execute anything behind return
                    return Ok(());
                }
                ast::Statement::Label{..} => {
                },
                ast::Statement::Goto{..} => {
                    //TODO ... this is going to be impossible
                    unimplemented!();
                },
                ast::Statement::Mark{..} => {
                },
                ast::Statement::Switch{expr, cases, default, ..} => {
                    //TODO actually implement branch execution

                    self.push("switch".into());
                    self.execute_expr(expr)?;
                    for (conds,body) in cases {
                        for expr in conds {
                            let freeze = self.memory.clone();
                            self.ssa.debug_loc(&expr.loc());
                            self.push("case".into());
                            self.ssa.push("case");
                            self.execute_expr(expr)?;
                            self.execute_scope(&mut (body.statements.iter_mut().map(|v|v).collect::<Vec<&mut ast::Statement>>()))?;
                            self.ssa.pop("end of case");
                            self.pop();
                            self.memory = freeze;
                            self.ssa.debug_loc(&expr.loc());
                        }
                    }

                    if let Some(default) = default {
                        let freeze = self.memory.clone();
                        self.push("case".into());
                        self.ssa.push("case");
                        self.execute_scope(&mut (default.statements.iter_mut().map(|v|v).collect::<Vec<&mut ast::Statement>>()))?;
                        self.ssa.pop("end of case");
                        self.pop();
                        self.memory = freeze;
                    }

                    self.pop();
                }
                ast::Statement::Assign{loc, lhs, op, rhs} => {
                    let lhs = self.execute_expr(lhs)?;
                    let rhs = self.execute_expr(rhs)?;

                    let (newtype, lhs, rhs) = self.type_coersion(lhs, rhs, loc)?;

                    if let ast::AssignOperator::Eq = op {
                        self.copy(lhs, rhs, loc)?;
                    } else {
                        let tmp = self.temporary("assign inter".to_string(),
                            newtype.clone(),
                            loc.clone(),
                            Tags::new(),
                        )?;

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

                        self.copy(lhs, tmp, loc)?;
                    }
                }
                ast::Statement::Continue{..} => {
                }
                ast::Statement::Break{..} => {
                }
                ast::Statement::Block(block) => {
                    self.push("block".to_string());
                    self.ssa.push("block");

                    self.execute_scope(&mut (block.statements.iter_mut().map(|v|&mut*v).collect::<Vec<&mut ast::Statement>>()))?;

                    self.pop();
                    self.ssa.pop("end of block");
                }
                ast::Statement::For{e1,e2,e3,body} => {
                    self.push("for loop".to_string());
                    self.ssa.push("for loop");

                    self.execute_scope(&mut (e1.iter_mut().map(|v|&mut**v).collect::<Vec<&mut ast::Statement>>()))?;

                    let prev_inf = self.ssa.infinite;
                    self.ssa.infinite = true;
                    self.execute_scope(&mut (e3.iter_mut().map(|v|&mut**v).collect::<Vec<&mut ast::Statement>>()))?;
                    self.ssa.infinite = prev_inf;

                    if let Some(expr) = e2 {
                        let sym = self.execute_expr(expr)?;
                        if self.memory[sym].t != smt::Type::Bool {
                            return Err(Error::new(format!("expected boolean, got {}", self.memory[sym].typed), vec![
                                                  (expr.loc().clone(), format!("must be boolean"))
                            ]));
                        }
                        let sym = (sym, self.memory[sym].temporal);
                        self.cur().trace.push(sym.clone());
                        if !self.ssa.constrain(sym, true) {
                            return Err(Error::new(format!("condition breaks ssa"), vec![
                                (expr.loc().clone(), format!("there may be conflicting constraints"))
                            ]));
                        }
                    }

                    self.execute_scope(&mut (body.statements.iter_mut().map(|v|&mut*v).collect::<Vec<&mut ast::Statement>>()))?;
                    self.ssa.pop("end of for loop");
                    self.pop();
                }
                ast::Statement::While{expr, body} => {
                    self.push("while loop".to_string());
                    self.ssa.push("while loop");

                    let sym = self.execute_expr(expr)?;
                    if self.memory[sym].t != smt::Type::Bool {
                        return Err(Error::new(format!("expected boolean, got {}", self.memory[sym].typed), vec![
                            (expr.loc().clone(), format!("must be boolean"))
                        ]));
                    }
                    let sym = (sym, self.memory[sym].temporal);
                    self.cur().trace.push(sym.clone());

                    if !self.ssa.constrain(sym, true) {
                        return Err(Error::new(format!("condition breaks ssa"), vec![
                            (expr.loc().clone(), format!("there may be conflicting constraints"))
                        ]));
                    }

                    self.execute_scope(&mut (body.statements.iter_mut().map(|v|&mut*v).collect::<Vec<&mut ast::Statement>>()))?;
                    self.ssa.pop("end of while loop");
                    self.pop();
                }
                ast::Statement::Unsafe{..} => {
                }
                ast::Statement::CBlock{..} => {
                }
            }
        }
        Ok(())
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
                let mut prev = called.get_mut(i-1).expect("ICE: tail tag without previous arg");
                let callptr = self.execute_expr(&mut prev)?;

                let prev_loc = prev.loc().clone();
                *prev = Box::new(ast::Expression::Cast {
                    into: defined[i-1].typed.clone(),
                    expr: prev.clone(),
                    loc:  defined[i].loc.clone(),
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
                    called.push(callargs.remove(0));
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
            None => return Err(Error::new(format!("{} does not a have a field named {}", self.memory[lhs_sym].typed, rhs), vec![
                (loc.clone(), format!("cannot access struct here"))
            ])),
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
            }
            fieldtyped.ptr.push(ast::Pointer{
                loc:  field.1.loc.clone(),
                tags: Tags::new(),
            });
        }

        //nested tail
        match fieldtyped.tail {
            ast::Tail::None => (),
            _ => {
                fieldtyped.tail = self.memory[lhs_sym].typed.tail.clone();
                //return Err(Error::new(
                //    format!("TODO: implement access to nested tail member"), vec![
                //    (loc.clone(), format!("here"))
                //]));
            }
        }

        let tmp = self.temporary(
            format!("{}.{}", self.memory[lhs_sym].name, rhs),
            fieldtyped,
            loc.clone(),
            Tags::new(),
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

                self.member_access(lhs_sym, rhs, loc)
            },
            ast::Expression::ArrayAccess {lhs, rhs, loc} => {
                let lhs_sym = self.execute_expr(lhs)?;
                let rhs_sym = self.execute_expr(rhs)?;

                if self.memory[rhs_sym].typed.t.signed() {
                    return Err(Error::new(format!("array access with signed index is not well defined"), vec![
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
                self.ssa.invocation(*lensym, vec![(lhs_sym, self.memory[lhs_sym].temporal)], tmp1);

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
                self.ssa.assert((tmp2, self.memory[tmp2].temporal), |a,model| match a {
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
                    Tags::new(),
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
                    Some("static_attest") => {
                        *emit = ast::EmitBehaviour::Skip;
                        let prev_inf = self.ssa.infinite;
                        self.ssa.infinite = false;
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
                        if !self.ssa.constrain((sym, self.memory[sym].temporal), true) {
                            return Err(Error::new(format!("function is unprovable"), vec![
                                (expr.loc().clone(), format!("static_attest leads to conflicting constrains"))
                            ]));
                        }

                        let r = self.literal(loc, Value::Integer(1), ast::Typed {
                            t:      ast::Type::ULiteral,
                            loc:    loc.clone(),
                            ptr:    Vec::new(),
                            tail:   ast::Tail::None,
                        });
                        self.ssa.infinite = prev_inf;
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
                        self.ssa.assert((sym, self.memory[sym].temporal) , |a,model|match a{
                            false => {
                                let mut estack = vec![(loc.clone(),
                                    format!("you may need an if condition or callsite_assert to increase confidence"))];
                                estack.extend(self.demonstrate(model.as_ref().unwrap(), (sym, self.memory[sym].temporal), 0));
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
                            if let Some(ast::Def::Fntype {ret,args,attr,vararg}) = self.defs.get(&n).cloned() {
                                self.deref(name_sym, loc)?;
                                self.memory[name_sym].value = Value::Function {
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

                        self.ssa.invocation(name_sym, syms, tmp);

                        Ok(tmp)
                    },
                    Value::Function{args: fargs, ret, vararg, callsite_assert, callsite_effect} => {

                        // borrochecker stupidity
                        let mut callsite_effect = callsite_effect.clone();
                        let vararg = *vararg;

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



                        // callsite assert evaluated in the callsite means we need to export
                        // callargs as their argument names

                        for callsite_assert in &mut callsite_assert {
                            self.push("callsite_assert".to_string());
                            self.ssa.push("callsite_assert");

                            for (i, farg) in fargs.iter().enumerate() {
                                let tmp = self.alloc(
                                    Name::from(&farg.name),
                                    farg.typed.clone(),
                                    args[i].loc().clone(),
                                    farg.tags.clone(),
                                )?;
                                self.copy(tmp, syms[i].0, args[i].loc())?;
                            }

                            let casym = self.execute_expr(callsite_assert)?;
                            self.ssa.assert((casym, self.memory[casym].temporal), |a,model| match a {
                                false => {
                                    let mut estack = vec![
                                        (loc.clone(),format!("in this callsite")),
                                        (callsite_assert.loc().clone(), format!("function call requires these conditions")),
                                    ];
                                    estack.extend(self.demonstrate(model.as_ref().unwrap(), (casym, self.memory[casym].temporal), 0));
                                    Err(Error::new(format!("unproven callsite assert for {}", self.memory[casym].name), estack))
                                }
                                true => {
                                    Ok(())
                                }
                            })?;

                            self.ssa.pop("end of callsite_assert");
                            self.pop();
                        }


                        // TODO for now mark all pointer call args as untrackable in the callsite
                        // this can be improved later, for example only mark mutable pointers
                        for (s,_) in &syms {
                            self.borrow_away(*s);
                        }

                        for callsite_effect in &mut callsite_effect {
                            self.push("callsite_effect".to_string());

                            for (i, farg) in fargs.iter().enumerate() {
                                let tmp = self.alloc(
                                    Name::from(&farg.name),
                                    farg.typed.clone(),
                                    args[i].loc().clone(),
                                    farg.tags.clone(),
                                )?;
                                self.copy(tmp, syms[i].0, args[i].loc())?;
                            }
                            let casym = self.execute_expr(callsite_effect)?;
                            if !self.ssa.constrain((casym, self.memory[casym].temporal), true) {
                                return Err(Error::new(format!("callsite effect would break SSA"), vec![
                                    (expr.loc().clone(), format!("there might be conflicting constrains"))
                                ]));
                            }

                            self.pop();
                        }

                        let tmp = self.temporary(
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
                        self.memory[tmp].value = value;

                        Ok(tmp)
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

                self.copy(tmp, lhs_sym, loc)?;

                let value = Value::PostfixOp {
                    lhs:    (tmp, self.memory[tmp].temporal),
                    op:     op.clone(),
                };
                self.memory[lhs_sym].value = value;

                self.memory[lhs_sym].temporal += 1;
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
                            tags :  ast::Tags::new(),
                            loc:    loc.clone(),
                        });
                        let tmp = self.temporary(
                            format!("address of {}", self.memory[lhs_sym].name),
                            typed,
                            loc.clone(),
                            Tags::new(),
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
                    _ => {
                        let lhs_sym = self.execute_expr(expr)?;
                        let tmp = self.temporary(
                            format!("unary expression"),
                            self.memory[lhs_sym].typed.clone(),
                            loc.clone(),
                            Tags::new(),
                        )?;


                        if op == &crate::ast::PrefixOperator::Boolnot && self.memory[lhs_sym].t != smt::Type::Bool {
                            return Err(Error::new(format!("expected boolean, got {}", self.memory[lhs_sym].typed), vec![
                                (expr.loc().clone(), format!("coercion to boolean is difficult to prove"))
                            ]));
                        }

                        if op == &crate::ast::PrefixOperator::Bitnot && self.memory[lhs_sym].t == smt::Type::Bool {
                            return Err(Error::new(format!("expected integer, got {}", self.memory[lhs_sym].typed), vec![
                                (expr.loc().clone(), format!("coercion from boolean is difficult to prove"))
                            ]));
                        }

                        let value = Value::PrefixOp {
                            lhs:    (lhs_sym, self.memory[lhs_sym].temporal),
                            op:     op.clone(),
                        };
                        self.memory[tmp].value = value;

                        self.ssa.prefix_op(
                            tmp,
                            (lhs_sym, self.memory[lhs_sym].temporal),
                            op.clone(),
                            self.memory[lhs_sym].t.clone(),
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
                self.memory[*to].value     = Value::Unconstrained("borrowed in function call".to_string());
            }
            _ => ()
        }
    }


    fn deref(&mut self, lhs_sym: Symbol, loc: &ast::Location) -> Result<Symbol, Error> {

        if let Value::Address(to) = self.memory[lhs_sym].value.clone() {
            return Ok(to);
        }

        let mut nutype = self.memory[lhs_sym].typed.clone();
        nutype.ptr.pop();

        let member = self.temporary(
            format!("*{}", self.memory[lhs_sym].name),
            nutype,
            loc.clone(),
            Tags::new(),
        )?;


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
        self.ssa.invocation(*theosym, vec![(lhs_sym, self.memory[lhs_sym].temporal)], tmp1);

        self.ssa.assert((tmp1, self.memory[tmp1].temporal), |a,model| match a {
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
            return Err(Error::new(format!("redeclation of local name '{}'", name), vec![
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
        if self.memory[sym].typed.ptr.len() > 0 {
            return Err(Error::new(format!("ICE: tail value on pointer"), vec![
                (loc.clone(), format!("tail binding must be on the type itself"))
            ]));
        }


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
        });
        debug!("{} {} := {}", name, typed, symbol);
        self.ssa.declare(symbol, &format!("{}", name), t);
        Ok(symbol)
    }


    //                  cpy here   from here
    fn copy(&mut self, lhs: Symbol, rhs: Symbol, used_here: &ast::Location) -> Result<(), Error> {

        // transfer safe mark
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
            self.ssa.invocation(*theosym, vec![(rhs, self.memory[rhs].temporal)], tmp_safe_transfer);
            self.ssa.invocation(*theosym, vec![(lhs, self.memory[lhs].temporal + 1)], tmp_safe_transfer);
        }
        self.memory[lhs].temporal += 1;



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
                                   Tags::new(),
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
        self.ssa.assign(
            (lhs, self.memory[lhs].temporal),
            (rhs, self.memory[rhs].temporal),
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
        self.ssa.invocation(*thsym, vec![(sym, self.memory[sym].temporal)], tmp);
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
        self.ssa.invocation(*lensym, vec![(sym, self.memory[sym].temporal)], tmp);
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
            current_function_name: String::new(),
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
                       estack.push((self.memory[sym.0].declared.clone(),
                       format!("for OVERFLOW of {} {{{}}} = {}", self.memory[sym.0].name, sym.1, v)));
                   } else {
                       estack.push((self.memory[sym.0].declared.clone(),
                       format!("for {} {{{}}} = {}", self.memory[sym.0].name, sym.1, v)));
                   }
               }

               if *lhs != sym {
                   estack.extend(self.demonstrate(model, *lhs, depth + 1));
               }
               if *rhs != sym {
                   estack.extend(self.demonstrate(model, *rhs, depth + 1));
               }
            },
            Value::Integer(_) => return estack,
            _ => {
                if let Some(v) = self.ssa.extract(model, sym) {
                    estack.push((self.memory[sym.0].declared.clone(),
                    format!("for {} {{{}}} = 0x{:x}", self.memory[sym.0].name, sym.1, v)));
                } else {
                    estack.push((self.memory[sym.0].declared.clone(),
                    format!("for {} {{{}}} = ??", self.memory[sym.0].temporal, sym.1)));
                }
            }
        };


        if depth == 0 {
            for trace in &self.stack.last().unwrap().trace {
                match self.ssa.extract(model, *trace) {
                    Some(f) if f > 0 => {
                        estack.push((self.memory[trace.0].declared.clone(), format!("reached because this branch condition was true")));
                        estack.extend(self.demonstrate(model, *trace, depth + 1));
                    },
                    Some(_)  => {
                        estack.push((self.memory[trace.0].declared.clone(), format!("reached because this branch condition was false")));
                        estack.extend(self.demonstrate(model, *trace, depth + 1));
                    },
                    None => {}
                };
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


pub fn execute(module: &mut flatten::Module) -> Symbolic {
    let mut sym = Symbolic::new(&module.name);
    match sym.execute_module(module) {
        Err(Error::Error{message, details}) => {
            use std::io::Write;
            emit_error(message.clone(), &details);


            write!(sym.ssa.debug.borrow_mut(), "; ERR {}", message).unwrap();
            sym.ssa.debug.borrow_mut().flush().unwrap();
            std::process::exit(9);
        },
        Ok(()) => {
        }
    }

    sym
}
