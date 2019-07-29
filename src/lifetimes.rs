use super::ast;
use super::flatten;
use super::parser;
use std::collections::HashMap;
use std::collections::HashSet;
use super::name::Name;


use std::sync::atomic::{AtomicBool, Ordering};
static ABORT: AtomicBool = AtomicBool::new(false);



//lvalue returns storage Address, rvalue returns Value
#[derive(PartialEq)]
enum Access {
    Storage,
    Value,
}

#[derive(Clone)]
enum Lifetime {
    Invalid,
    Static,
    Pointer(usize),
    Function {
        ret:  Option<Box<Lifetime>>,
        args: Vec<ast::NamedArg>
    },
    Moved{
    }
}

impl std::fmt::Display for Lifetime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lifetime::Invalid           => write!(f, "invalid"),
            Lifetime::Static            => write!(f, "static"),
            Lifetime::Pointer(to)       => write!(f, "ptr->{}", to),
            Lifetime::Function {..}     => write!(f, "function"),
            Lifetime::Moved{..}         => write!(f, "moved"),
        }
    }
}


#[derive(Clone)]
enum Storage {
    Valid {
        name:           Name,
        tags:           HashMap<String, ast::Location>,
        stored_here:    ast::Location,
        value:          Lifetime,
    },
    Invalid {
        stored_here:    ast::Location,
        dropped_here:   ast::Location,
    },
}

impl Storage {
    pub fn loc(&self) -> &ast::Location {
        match self {
            Storage::Valid{stored_here, ..}  => stored_here,
            Storage::Invalid{stored_here,..} => stored_here,
        }
    }
}

struct Scope {
    name:       String,
    locals:     HashMap<Name, usize>
}

#[derive(Default)]
struct Stack {
    pointers:   Vec<Storage>,
    stack:      Vec<Scope>,
}

impl Stack {
    fn push(&mut self, name: &str) {
        debug!("  scope {}", name);
        self.stack.push(Scope{
            name: name.to_string(),
            locals: HashMap::new(),
        });
    }

    fn pop(&mut self, dropped_here: &ast::Location) {
        let dead = self.stack.pop().unwrap();
        for ( _ , local) in dead.locals {
            let m = self.pointers.get_mut(local).unwrap();
            if let Storage::Valid{name, stored_here, value, tags} = m.clone() {
                *m = Storage::Invalid{stored_here, dropped_here: dropped_here.clone()};
            }
        }
    }

    fn cur(&mut self) -> &mut Scope {
        self.stack.last_mut().unwrap()
    }

    fn find(&self, name: &Name) -> Option<usize> {
        for scope in self.stack.iter().rev() {
            if let Some(v) = scope.locals.get(name) {
                return Some(*v);
            }
        }
        return None;
    }

    fn local(&mut self, name: Name, loc: ast::Location, tags: &HashMap<String, ast::Location>) -> usize {
        let ptr  = self.pointers.len();
        self.pointers.push(Storage::Valid{
            name:           name.clone(),
            stored_here:    loc,
            value:          Lifetime::Invalid,
            tags:           tags.clone(),
        });

        debug!("    let {} = {}", name, ptr);

        self.cur().locals.insert(name, ptr);
        ptr
    }

    fn check_name(&mut self, name: &Name, used_here: &ast::Location, access: Access) -> Lifetime {
        let local = match self.find(&name) {
            None => {
                warn!("undefined name '{}' \n{}", name,
                       parser::make_error(&used_here, format!("'{}' is not defined in this scope", name)),
                       );
                return Lifetime::Static;
            },
            Some(v) => v,
        };
        self.read(local, used_here, access)
    }


    fn read(&self, pointer: usize, used_here: &ast::Location, access: Access) -> Lifetime {
        match self.pointers.get(pointer) {
            None => panic!("ICE: invalid pointer"),
            Some(Storage::Invalid{stored_here, dropped_here}) => {
                error!("illegal read access to dangling pointer \n{}\n{}\n{}",
                       parser::make_error(&stored_here,     "this storage is no longer available"),
                       parser::make_error(&used_here,       "when used here"),
                       parser::make_error(&dropped_here,    "dropped here"),
                       );
                ABORT.store(true, Ordering::Relaxed);
                return Lifetime::Invalid;
            }
            Some(Storage::Valid{name, stored_here, value, tags}) => {
                if access == Access::Storage {
                    return Lifetime::Pointer(pointer)
                }

                if tags.contains_key("unsafe") {
                    error!("illegal read access to unsafe storage {}\n{}\n{}",
                           name,
                           parser::make_error(&used_here, "used here"),
                           parser::make_error(&stored_here, "suggestion: add a runtime check for this value and mark it safe"),
                           );
                    ABORT.store(true, Ordering::Relaxed);
                    return Lifetime::Invalid;
                }

                match value {
                    Lifetime::Invalid => {
                        error!("illegal read access to unitialized variable {}\n{}",
                               name,
                               parser::make_error(&used_here, "used here"),
                               );
                        ABORT.store(true, Ordering::Relaxed);
                        return Lifetime::Invalid;
                    }
                    Lifetime::Moved{..} => {
                        error!("illegal read access of moved value {}\n{}",
                               name,
                               parser::make_error(&used_here, "used here"),
                               );
                        ABORT.store(true, Ordering::Relaxed);
                        return Lifetime::Invalid;
                    }
                    v => {
                        v.clone()
                    }
                }
            }
        }
    }

    fn write(&mut self, pointer: usize, val: Lifetime, used_here: &ast::Location) {
        match self.pointers.get_mut(pointer) {
            None => panic!("ICE: invalid pointer"),
            Some(Storage::Invalid{stored_here, dropped_here}) => {
                error!("illegal write access to dangling pointer \n{}\n{}",
                       parser::make_error(&used_here, "used here"),
                       parser::make_error(&dropped_here, "dropped here"),
                       );
                ABORT.store(true, Ordering::Relaxed);
            }
            Some(Storage::Valid{name, stored_here, value, tags}) => {
                debug!("    {} <= {}", name, val);
                *value = val;
            }
        };
    }


    fn check_expr(&mut self, expr: &mut ast::Expression, access: Access) -> Lifetime {
        match expr {
            ast::Expression::Name(name) => {
                self.check_name(&name.name, &name.loc, access)
            }
            ast::Expression::MemberAccess {lhs, rhs, op, ..} => {
                //TODO

                if op == "->" {
                    self.check_expr(lhs, Access::Value)
                } else {
                    self.check_expr(lhs, access)
                }
            }
            ast::Expression::ArrayAccess {lhs, rhs, ..} => {
                //TODO
                self.check_expr(lhs, access)
            }
            ast::Expression::Literal { loc, .. } => {
                if access == Access::Storage {
                    error!("lvalue expression is not a storage location\n{}",
                           parser::make_error(&loc, "literal cannot be used as lvalue"),
                           );
                    ABORT.store(true, Ordering::Relaxed);
                    return Lifetime::Invalid;
                }
                Lifetime::Static
            },
            ast::Expression::Call { name, args: callargs, .. } => {
                if name.name.is_absolute() && name.name.0[1] == "libc" {
                    //TODO
                    return Lifetime::Static;
                }
                match self.check_name(&name.name, &name.loc, Access::Value) {
                    Lifetime::Function{ret, args} => {
                        if args.len() != callargs.len()  {
                            error!("call argument count mismatch\n{}",
                                   parser::make_error(&name.loc, format!("this function expects {} arguments", args.len()))
                                   );
                            ABORT.store(true, Ordering::Relaxed);
                            return Lifetime::Invalid;
                        }

                        for i in 0..args.len() {
                            let arglf = self.check_expr(&mut callargs[i], Access::Value);
                            match arglf {
                                Lifetime::Static => (),
                                Lifetime::Pointer(to)  => {
                                    if args[0].tags.contains_key("mutable") {
                                        //if !tags.contains_key("mutable") {
                                        //    error!("immutable storage cannot be used as mutable argument\n{}\n{}",
                                        //           parser::make_error(&callargs[0].loc(), "this expression is immutable"),
                                        //           parser::make_error(&self.pointers[to].loc(), "suggestion: change this declaration to mutable"),
                                        //           );
                                        //    ABORT.store(true, Ordering::Relaxed);
                                        //}
                                    }
                                }
                                Lifetime::Invalid | Lifetime::Moved{..} | Lifetime::Function{..} => {
                                    error!("invalid expression used as argument\n{}",
                                           parser::make_error(&callargs[0].loc(), "cannot determine lifetime of expression"),
                                           );
                                    ABORT.store(true, Ordering::Relaxed);
                                }
                            }
                        }

                        //TODO
                        return match ret {
                            Some(ret) => *ret.clone(),
                            None => Lifetime::Static,
                        }
                    },
                    _ => {
                        warn!("lvalue is not a valid function\n{}",
                               parser::make_error(&name.loc, "this expression cannot be used as function"),
                               );
                        return Lifetime::Invalid;
                    }
                }
            }
            ast::Expression::InfixOperation { lhs, rhs, loc} => {
                if access == Access::Storage {
                    error!("value expression is not a storage location\n{}",
                           parser::make_error(&loc, "this expression cannot be used as lvalue"),
                           );
                    ABORT.store(true, Ordering::Relaxed);
                    return Lifetime::Invalid;
                }
                for (_, expr) in rhs {
                    self.check_expr(expr, Access::Value);
                }
                self.check_expr(lhs, Access::Value)
            }
            ast::Expression::Cast { expr, .. } => {
                //TODO
                self.check_expr(expr, access)
            }
            ast::Expression::UnaryPost {expr, ..} => {
                Lifetime::Invalid
            }
            ast::Expression::UnaryPre {expr, op,..} => {
                if op == "&" {
                    self.check_expr(expr, Access::Storage)
                } else if op == "*" {
                    let v = self.check_expr(expr, Access::Value);
                    match v {
                        Lifetime::Pointer(to)  => {
                            self.read(to, expr.loc(), access)
                        }
                        Lifetime::Function{..} => { v } ,
                        Lifetime::Static => {
                            error!("taking pointer of static lifetime\n{}",
                                   parser::make_error(expr.loc(), "cannot determine lifetime of expression"),
                                   );
                            ABORT.store(true, Ordering::Relaxed);
                            return Lifetime::Invalid;
                        },
                        Lifetime::Invalid | Lifetime::Moved{..} => {
                            error!("taking pointer of invalid lifetime\n{}",
                                   parser::make_error(expr.loc(), "cannot determine lifetime of expression"),
                                   );
                            ABORT.store(true, Ordering::Relaxed);
                            return Lifetime::Invalid;
                        }
                    }
                } else {
                    Lifetime::Invalid
                }
            }
            ast::Expression::StructInit {loc, typed, fields} => {
                Lifetime::Invalid
            }
            ast::Expression::ArrayInit {fields, ..} => {
                Lifetime::Invalid
            }
        }
    }

    fn check_block(&mut self, body: &mut ast::Block) {
        for stm in &mut body.statements {
            self.check_stm(stm)
        }
    }

    fn check_stm(&mut self, stm: &mut ast::Statement) {
        match stm {
            ast::Statement::Mark{lhs, mark, loc} => {
                let lhs_lf = self.check_expr(lhs, Access::Storage);
                match lhs_lf {
                    Lifetime::Pointer(to)  => {
                        if mark == "safe" {
                            match self.pointers.get_mut(to).unwrap() {
                                Storage::Invalid{stored_here, dropped_here, .. } => {
                                    error!("cannot change lifetime of dropped pointer\n{}\n{}",
                                           parser::make_error(&stored_here, "this storage is no longer available"),
                                           parser::make_error(&dropped_here, "because it was dropped here"),
                                           );
                                    ABORT.store(true, Ordering::Relaxed);
                                },
                                Storage::Valid{tags,..} => {
                                    tags.remove("unsafe");
                                }
                            }
                        } else {
                            error!("undefined mark '{}'\n{}",
                                   mark,
                                   parser::make_error(loc, "the meaning of this mark is not (yet) known"),
                                   );
                            ABORT.store(true, Ordering::Relaxed);
                        }
                    },
                    _ => {
                        error!("lvalue is not a storage location\n{}",
                               parser::make_error(lhs.loc(), "left hand side doesn't name something with a lifetime"),
                               );
                        ABORT.store(true, Ordering::Relaxed);
                    },
                };
            }
            ast::Statement::Block(block) => {
                self.push("block");
                self.check_block(block);
                self.pop(&block.end);
            }
            ast::Statement::Cond{body, expr,..}=> {
                self.push("if");
                if let Some(expr) = expr {
                    self.check_expr(expr, Access::Value);
                }
                self.check_block(body);
                self.pop(&body.end);
            },
            ast::Statement::For{body, e1, e2, e3,..} => {
                self.push("for");
                if let Some(stm) = e1 {
                    self.check_stm(stm)
                }
                if let Some(stm) = e2 {
                    self.check_stm(stm)
                }
                if let Some(stm) = e3 {
                    self.check_stm(stm)
                }
                self.check_block(body);
                self.pop(&body.end);
            }
            ast::Statement::Expr{loc, expr} => {
                self.check_expr(expr, Access::Value);
            }
            ast::Statement::Var{name, assign, tags, loc, ..} => {
                let ptr = self.local(Name::from(&*name), loc.clone(), tags);

                if let Some(assign) = assign {
                    let rhs_rf = self.check_expr(assign, Access::Value);
                    match rhs_rf {
                        Lifetime::Invalid => {
                            warn!("rvalue has unknown lifetime\n{}",
                                  parser::make_error(assign.loc(), "cannot determine lifetime of right hand side"),
                                  );
                        }
                        _ => (),
                    };
                    self.write(ptr, rhs_rf, loc);
                }
            },
            ast::Statement::Assign{lhs, rhs,loc, ..} => {
                let rhs_rf = self.check_expr(rhs, Access::Value);
                match rhs_rf {
                    Lifetime::Invalid => {
                        warn!("rvalue has invalid lifetime\n{}",
                              parser::make_error(rhs.loc(), "cannot determine lifetime of right hand side"),
                              );
                    }
                    _ => (),
                };

                let lhs_lf = self.check_expr(lhs, Access::Storage);
                match lhs_lf {
                    Lifetime::Pointer(to)  => {
                        let tags = match self.pointers.get(to) {
                            Some(Storage::Valid{tags,..}) => tags,
                            _ => unreachable!(),
                        };
                        if !tags.contains_key("mutable") {
                            error!("cannot assign to immutable storage\n{}\n{}",
                                   parser::make_error(lhs.loc(), "lvalue expression is immutable"),
                                   parser::make_error(&self.pointers[to].loc(), "suggestion: change this declaration to mutable"),
                                   );
                            ABORT.store(true, Ordering::Relaxed);
                        }
                        self.write(to, rhs_rf, lhs.loc());
                    },
                    _ => {
                        error!("lvalue has invalid lifetime\n{}",
                               parser::make_error(lhs.loc(), "cannot determine lifetime of left hand side"),
                               );
                        ABORT.store(true, Ordering::Relaxed);
                    },
                };



            },
            ast::Statement::Return{expr,..} => {
            },
            ast::Statement::Goto{..}
            | ast::Statement::Label{..}
            => {},
        }
    }
}




pub fn check(md: &mut flatten::Module) {
    debug!("lifetime checking {}", md.name);

    let mut stack = Stack::default();
    stack.push("static");

    for d in &mut md.d {
        let local  = match d {
            flatten::D::Include(_) => continue,
            flatten::D::Local(v) => v,
        };
        match &mut local.def {
            ast::Def::Static{typed, expr, storage, tags} => {
                let localname = Name::from(&local.name);
                let ptr = stack.local(localname.clone(), local.loc.clone(), tags);
                stack.write(ptr, Lifetime::Static, &local.loc);
            },
            ast::Def::Function{body, args, ret, ..} => {

                let localname = Name::from(&local.name);
                let ptr = stack.local(localname.clone(), local.loc.clone(), &HashMap::new());

                stack.push(&local.name);
                for arg in args.iter() {
                    let argname = Name::from(&arg.name);

                    let mut storage = stack.local(argname.clone(), arg.loc.clone(), &arg.tags);

                    for ast_ptr in arg.typed.ptr.iter().rev() {
                        let site = stack.local(Name::from(&format!("__builtin::pointer_to_callsite::{}", storage)), arg.loc.clone(), &ast_ptr.tags);
                        stack.write(storage, Lifetime::Pointer(site), &arg.loc);
                        storage = site;

                        for (mark, loc) in &ast_ptr.tags {
                            match mark.as_str() {
                                "mutable" => {}
                                "unsafe" => {}
                                _ => {
                                    error!("undefined mark '{}'\n{}",
                                           mark,
                                           parser::make_error(loc, "the meaning of this mark is not (yet) known"),
                                           );
                                    ABORT.store(true, Ordering::Relaxed);
                                }
                            }
                        }
                    }

                    let site = stack.local(Name::from(&format!("__builtin::callstack::{}", storage)), arg.loc.clone(), &arg.tags);
                    stack.write(storage, Lifetime::Pointer(site), &arg.loc);
                }

                stack.check_block(body);


                let ret = match ret {
                    None => None,
                    Some(_) => {
                        //TODO
                        Some(Box::new(Lifetime::Static))
                    }
                };
                stack.write(ptr, Lifetime::Function{
                    ret,
                    args: args.clone(),
                }, &local.loc);

                stack.pop(&body.end);

            },
            _ => (),
        }
    }
    if ABORT.load(Ordering::Relaxed) {
        std::process::exit(9);
    }
}
