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
    Uninitialized,
    Static,
    Pointer(usize),
    Function {
        ret:  Option<Box<Lifetime>>,
        args: Vec<ast::NamedArg>,
        vararg: bool,
    },
    Moved{
        moved_here:   ast::Location,
    },
    Dropped {
        stored_here:    ast::Location,
        dropped_here:   ast::Location,
    },
}

impl Lifetime {
    pub fn as_pointer(&self) -> usize {
        match self {
            Lifetime::Pointer(u) => *u,
            _ => panic!("ICE: not a pointer"),
        }
    }
}

impl std::fmt::Display for Lifetime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lifetime::Uninitialized     => write!(f, "uninitialized"),
            Lifetime::Static            => write!(f, "static"),
            Lifetime::Pointer(to)       => write!(f, "ptr->{}", to),
            Lifetime::Function {..}     => write!(f, "function"),
            Lifetime::Moved{..}         => write!(f, "moved"),
            Lifetime::Dropped{..}       => write!(f, "dropped"),
        }
    }
}


#[derive(Clone)]
struct Storage {
    name:           Name,
    typ:            Option<ast::Typed>,
    tags:           HashMap<String, ast::Location>,
    stored_here:    ast::Location,
    changed_here:   Option<ast::Location>,
    value:          Lifetime,
}

struct Scope {
    name:       String,
    locals:     HashMap<Name, usize>
}

#[derive(Default)]
struct Stack {
    pointers:   Vec<Storage>,
    stack:      Vec<Scope>,

    current_return_ptr:     Option<usize>,
    must_move_before_ret:   HashMap<usize, ast::Location>,
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
            m.value = Lifetime::Dropped {
                stored_here:    m.stored_here.clone(),
                dropped_here:   dropped_here.clone(),
            };
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

    fn local(&mut self, typ: Option<ast::Typed>, name: Name, loc: ast::Location, tags: &HashMap<String, ast::Location>) -> usize {
        let ptr  = self.pointers.len();
        self.pointers.push(Storage{
            typ,
            name:           name.clone(),
            stored_here:    loc,
            value:          Lifetime::Uninitialized,
            tags:           tags.clone(),
            changed_here:   None,
        });

        debug!("    let {} = {}", name, ptr);

        self.cur().locals.insert(name, ptr);
        ptr
    }

    fn check_name(&mut self, name: &Name, used_here: &ast::Location, access: Access) -> Lifetime {
        let local = match self.find(&name) {
            None => {
                error!("undefined name '{}' \n{}", name,
                       parser::make_error(&used_here, format!("'{}' is not defined in this scope", name)),
                       );
                ABORT.store(true, Ordering::Relaxed);
                return Lifetime::Static;
            },
            Some(v) => v,
        };
        self.read(local, used_here, access)
    }


    fn read(&self, pointer: usize, used_here: &ast::Location, access: Access) -> Lifetime {
        let storage = self.pointers.get(pointer).expect("ICE: invalid pointer");

        match &storage.value {
            Lifetime::Dropped{stored_here, dropped_here} => {
                error!("illegal read access to dropped value {}\n{}\n{}\n{}",
                       storage.name,
                       parser::make_error(&used_here,     "used here"),
                       parser::make_error(&stored_here,   "points at this storage location"),
                       parser::make_error(&dropped_here,  "which was dropped here"),
                       );
                ABORT.store(true, Ordering::Relaxed);
                return Lifetime::Uninitialized;
            },
            _ => (),
        }

        if access == Access::Storage {
            return Lifetime::Pointer(pointer)
        }

        if storage.tags.contains_key("unsafe") {
            error!("illegal read access to unsafe storage {}\n{}\n{}",
                   storage.name,
                   parser::make_error(&used_here, "used here"),
                   parser::make_error(&storage.stored_here, "suggestion: add a runtime check for this value and mark it safe"),
                   );
            ABORT.store(true, Ordering::Relaxed);
            return Lifetime::Uninitialized;
        }

        match &storage.value {
            Lifetime::Uninitialized => {
                error!("illegal read access to unitialized variable {}\n{}",
                       storage.name,
                       parser::make_error(&used_here, "used here"),
                       );
                ABORT.store(true, Ordering::Relaxed);
                return Lifetime::Uninitialized;
            }
            Lifetime::Moved{moved_here} => {
                error!("illegal read access of moved value {}\n{}\n{}",
                       storage.name,
                       parser::make_error(&used_here, "use of moved value"),
                       parser::make_error(&moved_here, "was moved here"),
                       );
                ABORT.store(true, Ordering::Relaxed);
                return Lifetime::Uninitialized;
            }
            v => {
                v.clone()
            }
        }
    }

    fn write(&mut self, pointer: usize, val: Lifetime, used_here: &ast::Location) -> Lifetime {
        let storage = self.pointers.get_mut(pointer).expect("ICE: invalid pointer");
        debug!("    {} <= {}", storage.name, val);
        storage.value = val.clone();
        storage.changed_here = Some(used_here.clone());
        val
    }



    fn check_call_arg(&mut self, stack: &ast::NamedArg, callsite: &mut ast::Expression) {
        if let Lifetime::Static = self.check_expr(callsite, Access::Value) {
            return;
        }

        let mut callsite_ptr     = self.check_expr(callsite, Access::Storage).as_pointer();
        let mut callsite_storage = &mut self.pointers[callsite_ptr];

        for (i, stack_ptr) in stack.typed.ptr.iter().rev().enumerate() {

            // stack_ptr is now the last pointer in the functions stack argument
            // i.e it is "mut*" if the arg was "int ** mut* a"
            // we dig deeper into the callsite as we move right to left

            callsite_ptr = match &callsite_storage.value {
                Lifetime::Pointer(to) => {
                    *to
                },
                Lifetime::Uninitialized => {
                    error!("uninitialized pointer arg passed as safe pointer\n{}\n{}",
                           parser::make_error(&callsite.loc(), "this pointer must be safe"),
                           parser::make_error(&callsite_storage.stored_here, "but this value is unitialized"),
                           );
                    ABORT.store(true, Ordering::Relaxed);
                    return;
                },
                Lifetime::Dropped{stored_here, dropped_here} => {
                    error!("passing dropped value as safe pointer {}\n{}\n{}\n{}",
                           callsite_storage.name,
                           parser::make_error(&callsite.loc(),"used here"),
                           parser::make_error(&stored_here,   "points at this storage location"),
                           parser::make_error(&dropped_here,  "which was dropped here"),
                           );
                    ABORT.store(true, Ordering::Relaxed);
                    return;
                },
                Lifetime::Moved{moved_here} => {
                    error!("passing moved value '{}' as safe pointer \n{}\n{}",
                           callsite_storage.name,
                           parser::make_error(&callsite.loc(), "use of moved value"),
                           parser::make_error(&moved_here, "was moved here"),
                           );
                    ABORT.store(true, Ordering::Relaxed);
                    return;
                },
                Lifetime::Function{..} => {
                    error!("ICE: trying to pass function as pointer\n{}",
                           parser::make_error(&callsite.loc(), "cannot determine lifetime of expression"),
                           );
                    ABORT.store(true, Ordering::Relaxed);
                    return;
                },
                Lifetime::Static => {
                    if let Some(change) = &callsite_storage.changed_here {
                        error!("incompatible argument\n{}\n{}\n{}",
                               parser::make_error(&callsite.loc(), "this expression has a different pointer depth"),
                               parser::make_error(&change, "value assigned here might not be a pointer"),
                               parser::make_error(&stack.typed.loc, format!("expected at depth {} here", i)),
                               );
                    } else {
                        error!("incompatible argument\n{}\n{}",
                               parser::make_error(&callsite.loc(), "this value has a different pointer depth"),
                               parser::make_error(&stack.typed.loc, "expected this type"),
                               );
                    }
                    ABORT.store(true, Ordering::Relaxed);
                    return;
                }
            };
            callsite_storage = &mut self.pointers[callsite_ptr];


            if stack_ptr.tags.contains_key("mutable") && !callsite_storage.tags.contains_key("mutable") {
                error!("const pointer cannot be used as mut pointer in function call\n{}\n{}",
                       parser::make_error(&callsite.loc(), "this expression must yield a mutable pointer"),
                       parser::make_error(&callsite_storage.stored_here, "suggestion: change this declaration to mutable"),
                       );
                ABORT.store(true, Ordering::Relaxed);
                return;
            }

            if callsite_storage.tags.contains_key("unavailable") {
                error!("cannot use unavailable local '{}'\n{}",
                       callsite_storage.name,
                       parser::make_error(&callsite.loc(),
                       format!("'{}' is unavailable here and must be made available with ? first", callsite_storage.name)),
                       );
                ABORT.store(true, Ordering::Relaxed);
                return;

            }

            if let Some(tagloc) = stack_ptr.tags.get("via") {
                callsite_storage.tags.insert("unavailable".to_string(), callsite.loc().clone());
            }

            if let Some(tagloc) = stack_ptr.tags.get("move") {
                if callsite_storage.tags.contains_key("stack") {
                    error!("cannot move stack\n{}",
                           parser::make_error(&callsite.loc(),
                           format!("this expression would move '{}' out of scope, which is on the stack", callsite_storage.name)),
                           );
                    ABORT.store(true, Ordering::Relaxed);
                    return;
                }

                if let Some(_) = callsite_storage.tags.get("borrowed") {
                    error!("cannot move borrowed pointer\n{}\n{}\n{}",
                           parser::make_error(&callsite.loc(),
                           format!("this expression would move '{}' out of scope", callsite_storage.name)),
                           parser::make_error(&tagloc, "required because this call argument is move"),
                           parser::make_error(&callsite_storage.stored_here, "try changing this declaration to move"),
                           );
                    ABORT.store(true, Ordering::Relaxed);
                    return;
                }
                //TODO this will only move the rightmost value
                callsite_storage.value = Lifetime::Moved{
                    moved_here: callsite.loc().clone(),
                };
                return;
            }

            if stack_ptr.tags.contains_key("unsafe") {
                return;
            }

            if !stack_ptr.tags.contains_key("unsafe") && callsite_storage.tags.contains_key("unsafe") {
                error!("passing unsafe pointer to safe function call\n{}\n{}",
                       parser::make_error(&callsite.loc(), "this expression must be safe"),
                       parser::make_error(&callsite_storage.stored_here, "suggestion: add a runtime check for this value and mark it safe"),
                       );
                ABORT.store(true, Ordering::Relaxed);
                return;
            }
        }
    }


    fn check_expr(&mut self, expr: &mut ast::Expression, access: Access) -> Lifetime {
        let exprloc = expr.loc().clone();
        match expr {
            ast::Expression::Name(name) => {
                if name.name.is_absolute() && name.name.0[1] == "ext" {
                    //TODO
                    return Lifetime::Static;
                }
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
                    return Lifetime::Uninitialized;
                }
                Lifetime::Static
            },
            ast::Expression::Call { name, args: callargs, loc: callloc, .. } => {
                if name.name.is_absolute() && name.name.0[1] == "ext" {
                    //TODO
                    return Lifetime::Static;
                }
                match self.check_name(&name.name, &name.loc, Access::Value) {
                    Lifetime::Function{ret, args, vararg} => {
                        debug!("    checking function call {}", name.name);


                        // generated arguments
                        for i in 0..args.len() {
                            if args[i].tags.contains_key("_callsite_file") {
                                if i > callargs.len() { break }
                                callargs.insert(i, Box::new(ast::Expression::Literal{
                                    loc: callloc.clone(),
                                    v:   "__FILE__".into(),
                                }));
                            } else if args[i].tags.contains_key("_callsite_line") {
                                if i > callargs.len() { break }
                                callargs.insert(i, Box::new(ast::Expression::Literal{
                                    loc: callloc.clone(),
                                    v:   "__LINE__".into(),
                                }));
                            } else if args[i].tags.contains_key("_callsite_function") {
                                if i > callargs.len() { break }
                                callargs.insert(i, Box::new(ast::Expression::Literal{
                                    loc: callloc.clone(),
                                    v:   "__FUNCTION__".into(),
                                }));
                            }
                        }

                        if (!vararg && args.len() != callargs.len()) | (vararg && args.len() > callargs.len())   {
                            error!("call argument count mismatch\n{}",
                                   parser::make_error(&name.loc,
                                        format!("this function expects {} arguments, but you passed {}",
                                                args.len(),
                                                callargs.len() ))
                                   );
                            ABORT.store(true, Ordering::Relaxed);
                            return Lifetime::Uninitialized;
                        }
                        for i in 0..args.len() {
                            // check value of expression, which will be copied into the functions scope
                            self.check_call_arg(&args[i], &mut callargs[i])
                        }

                        return match ret {
                            Some(ret) => {
                                let mut return_scope_lf  = *ret.clone();
                                let mut callsite_lf = return_scope_lf.clone();

                                loop {
                                    if let Lifetime::Pointer(return_scope_ptr) = return_scope_lf {
                                        let tags = &self.pointers[return_scope_ptr].tags.clone();
                                        return_scope_lf = self.pointers[return_scope_ptr].value.clone();

                                        let mut callsite_ptr = self.local(None, Name::from(
                                                &format!("function call return {} at {:?}", name.name, exprloc.span.start_pos().line_col())),
                                                exprloc.clone(),
                                                &tags);

                                        self.write(callsite_ptr, callsite_lf, &exprloc);
                                        callsite_lf     = Lifetime::Pointer(callsite_ptr);
                                    } else {
                                        break callsite_lf;
                                    }
                                }

                            }
                            None => Lifetime::Static,
                        }
                    },
                    //TODO c functions, macros
                    Lifetime::Static => {
                        return Lifetime::Static;
                    },
                    _ => {
                        warn!("lvalue is not a valid function\n{}",
                               parser::make_error(&name.loc, "this expression cannot be used as function"),
                               );
                        return Lifetime::Uninitialized;
                    }
                }
            }
            ast::Expression::InfixOperation { lhs, rhs, loc} => {
                if access == Access::Storage {
                    error!("value expression is not a storage location\n{}",
                           parser::make_error(&loc, "this expression cannot be used as lvalue"),
                           );
                    ABORT.store(true, Ordering::Relaxed);
                    return Lifetime::Uninitialized;
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
            ast::Expression::UnaryPost {expr, ref mut op, ..}=> {
                self.check_expr(expr, access)
            }
            ast::Expression::UnaryPre {expr, op,..} => {
                if op == "&" {

                    let lf = self.check_expr(expr, Access::Storage);
                    let temp_ptr = self.local(None, Name::from(
                            &format!("temporary access at {:?}", expr.loc().span.start_pos().line_col())),
                            expr.loc().clone(),
                            &HashMap::new());
                    self.write(temp_ptr, lf, &expr.loc());
                    Lifetime::Pointer(temp_ptr)

                } else if op == "*" {
                    let v = self.check_expr(expr, Access::Value);
                    match v {
                        Lifetime::Uninitialized => Lifetime::Uninitialized,
                        Lifetime::Pointer(to)  => {
                            self.read(to, expr.loc(), access)
                        }
                        _ => {
                            let v_ptr = self.check_expr(expr, Access::Storage).as_pointer();
                            let v_store = self.pointers.get_mut(v_ptr).unwrap();
                            if let Some(change) = &v_store.changed_here {
                                error!("dereferencing something that is not a pointer\n{}\n{}",
                                       parser::make_error(expr.loc(), "cannot determine lifetime of expression"),
                                       parser::make_error(&change, "this assignment does not make a valid pointer"),
                                       );
                            } else {
                                error!("dereferencing something that is not a pointer\n{}",
                                       parser::make_error(expr.loc(), "cannot determine lifetime of expression"),
                                       );
                            }
                            ABORT.store(true, Ordering::Relaxed);
                            return Lifetime::Uninitialized;
                        }
                    }
                } else {
                    Lifetime::Uninitialized
                }
            }
            ast::Expression::StructInit {loc, typed, fields} => {
                let mut all_static = true;
                for (_, field) in fields {
                    if let Lifetime::Static = self.check_expr(field, Access::Value)  {
                    } else {
                        all_static = false
                    }
                }

                if all_static {
                    Lifetime::Static
                } else {
                    Lifetime::Uninitialized
                }
            }
            ast::Expression::ArrayInit {fields, ..} => {
                let mut all_static = true;
                for field in fields {
                    if let Lifetime::Static = self.check_expr(field, Access::Value)  {
                    } else {
                        all_static = false
                    }
                }

                if all_static {
                    Lifetime::Static
                } else {
                    Lifetime::Uninitialized
                }
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
                        let storage = self.pointers.get_mut(to).unwrap();
                        if mark == "safe" {
                            storage.tags.remove("unsafe");
                        } else if mark == "available" {
                            storage.tags.remove("unavailable");
                        } else {
                            storage.tags.insert(mark.to_string(), loc.clone());
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
            ast::Statement::Via{body, expr, loc} => {
                let lhs_lf = self.check_expr(expr, Access::Value);
                match lhs_lf {
                    Lifetime::Pointer(to)  => {
                        let storage = self.pointers.get_mut(to).unwrap();
                        storage.tags.remove("unavailable");

                        let typ = match &storage.typ {
                            None => {
                                error!("untyped storage location\n{}",
                                       parser::make_error(expr.loc(), "we cannot desugar unknown types"),
                                       );
                                ABORT.store(true, Ordering::Relaxed);
                                return;
                            },
                            Some(t) => t.name.clone(),
                        };
                        let mut typename = typ.0.clone();

                        if typename[1] == "ext" {
                            error!("via is impossible for ext type '{}'\n{}",
                                   typ,
                                   parser::make_error(expr.loc(), "cannot desugar ext type"),
                                   );
                            ABORT.store(true, Ordering::Relaxed);
                            return;

                        }

                        typename.pop();
                        typename.push("via".to_string());

                        let nucall = ast::Expression::Call{
                            loc:    loc.clone(),
                            name:   ast::Typed{
                                loc: storage.typ.as_ref().unwrap().loc.clone(),
                                name: Name(typename),
                                ptr: Vec::new(),
                            },
                            args:   vec![
                                expr.clone()
                            ],
                        };

                        let mut nustm = ast::Statement::Cond{
                            op:         "if".to_string(),
                            expr:       Some(nucall),
                            body:       ast::Block{
                                statements: vec![*body.clone()],
                                end: loc.clone(),
                            },
                        };
                        self.check_stm(&mut nustm);

                        *stm = nustm;
                    },
                    _ => {
                        error!("lvalue is not a pointer\n{}",
                               parser::make_error(expr.loc(), "via's are pointers"),
                               );
                        ABORT.store(true, Ordering::Relaxed);
                        return;
                    },
                };

            },
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
                let mut tags = tags.clone();
                tags.insert("stack".to_string(), loc.clone());
                let ptr = self.local(None, Name::from(&*name), loc.clone(), &tags);

                if let Some(assign) = assign {
                    let rhs_rf = self.check_expr(assign, Access::Value);
                    match rhs_rf {
                        Lifetime::Uninitialized => {
                            warn!("rvalue has unknown lifetime\n{}",
                                  parser::make_error(assign.loc(), "cannot determine lifetime of right hand side"),
                                  );
                        }
                        _ => (),
                    };
                    self.write(ptr, rhs_rf, loc);
                }
            },
            ast::Statement::Assign{lhs, rhs, loc, ..} => {
                let rhs_rf = self.check_expr(rhs, Access::Value);
                match rhs_rf {
                    Lifetime::Uninitialized => {
                        warn!("rvalue has invalid lifetime\n{}",
                              parser::make_error(rhs.loc(), "cannot determine lifetime of right hand side"),
                              );
                    }

                    _ => (),
                };

                let lhs_lf = self.check_expr(lhs, Access::Storage);
                match lhs_lf {
                    Lifetime::Pointer(to)  => {
                        let storage = &self.pointers.get(to).expect("ICE: invalid pointer");
                        let tags = &storage.tags;
                        if !tags.contains_key("mutable") {
                            error!("cannot assign to immutable storage\n{}\n{}",
                                   parser::make_error(lhs.loc(), "lvalue expression must be mutable"),
                                   parser::make_error(&self.pointers[to].stored_here, "suggestion: change this declaration to mutable"),
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
            ast::Statement::Return{ref mut expr, loc} => {
                if let Some(ref mut expr) = expr {
                    let val = self.check_expr(expr, Access::Value);
                    if let Some(rptr) = self.current_return_ptr {
                    }
                }

                for (ptr, callloc) in std::mem::replace(&mut self.must_move_before_ret, HashMap::new()) {
                    let store = self.pointers.get(ptr).unwrap();
                    if let Lifetime::Moved{..} = store.value {
                        continue;
                    }
                    error!("function returns orphaning moved pointer\n{}\n{}",
                           parser::make_error(&callloc, "the call moves a return value into scope"),
                           parser::make_error(&loc, "but will be orphaned here"),
                           );
                    ABORT.store(true, Ordering::Relaxed);

                }
            },
            ast::Statement::Goto{..}
            | ast::Statement::Label{..}
            | ast::Statement::Break{..}
            => {},
        }
    }
}




pub fn check(md: &mut flatten::Module) {
    debug!("lifetime checking {}", md.name);

    let mut stack = Stack::default();
    stack.push("static");

    for (name,loc) in &md.c_names {
        let mut tags = HashMap::new();
        let ptr = stack.local(None, name.clone(), loc.clone(), &tags);
        stack.write(ptr, Lifetime::Static, &loc);
    }

    for d in &mut md.d {
        let local  = match d {
            flatten::D::Include(_) => continue,
            flatten::D::Local(v) => v,
        };
        match &mut local.def {
            ast::Def::Macro{args, body} => {
                let localname = Name::from(&local.name);
                let ptr = stack.local(None, localname.clone(), local.loc.clone(), &HashMap::new());
                stack.write(ptr, Lifetime::Static, &local.loc);
            },
            ast::Def::Static{typed, expr, storage, tags} => {
                let localname = Name::from(&local.name);
                let ptr = stack.local(Some(typed.clone()), localname.clone(), local.loc.clone(), tags);
                stack.write(ptr, Lifetime::Static, &local.loc);
            },
            ast::Def::Function{body, args, ret, vararg, ..} => {

                let localname = Name::from(&local.name);
                let ptr = stack.local(None, localname.clone(), local.loc.clone(), &HashMap::new());

                let ret = match ret {
                    None => None,
                    Some(ret) => {
                        let mut rlf = Lifetime::Static;

                        for ast_ptr in ret.typed.ptr.iter() {
                            let mut tags = ast_ptr.tags.clone();
                            if !tags.contains_key("move") {
                                tags.insert("borrowed".to_string(), ret.typed.loc.clone());
                            }
                            let storage = stack.local(
                                Some(ret.typed.clone()),
                                Name::from(&format!("return value of {}", local.name)),
                                ret.typed.loc.clone(), &tags);
                            stack.write(storage, rlf, &ret.typed.loc.clone());
                            stack.current_return_ptr = Some(storage);
                            rlf = Lifetime::Pointer(storage);
                        }

                        Some(Box::new(rlf))
                    }
                };

                stack.push(&local.name);

                for arg in args.iter() {
                    let argname = Name::from(&arg.name);

                    let mut storage = stack.local(Some(arg.typed.clone()), argname.clone(), arg.loc.clone(), &arg.tags);

                    for ast_ptr in arg.typed.ptr.iter().rev() {
                        let mut body_tags = ast_ptr.tags.clone();
                        if !body_tags.contains_key("move") {
                            body_tags.insert("borrowed".to_string(), arg.loc.clone());
                        }
                        let site = stack.local(
                            Some(arg.typed.clone()),
                            Name::from(&format!("__builtin::pointer_to_callsite::{}", storage)), arg.loc.clone(), &body_tags
                        );
                        stack.write(storage, Lifetime::Pointer(site), &arg.loc);
                        storage = site;

                        for (mark, loc) in &ast_ptr.tags {
                            match mark.as_str() {
                                "mutable" => {}
                                "unsafe" => {}
                                "move" => {}
                                "via" => {}
                                _ => {
                                    warn!("undefined mark '{}'\n{}",
                                           mark,
                                           parser::make_error(loc, "the meaning of this mark is not (yet) known"),
                                           );
                                }
                            }
                        }

                    }

                    let site = stack.local(Some(arg.typed.clone()),
                        Name::from(&format!("__builtin::callstack::{}", storage)), arg.loc.clone(), &arg.tags
                    );
                    stack.write(storage, Lifetime::Pointer(site), &arg.loc);
                }

                stack.check_block(body);


                stack.write(ptr, Lifetime::Function{
                    ret,
                    args: args.clone(),
                    vararg: *vararg,
                }, &local.loc);

                stack.pop(&body.end);
                stack.current_return_ptr = None;

            },
            _ => (),
        }
    }
    if ABORT.load(Ordering::Relaxed) {
        std::process::exit(9);
    }
}
