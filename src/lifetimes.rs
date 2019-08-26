use super::ast;
use super::flatten;
use super::parser;
use std::collections::HashMap;
use super::name::Name;
use super::ast::Tags;


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
    tags:           Tags,
    stored_here:    ast::Location,
    changed_here:   Option<ast::Location>,
    value:          Lifetime,
    array:          HashMap<usize, usize>,
}

struct Scope {
    name:       String,
    locals:     HashMap<Name, usize>
}

#[derive(Default)]
struct Stack {
    defs:       HashMap<Name, ast::Def>,

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
        let mut dead : Vec<usize> = dead.locals.iter().map(|(_,ptr)|*ptr).collect();
        for d in dead.clone() {
            let m = self.pointers.get_mut(d).unwrap();
            for (_,ar) in &m.array {
                dead.push(*ar);
            }
        }

        for d in dead {
            let m = self.pointers.get_mut(d).unwrap();

            if !m.tags.contains_key("borrowed") {
                if let Some(vals) = m.tags.get("marked") {
                    for (val,vloc) in vals {
                        warn!("dropped local '{}' while still marked as '{}'\n{}\n{}", m.name, val,
                              parser::make_error(&dropped_here, format!("'{}' will be dropped unclean", m.name)),
                              parser::make_error(&vloc, format!("marked '{}' here. probably must call a related function to unset", val)),
                              );
                    }
                }
            }

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

    fn local(&mut self, typ: Option<ast::Typed>, name: Name, loc: ast::Location, tags: Tags) -> usize {
        if let Some(ptr) = self.cur().locals.get(&name) {
            error!("redeclation of local name '{}'\n{}", name,
                   parser::make_error(&loc, "this declaration would shadow a previous name"),
                   );
            ABORT.store(true, Ordering::Relaxed);
        }


        let ptr  = self.pointers.len();
        self.pointers.push(Storage{
            typ,
            name:           name.clone(),
            stored_here:    loc,
            value:          Lifetime::Uninitialized,
            tags:           tags,
            changed_here:   None,
            array:          HashMap::new(),
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
                error!("illegal read access to unitialized local {}\n{}",
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
                    if callsite_storage.tags.contains("safe") {
                        return;
                    }
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

            if let Some(vals) = stack_ptr.tags.get("require") {
                let mut missing = vals.clone();
                if let Some(v) = callsite_storage.tags.get("marked") {
                    for (v,_) in v {
                        missing.remove(v);
                    }
                }
                for (val,_) in missing {
                    error!("missing required type state '{}'\n{}",
                           val,
                           parser::make_error(&callsite.loc(),
                           format!("argument here must be '{}'", val)),
                           );
                    ABORT.store(true, Ordering::Relaxed);
                    return;
                }

            }

            if let Some(v) = stack_ptr.tags.get("unset") {
                for (v,_) in v {
                    callsite_storage.tags.remove("marked", Some(v));
                }
            }

            if let Some(vals) = callsite_storage.tags.get("marked") {

                let mut missing = vals.clone();
                if let Some(v) = stack_ptr.tags.get("require") {
                    for (v,_) in v {
                        missing.remove(v);
                    }
                }
                if let Some(v) = stack_ptr.tags.get("allow") {
                    for (v,_) in v {
                        missing.remove(v);
                    }
                }

                for (val,vloc) in missing {
                    error!("cannot use marked local '{}'\n{}\n{}",
                           callsite_storage.name,
                           parser::make_error(&callsite.loc(),
                           format!("argument here is not allowed to be '{}'", val)),
                           parser::make_error(&vloc, format!("marked '{}' here. probably must call a related function to unset", val)),
                           );
                    ABORT.store(true, Ordering::Relaxed);
                    return;
                }
            }

            if let Some(tag) = stack_ptr.tags.get("set") {
                if callsite_storage.tags.contains_key("borrowed") {
                    let mut taint_missing  = tag.clone();
                    if let Some(tag2) = callsite_storage.tags.get("set") {
                        for (value,_) in tag2 {
                            taint_missing.remove(value);
                        }
                    }
                    for missing in taint_missing {
                        error!("call would mark borrowed callsite\n{}\n{}",
                               parser::make_error(&callsite.loc(),
                               format!("this expression would mark the callsite with undeclared mark '{}'",
                                       missing.0)),
                                       parser::make_error(&callsite_storage.stored_here,
                                                          format!("try adding a set<{}> tag here",
                                                                  missing.0)),
                                                                  );
                        ABORT.store(true, Ordering::Relaxed);
                        return;
                    }
                }

                for (val,_) in tag {
                    callsite_storage.tags.insert(
                        "marked".to_string(),
                        val.clone(),
                        callsite.loc().clone(),
                    );
                }
            }

            if let Some(tag) = stack_ptr.tags.get("move") {
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
                           parser::make_error(&tag.iter().next().unwrap().1, "required because this call argument is move"),
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

        // leftmost pointer 
        match &callsite_storage.value {
            Lifetime::Uninitialized => {
                error!("uninitialized arg passed as safe \n{}\n{}",
                       parser::make_error(&callsite.loc(), "this arg must be safe"),
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
            _ => (),
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
            ast::Expression::MemberAccess {loc, lhs, rhs, op, ..} => {
                let ptr = match self.check_expr(lhs, Access::Storage) {
                    Lifetime::Pointer(to) => {
                        if op == "->" {
                            match self.read(to, loc, Access::Value) {
                                Lifetime::Pointer(to) => {
                                    to
                                }
                                Lifetime::Uninitialized | Lifetime::Moved{..} => {
                                    ABORT.store(true, Ordering::Relaxed);
                                    return Lifetime::Uninitialized;
                                }
                                _ => {
                                    if let Some(assigned_here) = &self.pointers.get(to).unwrap().changed_here {
                                        error!("lvalue expression is not a storage location\n{}\n{}",
                                               parser::make_error(&lhs.loc(), "cannot be accessed"),
                                               parser::make_error(assigned_here, "value assigned here is not a struct"),
                                               );
                                    } else {
                                        error!("lvalue expression is not a storage location\n{}",
                                               parser::make_error(&lhs.loc(), "cannot be accessed"),
                                               );
                                    }
                                    ABORT.store(true, Ordering::Relaxed);
                                    return Lifetime::Uninitialized;
                                }
                            }
                        } else {
                            to
                        }
                    },
                    _ => {
                        error!("lvalue expression is not a storage location\n{}",
                               parser::make_error(&lhs.loc(), "cannot be accessed"),
                               );
                        ABORT.store(true, Ordering::Relaxed);
                        return Lifetime::Uninitialized;
                    }
                };
                let mut storage = self.pointers.get(ptr).unwrap().clone();
                if storage.tags.contains_key("unsafe") {
                    error!("illegal read access to unsafe storage {}\n{}\n{}",
                           storage.name,
                           parser::make_error(&loc, "used here"),
                           parser::make_error(&storage.stored_here, "suggestion: add a runtime check for this value and mark it safe"),
                           );
                    ABORT.store(true, Ordering::Relaxed);
                    return Lifetime::Uninitialized;
                }


                match &storage.typ {
                    None => {
                        error!("unknown type cannot be accessed\n{}",
                               parser::make_error(&lhs.loc(), format!("type of '{}' is not known", storage.name)),
                               );
                        ABORT.store(true, Ordering::Relaxed);
                        return Lifetime::Uninitialized;
                    },
                    Some(t) => {
                        if t.name.0[1] == "ext" {
                            return Lifetime::Static;
                        }

                        let def = self.defs.get(&t.name).cloned();
                        match def {
                            Some(ast::Def::Struct{fields,..}) => {
                                for (i, field) in fields.iter().enumerate() {
                                    if &field.name == rhs {
                                        if !storage.array.contains_key(&i) {
                                            let mut tags = field.tags.clone();

                                            let m_ptr  = self.pointers.len();
                                            self.pointers.push(Storage{
                                                typ:            Some(field.typed.clone()),
                                                name:           Name::from(
                                                    &format!("member access of {}@{}->{}", storage.name, ptr, field.name)
                                                ),
                                                stored_here:    field.loc.clone(),
                                                value:          Lifetime::Uninitialized,
                                                tags:           tags.clone(),
                                                changed_here:   None,
                                                array:          HashMap::new(),
                                            });
                                            storage.array.insert(i, m_ptr);
                                            *self.pointers.get_mut(ptr).unwrap() = storage.clone();

                                            if field.typed.ptr.len() > 0 {
                                                tags.insert("unsafe".to_string(), String::new(), loc.clone());
                                                let ptr3 = self.pointers.len();
                                                self.pointers.push(Storage{
                                                    typ:            Some(field.typed.clone()),
                                                    name:           Name::from(
                                                        &format!("member access to unsafe memory {}@{}->{}",
                                                                 storage.name, ptr3, field.name)
                                                        ),
                                                        stored_here:    field.loc.clone(),
                                                        value:          Lifetime::Uninitialized,
                                                        tags:           tags.clone(),
                                                        changed_here:   None,
                                                        array:          HashMap::new(),
                                                });
                                                self.write(m_ptr, Lifetime::Pointer(ptr3), &field.loc);
                                            } else {
                                                self.write(m_ptr, Lifetime::Static, &field.loc);
                                            }

                                        }

                                        return self.read(*storage.array.get(&i).unwrap(), loc, access);
                                    }
                                }
                                error!("struct '{}' has no member named '{}' \n{}",
                                       t.name, rhs,
                                       parser::make_error(&loc, format!("unresolveable member access of '{}' here", storage.name)),
                                       );
                                ABORT.store(true, Ordering::Relaxed);
                                return Lifetime::Uninitialized;
                            },
                            _ =>  {
                                error!("{} is not a struct\n{}",
                                       t.name,
                                       parser::make_error(&lhs.loc(), format!("'{}' is not accessible as struct", t.name)),
                                       );
                                ABORT.store(true, Ordering::Relaxed);
                                return Lifetime::Uninitialized;
                            },
                        }
                    }
                };
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
                            if let Some(cs) = args[i].tags.get("callsite_macro") {
                                let (v, loc) = cs.iter().next().unwrap();
                                if i > callargs.len() { break }
                                callargs.insert(i, Box::new(ast::Expression::Literal{
                                    loc: name.loc.clone(),
                                    v:   v.clone(),
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
                                                tags.clone());

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
            ast::Expression::UnaryPre {expr, op, loc}=> {
                if op == "&" {

                    let lf = self.check_expr(expr, Access::Storage);
                    let temp_ptr = self.local(None, Name::from(
                            &format!("temporary access {}", self.pointers.len())),
                            expr.loc().clone(),
                            Tags::new());
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
                            let v_ptr = match self.check_expr(expr, Access::Storage) {
                                Lifetime::Pointer(u) => u,
                                _ => {
                                    error!("dereferencing something that is not a pointer\n{}",
                                           parser::make_error(expr.loc(), "cannot determine lifetime of expression"),
                                           );
                                    return Lifetime::Uninitialized;
                                }
                            };
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
                    self.check_expr(expr, access)
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
            ast::Statement::Mark{lhs, key, value, loc} => {
                let lhs_lf = self.check_expr(lhs, Access::Storage);
                match lhs_lf {
                    Lifetime::Pointer(to)  => {
                        let xep = self.pointers.len();
                        let mut storage = self.pointers.get(to).unwrap().clone();

                        if key == "safe" {
                            storage.tags.remove("unsafe", None);
                            storage.tags.insert("safe".to_string(), String::new(), loc.clone());

                            storage.value = Lifetime::Static;
                            storage.changed_here = Some(loc.clone());
                            if let Some(typ) = &storage.typ {
                                if typ.ptr.len() > 0 {
                                    let site = self.local(
                                        Some(typ.clone()),
                                        Name::from(&format!("safe pointer @{}", xep)),
                                        typ.ptr[0].loc.clone(), typ.ptr[0].tags.clone()
                                        );
                                    storage.value = Lifetime::Pointer(site);
                                }
                            }

                        } else if key == "pure" {
                            storage.tags.remove("marked", None);
                        } else {
                            storage.tags.insert(key.clone(), value.clone(), loc.clone());
                        }

                        *self.pointers.get_mut(to).unwrap() = storage;
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
                for stm in e1 {
                    self.check_stm(stm)
                }
                for stm in e2 {
                    self.check_stm(stm)
                }
                for stm in e3 {
                    self.check_stm(stm)
                }
                self.check_block(body);
                self.pop(&body.end);
            }
            ast::Statement::Expr{loc, expr} => {
                self.check_expr(expr, Access::Value);
            }
            ast::Statement::Var{name, assign, tags, loc, typed, ..} => {
                let mut tags = tags.clone();
                tags.insert("stack".to_string(), String::new(), loc.clone());
                let ptr = self.local(Some(typed.clone()), Name::from(&*name), loc.clone(), tags);

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
            ast::Statement::Switch{loc, expr, cases, default} => {
                let val = self.check_expr(expr, Access::Value);
                for (expr, block) in cases {
                    let val = self.check_expr(expr, Access::Value);
                    self.push("block");
                    self.check_block(block);
                    self.pop(&block.end);
                }
                if let Some(default) = default {
                    self.check_block(default);
                }
            }
            ast::Statement::Goto{..}
            | ast::Statement::Label{..}
            | ast::Statement::Break{..}
            | ast::Statement::Continue{..}
            => {},
        }
    }
}




pub fn check(md: &mut flatten::Module) {
    debug!("lifetime checking {}", md.name);

    let mut stack = Stack::default();
    stack.push("static");

    for (name,loc) in &md.c_names {
        let mut tags = Tags::new();
        let ptr = stack.local(None, name.clone(), loc.clone(), tags);
        stack.write(ptr, Lifetime::Static, &loc);
    }

    for d in &mut md.d {
        let local  = match d {
            flatten::D::Include(_) => continue,
            flatten::D::Local(v) => v,
        };
        stack.defs.insert(Name::from(&local.name), local.def.clone());
        match &mut local.def {
            ast::Def::Macro{args, body} => {
                let localname = Name::from(&local.name);
                let ptr = stack.local(None, localname.clone(), local.loc.clone(), Tags::new());
                stack.write(ptr, Lifetime::Static, &local.loc);
            },
            ast::Def::Const{typed, expr} => {
                let localname = Name::from(&local.name);
                let ptr = stack.local(Some(typed.clone()), localname.clone(), local.loc.clone(), Tags::new());
                stack.write(ptr, Lifetime::Static, &local.loc);
            },
            ast::Def::Static{typed, expr, storage, tags} => {
                let localname = Name::from(&local.name);
                let ptr = stack.local(Some(typed.clone()), localname.clone(), local.loc.clone(), tags.clone());
                stack.write(ptr, Lifetime::Static, &local.loc);
            },
            ast::Def::Enum{names, ..} => {
                for (name,_) in names {
                    let mut localname = Name::from(&local.name);
                    localname.push(name.clone());
                    let ptr = stack.local(None, localname.clone(), local.loc.clone(), Tags::new());
                    stack.write(ptr, Lifetime::Static, &local.loc);
                }
            },
            ast::Def::Function{body, args, ret, vararg, ..} => {

                let localname = Name::from(&local.name);
                let ptr = stack.local(None, localname.clone(), local.loc.clone(), Tags::new());

                let ret = match ret {
                    None => None,
                    Some(ret) => {
                        let mut rlf = Lifetime::Static;

                        for ast_ptr in ret.typed.ptr.iter() {
                            let mut tags = ast_ptr.tags.clone();
                            if !tags.contains_key("move") {
                                tags.insert("borrowed".to_string(), String::new(), ret.typed.loc.clone());
                            }
                            let storage = stack.local(
                                Some(ret.typed.clone()),
                                Name::from(&format!("return value of {}", local.name)),
                                ret.typed.loc.clone(), tags);
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

                    let mut storage = stack.local(Some(arg.typed.clone()), argname.clone(), arg.loc.clone(), arg.tags.clone());

                    for ast_ptr in arg.typed.ptr.iter().rev() {
                        let mut body_tags = ast_ptr.tags.clone();
                        if !body_tags.contains_key("move") {
                            body_tags.insert("borrowed".to_string(), String::new(), arg.loc.clone());
                        }
                        let site = stack.local(
                            Some(arg.typed.clone()),
                            Name::from(&format!("pointer to callsite of {} ({})", local.name, storage)), arg.loc.clone(), body_tags.clone()
                        );
                        stack.write(storage, Lifetime::Pointer(site), &arg.loc);
                        storage = site;
                    }

                    let site = stack.local(Some(arg.typed.clone()),
                        Name::from(&format!("callsite of {}({})",local.name, storage)), arg.loc.clone(), arg.tags.clone()
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
