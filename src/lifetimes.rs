use super::ast;
use super::flatten;
use super::parser::{emit_error, emit_warn};
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
        ret:  Option<(Box<Lifetime>, ast::Typed)>,
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
                        emit_warn(format!("dropped local '{}' while still marked as '{}'", m.name, val), &[
                            (dropped_here.clone(), format!("'{}' will be dropped unclean", m.name)),
                            (vloc.clone(), format!("marked '{}' here. probably must call a related function to unset", val))
                        ]);
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
            emit_error(format!("redeclation of local name '{}'", name), &[
                (loc.clone(), "this declaration would shadow a previous name")
            ]);
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
                emit_error(format!("undefined name '{}'", name), &[
                    (used_here.clone(), format!("'{}' is not defined in this scope", name))
                ]);
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
                emit_error(format!("illegal read access to dropped value '{}'", storage.name), &[
                    (used_here.clone(),     "used here"),
                    (stored_here.clone(),   "points at this storage location"),
                    (dropped_here.clone(),  "which was dropped here"),
                ]);
                ABORT.store(true, Ordering::Relaxed);
                return Lifetime::Uninitialized;
            },
            _ => (),
        }

        if access == Access::Storage {
            return Lifetime::Pointer(pointer)
        }

        if storage.tags.contains_key("unsafe") {
            emit_error(format!("illegal read access to unsafe storage '{}'", storage.name), &[
                (used_here.clone(), "used here"),
                (storage.stored_here.clone(), "suggestion: add a runtime check for this value and mark it safe"),
            ]);
            ABORT.store(true, Ordering::Relaxed);
            return Lifetime::Uninitialized;
        }

        match &storage.value {
            Lifetime::Uninitialized => {
                emit_error(format!("illegal read access to unitialized local '{}'", storage.name), &[
                    (used_here.clone(), "used here")
                ]);
                ABORT.store(true, Ordering::Relaxed);
                return Lifetime::Uninitialized;
            }
            Lifetime::Moved{moved_here} => {
                emit_error(format!("illegal read access of moved value '{}'", storage.name), &[
                    (used_here.clone(),  "use of moved value"),
                    (moved_here.clone(), "was moved here")
                ]);
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
                    emit_error("uninitialized pointer arg passed as safe pointer", &[
                        (callsite.loc().clone(), "this pointer must be safe"),
                        (callsite_storage.stored_here.clone(), "but this value is unitialized"),
                    ]);

                    ABORT.store(true, Ordering::Relaxed);
                    return;
                },
                Lifetime::Dropped{stored_here, dropped_here} => {
                    emit_error(format!("passing dropped value '{}' as safe pointer", callsite_storage.name), &[
                        (callsite.loc().clone(),    "used here"),
                        (stored_here.clone(),       "points at this storage location"),
                        (dropped_here.clone(),      "which was dropped here"),
                    ]);
                    ABORT.store(true, Ordering::Relaxed);
                    return;
                },
                Lifetime::Moved{moved_here} => {
                    emit_error(format!("passing moved value '{}' as safe pointer", callsite_storage.name), &[
                        (callsite.loc().clone(),    "use of moved value"),
                        (moved_here.clone(),        "was moved here")
                    ]);
                    ABORT.store(true, Ordering::Relaxed);
                    return;
                },
                Lifetime::Function{..} => {
                    emit_error("ICE: trying to pass function as pointer", &[
                        (callsite.loc().clone(), "cannot determine lifetime of expression")
                    ]);
                    ABORT.store(true, Ordering::Relaxed);
                    return;
                },
                Lifetime::Static => {
                    if callsite_storage.tags.contains("safe") {
                        return;
                    }
                    if let Some(change) = &callsite_storage.changed_here {
                        emit_error("incompatible argument", &[
                            (callsite.loc().clone(), "this expression has a different pointer depth".to_string()),
                            (change.clone(), "value assigned here might not be a pointer".to_string()),
                            (stack.typed.loc.clone(), format!("expected at depth {} here", i)),
                        ]);
                    } else {
                        emit_error("incompatible argument", &[
                            (callsite.loc().clone(), "this expression has a different pointer depth".to_string()),
                            (stack.typed.loc.clone(), format!("expected at depth {} here", i)),
                        ]);
                    }
                    ABORT.store(true, Ordering::Relaxed);
                    return;
                }
            };
            callsite_storage = &mut self.pointers[callsite_ptr];


            if stack_ptr.tags.contains_key("mutable") && !callsite_storage.tags.contains_key("mutable") {
                emit_error("const pointer cannot be used as mut pointer in function call", &[
                       (callsite.loc().clone(), "this expression must yield a mutable pointer"),
                       (callsite_storage.stored_here.clone(), "suggestion: change this declaration to mutable"),
                ]);
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
                    emit_error(format!("missing required type state '{}'", val), &[
                           (callsite.loc().clone(), format!("argument here must be '{}'", val))
                    ]);
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
                    emit_error(format!("cannot use marked local '{}'", callsite_storage.name), &[
                        (callsite.loc().clone(), format!("argument here is not allowed to be '{}'", val)),
                        (vloc.clone(), format!("marked '{}' here. probably must call a related function to unset", val)),
                    ]);
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
                        emit_error("call would mark borrowed callsite", &[
                               (callsite.loc().clone(),
                               format!("this expression would mark the callsite with undeclared mark '{}'",
                                       missing.0)),
                               (callsite_storage.stored_here.clone(),
                               format!("try adding a set<{}> tag here",
                                       missing.0)),
                        ]);
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
                    emit_error("cannot move stack", &[
                           (callsite.loc().clone(),
                           format!("this expression would move '{}' out of scope, which is on the stack", callsite_storage.name)),
                    ]);
                    ABORT.store(true, Ordering::Relaxed);
                    return;
                }

                if let Some(_) = callsite_storage.tags.get("borrowed") {
                    emit_error("cannot move borrowed pointer", &[
                           (callsite.loc().clone(),
                           format!("this expression would move '{}' out of scope", callsite_storage.name)),
                           (tag.iter().next().unwrap().1.clone(), "required because this call argument is move".to_string()),
                           (callsite_storage.stored_here.clone(), "try changing this declaration to move".to_string()),
                    ]);
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
                emit_error("passing unsafe pointer to safe function call", &[
                       (callsite.loc().clone(), "this expression must be safe"),
                       (callsite_storage.stored_here.clone(), "suggestion: add a runtime check for this value and mark it safe"),
                ]);
                ABORT.store(true, Ordering::Relaxed);
                return;
            }
        }

        // leftmost pointer 
        match &callsite_storage.value {
            Lifetime::Uninitialized => {
                emit_error("uninitialized arg passed as safe", &[
                       (callsite.loc().clone(), "this arg must be safe"),
                       (callsite_storage.stored_here.clone(), "but this value is unitialized"),
                ]);
                ABORT.store(true, Ordering::Relaxed);
                return;
            },
            Lifetime::Dropped{stored_here, dropped_here} => {
                emit_error(format!("passing dropped value '{}' as safe pointer", callsite_storage.name), &[
                           (callsite.loc().clone(),"used here"),
                           (stored_here.clone(),   "points at this storage location"),
                           (dropped_here.clone(),  "which was dropped here"),
                ]);
                ABORT.store(true, Ordering::Relaxed);
                return;
            },
            Lifetime::Moved{moved_here} => {
                emit_error(format!("passing moved value '{}' as safe pointer", callsite_storage.name), &[
                       (callsite.loc().clone(), "use of moved value"),
                       (moved_here.clone(), "was moved here"),
                ]);
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
                                        emit_error("lvalue expression is not a storage location", &[
                                                   (lhs.loc().clone(), "cannot be accessed"),
                                                   (assigned_here.clone(), "value assigned here is not a struct"),
                                        ]);
                                    } else {
                                        emit_error("lvalue expression is not a storage location", &[
                                                   (lhs.loc().clone(), "cannot be accessed"),
                                        ]);
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
                        emit_error("lvalue expression is not a storage location}", &[
                               (lhs.loc().clone(), "cannot be accessed"),
                        ]);
                        ABORT.store(true, Ordering::Relaxed);
                        return Lifetime::Uninitialized;
                    }
                };
                let mut storage = self.pointers.get(ptr).unwrap().clone();
                if storage.tags.contains_key("unsafe") {
                    emit_error(format!("illegal read access to unsafe storage '{}'", storage.name), &[
                           (loc.clone(), "used here"),
                           (storage.stored_here.clone(), "suggestion: add a runtime check for this value and mark it safe"),
                    ]);
                    ABORT.store(true, Ordering::Relaxed);
                    return Lifetime::Uninitialized;
                }


                match &storage.typ {
                    None => {
                        emit_error("unknown type cannot be accessed", &[
                               (lhs.loc().clone(), format!("type of '{}' is not known", storage.name)),
                        ]);
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
                                emit_error(format!("struct '{}' has no member named '{}'", t.name, rhs), &[
                                    (loc.clone(), format!("unresolveable member access of '{}' here", storage.name))
                                ]);
                                ABORT.store(true, Ordering::Relaxed);
                                return Lifetime::Uninitialized;
                            },
                            _ =>  {
                                emit_error(format!("'{}' is not a struct", t.name), &[
                                       (lhs.loc().clone(), format!("'{}' is not accessible as struct", t.name))
                                ]);
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
                    emit_error("lvalue expression is not a storage location", &[
                           (loc.clone(), "literal cannot be used as lvalue"),
                    ]);
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
                            emit_error("call argument count mismatch", &[
                                (   name.loc.clone(),
                                    format!("this function expects {} arguments, but you passed {}",
                                            args.len(),
                                            callargs.len() ))
                            ]);
                            ABORT.store(true, Ordering::Relaxed);
                            return Lifetime::Uninitialized;
                        }
                        for i in 0..args.len() {
                            // check value of expression, which will be copied into the functions scope
                            self.check_call_arg(&args[i], &mut callargs[i])
                        }

                        return match ret {
                            Some(ret) => {
                                let (return_scope_lf, ret_typ)  = ret;
                                let mut return_scope_lf = *return_scope_lf;
                                let mut callsite_lf = return_scope_lf.clone();

                                loop {
                                    if let Lifetime::Pointer(return_scope_ptr) = return_scope_lf {
                                        let tags = &self.pointers[return_scope_ptr].tags.clone();
                                        return_scope_lf = self.pointers[return_scope_ptr].value.clone();

                                        let callsite_ptr = self.local(Some(ret_typ.clone()), Name::from(
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
                        emit_warn("lvalue is not a valid function", &[
                               (name.loc.clone(), "this expression cannot be used as function"),
                        ]);
                        return Lifetime::Uninitialized;
                    }
                }
            }
            ast::Expression::InfixOperation { lhs, rhs, loc} => {
                if access == Access::Storage {
                    emit_error("value expression is not a storage location", &[
                        (loc.clone(), "this expression cannot be used as lvalue")
                    ]);
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
                                    emit_error(
                                        "dereferencing something that is not a pointer",
                                        &[(expr.loc().clone(), "cannot determine lifetime of expression")]
                                        );
                                    return Lifetime::Uninitialized;
                                }
                            };
                            let v_store = self.pointers.get_mut(v_ptr).unwrap();
                            if let Some(change) = &v_store.changed_here {
                                emit_error("dereferencing something that is not a pointer", &[
                                    (expr.loc().clone(), "cannot determine lifetime of expression"),
                                    (change.clone(), "this assignment does not make a valid pointer")
                                ]);
                            } else {
                                emit_error("dereferencing something that is not a pointer", &[
                                    (expr.loc().clone(), "cannot determine lifetime of expression"),
                                ]);
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
                let mut combined_lf = Lifetime::Static;

                for (_, field) in fields {
                    match self.check_expr(field, Access::Value) {
                        Lifetime::Static => {
                        },
                        Lifetime::Pointer(ptr) => {
                            // TODO combined lifetimes dont actually exit yet, so we just use the
                            // last field
                            combined_lf = Lifetime::Pointer(ptr);
                        }
                        _ => {
                            emit_warn("cannot determinte lifetime of field initialization", &[
                                (field.loc().clone(), "this assignment is untraceable")
                            ]);
                        }
                    }
                }
                combined_lf
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

                        } else if key == "clean" || key == "pure"{
                            storage.tags.remove("marked", None);
                        } else {
                            storage.tags.insert(key.clone(), value.clone(), loc.clone());
                        }

                        *self.pointers.get_mut(to).unwrap() = storage;
                    },
                    _ => {
                        emit_error("lvalue is not a storage location", &[
                            (lhs.loc().clone(), "left hand side doesn't name something with a lifetime")
                        ]);
                        ABORT.store(true, Ordering::Relaxed);
                    },
                };
            }
            ast::Statement::CBlock{..} => {
            }
            ast::Statement::Unsafe(_) => {
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
            ast::Statement::Var{name, assign, tags, loc, typed, array, ..} => {
                let mut tags = tags.clone();
                tags.insert("stack".to_string(), String::new(), loc.clone());
                let ptr = self.local(Some(typed.clone()), Name::from(&*name), loc.clone(), tags);

                if let Some(assign) = assign {
                    let rhs_rf = self.check_expr(assign, Access::Value);
                    match rhs_rf {
                        Lifetime::Uninitialized => {
                            emit_error("rvalue has unknown lifetime", &[
                                (assign.loc().clone(), "cannot determine lifetime of right hand side")
                            ]);

                        }
                        _ => (),
                    };
                    self.write(ptr, rhs_rf, loc);
                }
                if let Some(array) = array {
                    if let Some(expr) = array {
                        self.check_expr(expr, Access::Value);
                    }
                }
            },
            ast::Statement::Assign{lhs, rhs, loc, ..} => {
                let rhs_rf = self.check_expr(rhs, Access::Value);
                match rhs_rf {
                    Lifetime::Uninitialized => {
                        emit_error("rvalue has invalid lifetime", &[
                            (rhs.loc().clone(), "cannot determine lifetime of right hand side")
                        ]);
                    }

                    _ => (),
                };

                let lhs_lf = self.check_expr(lhs, Access::Storage);
                match lhs_lf {
                    Lifetime::Pointer(to)  => {
                        let storage = &self.pointers.get(to).expect("ICE: invalid pointer");
                        let tags = &storage.tags;
                        if !tags.contains_key("mutable") {
                            emit_error("cannot assign to immutable storage", &[
                                (lhs.loc().clone(), "lvalue expression must be mutable"),
                                (self.pointers[to].stored_here.clone(), "suggestion: change this declaration to mutable"),
                            ]);
                            ABORT.store(true, Ordering::Relaxed);
                        }
                        self.write(to, rhs_rf, lhs.loc());
                    },
                    _ => {
                        emit_error("lvalue has invalid lifetime", &[
                                   (lhs.loc().clone(), "cannot determine lifetime of left hand side"),
                        ]);
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
                    emit_error("function returns orphaning moved pointer", &[
                           (callloc.clone(), "the call moves a return value into scope"),
                           (loc.clone(), "but will be orphaned here"),
                    ]);
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


    for (local,_) in &mut md.d {
        let localname = Name::from(&local.name);
        debug!("   def {}", localname);

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
            ast::Def::Static{typed, expr, storage, tags, array} => {
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

                        Some((Box::new(rlf), ret.typed.clone()))
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
