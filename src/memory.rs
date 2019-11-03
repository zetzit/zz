use super::ast;
use super::flatten;
use super::parser::{emit_error, emit_warn, emit_debug};
use std::collections::HashMap;
use super::name::Name;
use super::ast::Tags;


use std::sync::atomic::{AtomicBool, Ordering};
static ABORT: AtomicBool = AtomicBool::new(false);



type Address = usize;
#[derive(Debug)]
enum Error {
    Error {
        message:    String,
        details:    Vec<(ast::Location, String)>,
    },
    Untrackable(String),
}

impl Error {
    pub fn new(message: String, details:    Vec<(ast::Location, String)>) -> Self {
        Self::Error {
            message,
            details,
        }
    }
}

#[derive(Clone)]
enum Value {
    Void,
    Uninitialized,
    Untrackable(String),
    Literal(String),
    Address(Address),
    Array(Vec<Address>),
    Struct(HashMap<String, Address>),
    Function {
        name:   Name,
        ret:    Option<(Address, ast::Typed)>,
        args:   Vec<ast::NamedArg>,
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

impl Value {
    pub fn as_pointer(&self) -> usize {
        match self {
            Value::Address(u) => *u,
            _ => panic!("ICE: not a pointer"),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Void              => write!(f, "void"),
            Value::Uninitialized     => write!(f, "uninitialized"),
            Value::Untrackable(s)    => write!(f, "untrackable: {}", s),
            Value::Literal(s)        => write!(f, "literal({})", s),
            Value::Struct(_)         => write!(f, "struct"),
            Value::Array(_)          => write!(f, "array"),
            Value::Address(to)       => write!(f, "ptr->{}", to),
            Value::Function {..}     => write!(f, "function"),
            Value::Moved{..}         => write!(f, "moved"),
            Value::Dropped{..}       => write!(f, "dropped"),
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
    value:          Value,
}

struct Scope {
    name:       String,
    locals:     HashMap<Name, usize>
}

#[derive(Default)]
struct Stack {
    defs:       HashMap<Name, ast::Def>,

    storage:   Vec<Storage>,
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
            let m = self.storage.get_mut(d).unwrap();
            if let Value::Struct(ref fields) = m.value{
                for (_,ar) in fields {
                    dead.push(*ar);
                }
            }
        }

        for d in dead {
            let m = self.storage.get_mut(d).unwrap();

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

            m.value = Value::Dropped {
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

    fn local(&mut self, typ: Option<ast::Typed>, name: Name, loc: ast::Location, tags: Tags) -> Address {
        if let Some(_) = self.cur().locals.get(&name) {
            emit_error(format!("redeclation of local name '{}'", name), &[
                (loc.clone(), "this declaration would shadow a previous name")
            ]);
            ABORT.store(true, Ordering::Relaxed);
        }


        let ptr  = self.storage.len();
        self.storage.push(Storage{
            typ,
            name:           name.clone(),
            stored_here:    loc.clone(),
            value:          Value::Uninitialized,
            tags:           tags,
            changed_here:   None,
        });

        debug!("    let {} = {}", name, ptr);

        self.cur().locals.insert(name, ptr);

        self.initialize(ptr, &loc, false);

        ptr
    }


    fn check_name(&mut self, name: &Name, used_here: &ast::Location) -> Result<Address, Error> {
        match self.find(&name) {
            None => {
                return Err(Error::new(format!("undefined name '{}'", name), vec![
                    (used_here.clone(), format!("'{}' is not defined in this scope", name))
                ]));
            },
            Some(v) => Ok(v),
        }
    }

    /*
    fn read(&self, pointer: usize, used_here: &ast::Location) -> Result<SafeValue, Error> {
        let storage = self.storage.get(pointer).expect("ICE: invalid pointer");

        match &storage.value {
            Value::Dropped{stored_here, dropped_here} => {
                return Err(Error::new(format!("illegal read access to dropped value '{}'", storage.name), vec![
                    (used_here.clone(),     "used here".to_string()),
                    (stored_here.clone(),   "points at this storage location".to_string()),
                    (dropped_here.clone(),  "which was dropped here".to_string()),
                ]));
            },
            _ => (),
        }

        if storage.tags.contains_key("unsafe") {
            return Err(Error::new(format!("illegal read access to unsafe storage '{}'", storage.name), vec![
                (used_here.clone(), "used here".to_string()),
                (storage.stored_here.clone(), "suggestion: add a runtime check for this value and mark it safe".to_string()),
            ]));
        }

        match &storage.value {
            Value::Uninitialized => {
                return Err(Error::new(format!("illegal read access to unitialized local '{}'", storage.name), vec![
                    (used_here.clone(), "used here".to_string())
                ]));
            }
            Value::Moved{moved_here} => {
                return Err(Error::new(format!("illegal read access of moved value '{}'", storage.name), vec![
                    (used_here.clone(),  "use of moved value".to_string()),
                    (moved_here.clone(), "was moved here".to_string())
                ]));
            }
            Value::Address(p) => {
                return Ok(SafeValue::Address(*p));
            }
            o => {
                return Err(Error::new(format!("illegal read access of {} '{}'", o, storage.name), vec![
                    (used_here.clone(),  "used in this context".to_string()),
                ]));
            }
        }
    }
    */

    fn write(&mut self, pointer: usize, val: Value, used_here: &ast::Location) {
        let storage = self.storage.get_mut(pointer).expect("ICE: invalid pointer");
        debug!("    {} {} <= {}", storage.name, pointer, val);
        storage.value = val.clone();
        storage.changed_here = Some(used_here.clone());
    }

    fn initialize(&mut self, to: usize, loc: &ast::Location, deep : bool) {
        let storage = self.storage[to].clone();
        if let Some(totyp) = storage.typ {
            if totyp.ptr.len() > 0 {
                if deep {
                    let mut depth = totyp.clone();
                    let ptrt = depth.ptr.pop().unwrap();
                    let mut tags = Tags::new();
                    tags.insert("unsafe".to_string(), String::new(), loc.clone());

                    let temp_ptr = self.local(Some(depth.clone()), Name::from(
                            &format!("safe mark of {} ({})", storage.name, self.storage.len())),
                            loc.clone(),
                            ptrt.tags.clone());
                    self.write(to, Value::Address(temp_ptr), loc);
                }
                return;
            }
            let def = self.defs.get(&totyp.name).cloned();
            match def {
                Some(ast::Def::Struct{fields,..}) => {
                    let mut value = HashMap::new();
                    for field in fields.iter() {
                        let temp_ptr = self.local(Some(field.typed.clone()), Name::from(
                                &format!("struct member {}.{} {}", totyp.name, field.name, self.storage.len())),
                                loc.clone(),
                                Tags::new());
                        self.initialize(temp_ptr, loc, true);
                        value.insert(field.name.clone(), temp_ptr);
                        if let Some(mut a) = field.array.clone() {
                            match self.check_expr(&mut a) {
                                Err(_) => (),
                                Ok(a_ptr) => {
                                    match &self.storage[a_ptr].value {
                                        Value::Literal(a) => {
                                            if let Ok(a) = a.parse::<u64>() {
                                                let mut ar = Vec::new();
                                                for _ in 0..a {
                                                    let temp_ptr2 = self.local(Some(field.typed.clone()), Name::from(
                                                            &format!("deep init array {}", self.storage.len())),
                                                            loc.clone(),
                                                            Tags::new());
                                                    ar.push(temp_ptr2);
                                                }
                                                self.write(temp_ptr, Value::Array(ar), loc);
                                            }
                                        },
                                        _ => (),
                                    }
                                }
                            }
                        }
                    }
                    self.write(to, Value::Struct(value), loc);
                },
                _ =>  {
                    if deep {
                        self.write(to, Value::Untrackable("deep init from safe assertion".to_string()), loc);
                    }
                },
            };
        }
    }



    fn copy(&mut self, from: Address, to: Address, used_here: &ast::Location) -> Result<(), Error> {
        emit_debug("copy",&[(used_here.clone(), format!("this will copy ({}|{})={} over ({}|{})={}",
            from, self.storage[from].name, self.storage[from].value,
            to, self.storage[to].name, self.storage[to].value,
        ))]);

        let from = &mut self.storage[from].clone();

        if from.tags.contains_key("unsafe") {
            return Err(Error::new(format!("illegal read access to unsafe storage '{}'", from.name), vec![
                (used_here.clone(), "used here".to_string()),
                (from.stored_here.clone(), "suggestion: add a runtime check for this value and mark it safe".to_string()),
            ]));
        }

        match &from.value {
            Value::Void => {
                return Err(Error::new(format!("void is not a value: '{}'", from.name), vec![
                    (used_here.clone(), "used here".to_string())
                ]));
            }
            Value::Uninitialized => {
                return Err(Error::new(format!("unsafe read access to possibly uninitialized local '{}'", from.name), vec![
                    (used_here.clone(), "used here".to_string())
                ]));
            }
            Value::Moved{ref moved_here} => {
                return Err(Error::new(format!("illegal read access of moved value '{}'", from.name), vec![
                    (used_here.clone(),  "use of moved value".to_string()),
                    (moved_here.clone(), "was moved here".to_string())
                ]));
            }
            Value::Function{..} => {
                //return Err(Error::new(format!("function cannot be copied '{}'", from.name), vec![
                //    (used_here.clone(),  "maybe you wanted to take the address instead".to_string()),
                //]));
            },
            Value::Struct(_) => {
                match self.storage[to].value.clone() {
                    Value::Array(_) => {
                        return Err(Error::new(format!("copying struct over array"), vec![
                            (used_here.clone(),  "confusing assignment".to_string()),
                        ]));
                    },
                    _ => (),
                }
            },
            Value::Array(ref af) => {
                match self.storage[to].value.clone() {
                    Value::Struct(fields) => {
                        for (i,f) in fields.keys().enumerate() {
                            if i < af.len() {
                                self.copy(af[i], fields[f], used_here)?;
                            } else {
                                self.write(fields[f], Value::Literal(String::from("0")), used_here);
                            }
                        }
                        return Ok(());
                    },
                    Value::Array(fields) => {
                        for (i,f) in fields.iter().enumerate() {
                            if i < af.len() {
                                self.copy(af[i], *f, used_here)?;
                            }
                        }
                        return Ok(());
                    }
                    _ => (),
                }
            }
            _ => {
            }
        }
        self.write(to, from.value.clone(), used_here);
        Ok(())
    }




    fn deref(&mut self, address: usize, used_here: &ast::Location) -> Result<Address, Error> {
        let storage = &self.storage[address];

        if storage.tags.contains_key("unsafe") {
            return Err(Error::new(format!("illegal deref of unsafe pointer '{}'", storage.name), vec![
                (used_here.clone(), "used here".to_string()),
                (storage.stored_here.clone(), "suggestion: add a runtime check for this storage and mark it safe".to_string()),
            ]));
        }

        match &storage.value {
            Value::Address(to)  => {
                return Ok(*to);
            }
            Value::Uninitialized => {
                return Err(Error::new(format!("unsafe deref of possibly uninitialized local '{}'", storage.name), vec![
                    (used_here.clone(), "used here".to_string())
                ]));
            }
            Value::Moved{moved_here} => {
                return Err(Error::new(format!("illegal deref of moved value '{}'", storage.name), vec![
                    (used_here.clone(),  "use of moved value".to_string()),
                    (moved_here.clone(), "was moved here".to_string())
                ]));
            }
            Value::Dropped{stored_here, dropped_here} => {
                return Err(Error::new(format!("illegal deref of  dropped value '{}'", storage.name), vec![
                    (used_here.clone(),     "used here".to_string()),
                    (stored_here.clone(),   "points at this storage location".to_string()),
                    (dropped_here.clone(),  "which was dropped here".to_string()),
                ]));
            },
            x => {
                return Err(Error::new(format!("illegal deref of {} '{}'", x, storage.name), vec![
                    (used_here.clone(), "used here".to_string())
                ]));
            }
        }
    }


    fn check_call_arg(&mut self, stack: &ast::NamedArg, callsite: &mut ast::Expression) -> Result<(), Error> {
        let mut callsite_ptr     = self.check_expr(callsite)?;
        let mut callsite_storage = &mut self.storage[callsite_ptr];

        let mut it = stack.typed.ptr.iter().rev().enumerate();
        loop {
            let (i, stack_ptr) = match it.next() {
                Some(v) => v,
                None => break,
            };

            // stack_ptr is now the last pointer in the functions stack argument
            // i.e it is "mut*" if the arg was "int ** mut* a"
            // we dig deeper into the callsite as we move right to left

            callsite_ptr = match &callsite_storage.value {
                Value::Address(to) => {
                    *to
                },
                Value::Untrackable(s) => {
                    return Err(Error::Untrackable(s.clone()));
                }
                Value::Struct{..} => {
                    return Err(Error::new("complex object passed by value".to_string(), vec![
                        (callsite.loc().clone(), "used as value here".to_string()),
                        (callsite_storage.stored_here.clone(), "but this expression is complex".to_string()),
                    ]));
                }
                Value::Array{..} => {
                    // C has this weird semantic that passing an array by value will make it a pointer
                    callsite_ptr
                },
                Value::Void => {
                    return Err(Error::new("void used as value".to_string(), vec![
                        (callsite.loc().clone(), "used as value here".to_string()),
                        (callsite_storage.stored_here.clone(), "but this expression is void".to_string()),
                    ]));
                },
                Value::Uninitialized => {
                    return Err(Error::new("uninitialized pointer arg passed as safe pointer".to_string(), vec![
                        (callsite.loc().clone(), "this pointer must be safe".to_string()),
                        (callsite_storage.stored_here.clone(), "but this value is possibly uninitialized".to_string()),
                    ]));
                },
                Value::Dropped{stored_here, dropped_here} => {
                    return Err(Error::new(format!("passing dropped value '{}' as safe pointer", callsite_storage.name), vec![
                        (callsite.loc().clone(),    "used here".to_string()),
                        (stored_here.clone(),       "points at this storage location".to_string()),
                        (dropped_here.clone(),      "which was dropped here".to_string()),
                    ]));
                },
                Value::Moved{moved_here} => {
                    return Err(Error::new(format!("passing moved value '{}' as safe pointer", callsite_storage.name), vec![
                        (callsite.loc().clone(),    "use of moved value".to_string()),
                        (moved_here.clone(),        "was moved here".to_string())
                    ]));
                },
                Value::Function{..} => {
                    return Err(Error::new(format!("ICE: trying to pass function as pointer"), vec![
                        (callsite.loc().clone(), "cannot determine lifetime of expression".to_string())
                    ]));
                },
                Value::Literal(val) => {

                    if callsite_storage.tags.contains("safe") {
                        return Ok(());
                    }

                    match it.next() {
                        Some((i, stack_ptr)) => {
                            if stack_ptr.tags.contains("unsafe") {
                                return Ok(());
                            }
                        },
                        None => {
                            if stack.tags.contains("unsafe") {
                                return Ok(());
                            }
                        },
                    };

                    // string literal
                    if val.contains("\"") {
                        return Err(Error::Untrackable("string literals are weird".to_string()));
                    }

                    if let Some(change) = &callsite_storage.changed_here {
                        return Err(Error::new(format!("incompatible argument"), vec![
                            (callsite.loc().clone(), "this expression has a different pointer depth".to_string()),
                            (change.clone(), format!("{} might not be a safe pointer", val)),
                            (stack.typed.loc.clone(), format!("expected at depth {} here", i)),
                        ]));
                    } else {
                        return Err(Error::new(format!("incompatible argument"), vec![
                            (callsite.loc().clone(), "this expression has a different pointer depth".to_string()),
                            (stack.typed.loc.clone(), format!("expected at depth {} here", i)),
                        ]));
                    }
                }
            };
            callsite_storage = &mut self.storage[callsite_ptr];

            if stack_ptr.tags.contains_key("mutable") && !callsite_storage.tags.contains_key("mutable") {
                return Err(Error::new("const pointer cannot be used as mut pointer in function call".to_string(), vec![
                       (callsite.loc().clone(), "this expression must yield a mutable pointer".to_string()),
                       (callsite_storage.stored_here.clone(), "suggestion: change this declaration to mutable".to_string()),
                ]));
            }

            if let Some(vals) = stack_ptr.tags.get("require") {
                let mut missing = vals.clone();
                if let Some(v) = callsite_storage.tags.get("marked") {
                    for (v,_) in v {
                        missing.remove(v);
                    }
                }
                for (val,_) in missing {
                    return Err(Error::new(format!("missing required type state '{}'", val), vec![
                           (callsite.loc().clone(), format!("argument here must be '{}'", val))
                    ]));
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
                    return Err(Error::new(format!("cannot use marked local '{}'", callsite_storage.name), vec![
                        (callsite.loc().clone(), format!("argument here is not allowed to be '{}'", val)),
                        (vloc.clone(), format!("marked '{}' here. probably must call a related function to unset", val)),
                    ]));
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
                        return Err(Error::new("call would mark borrowed callsite".to_string(), vec![
                               (callsite.loc().clone(),
                               format!("this expression would mark the callsite with undeclared mark '{}'",
                                       missing.0)),
                               (callsite_storage.stored_here.clone(),
                               format!("try adding a set<{}> tag here",
                                       missing.0)),
                        ]));
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
                    return Err(Error::new("cannot move stack".to_string(), vec![
                           (callsite.loc().clone(),
                           format!("this expression would move '{}' out of scope, which is on the stack", callsite_storage.name)),
                    ]));
                }

                if let Some(_) = callsite_storage.tags.get("borrowed") {
                    return Err(Error::new("cannot move borrowed pointer".to_string(), vec![
                           (callsite.loc().clone(),
                           format!("this expression would move '{}' out of scope", callsite_storage.name)),
                           (tag.iter().next().unwrap().1.clone(), "required because this call argument is move".to_string()),
                           (callsite_storage.stored_here.clone(), "try changing this declaration to move".to_string()),
                    ]));
                }
                //TODO this will only move the rightmost value
                callsite_storage.value = Value::Moved{
                    moved_here: callsite.loc().clone(),
                };
                return Ok(());
            }

            if stack_ptr.tags.contains_key("unsafe") {
                return Ok(());
            }

            if !stack_ptr.tags.contains_key("unsafe") && callsite_storage.tags.contains_key("unsafe") {
                return Err(Error::new("passing unsafe pointer to safe function call".to_string(), vec![
                    (callsite.loc().clone(), "this expression must be safe".to_string()),
                    (callsite_storage.stored_here.clone(), "suggestion: add a runtime check for this value and mark it safe".to_string()),
                ]));
            }
        }

        /* this is broken. it should check rightmost pointer.
         * and kind of useless because we assume all fn arguments are unsafe
        match &callsite_storage.value {
            Value::Uninitialized => {
                return Err(Error::new(format!("uninitialized var {} passed as safe arg", callsite_storage.name), vec![
                    (callsite.loc().clone(), "this arg must be safe".to_string()),
                    (callsite_storage.stored_here.clone(), "but this value is uninitialized".to_string()),
                ]));
            },
            Value::Dropped{stored_here, dropped_here} => {
                return Err(Error::new(format!("passing dropped value '{}' as safe pointer", callsite_storage.name), vec![
                    (callsite.loc().clone(),"used here".to_string()),
                    (stored_here.clone(),   "points at this storage location".to_string()),
                    (dropped_here.clone(),  "which was dropped here".to_string()),
                ]));
            },
            Value::Moved{moved_here} => {
                return Err(Error::new(format!("passing moved value '{}' as safe pointer", callsite_storage.name), vec![
                       (callsite.loc().clone(), "use of moved value".to_string()),
                       (moved_here.clone(), "was moved here".to_string()),
                ]));
            },
            _ => (),
        };
        */
        Ok(())
    }


    fn check_expr(&mut self, expr: &mut ast::Expression) -> Result<Address, Error>  {
        let exprloc = expr.loc().clone();
        match expr {
            ast::Expression::Name(name) => {
                if name.name.is_absolute() && name.name.0[1] == "ext" {
                    return Err(Error::Untrackable(format!("ext name {}", name.name)));
                }
                self.check_name(&name.name, &name.loc)
            }
            ast::Expression::MemberAccess {loc, lhs, rhs, op, ..} => {
                let mut ptr = self.check_expr(lhs)?;
                if op == "->" {
                    ptr = self.deref(ptr, &exprloc)?;
                }
                let mut storage = self.storage.get(ptr).unwrap().clone();
                if storage.tags.contains_key("unsafe") {
                    return Err(Error::new(format!("illegal read access to unsafe storage '{}'", storage.name), vec![
                           (loc.clone(), "used here".to_string()),
                           (storage.stored_here.clone(), "suggestion: add a runtime check for this value and mark it safe".to_string()),
                    ]));
                }
                let value = match &mut storage.value {
                    Value::Struct(fields) => fields,
                    Value::Untrackable(s) => {
                        return Err(Error::Untrackable(s.clone()));
                    },
                    Value::Uninitialized  => {
                        return Err(Error::new(format!("unsafe member access to possibly uninitialized pointer '{}'", storage.name), vec![
                            (loc.clone(), "used here".to_string()),
                        ]));
                    },
                    Value::Address(_) => {
                        return Err(Error::new(format!("member access to non struct pointer {}", storage.name), vec![
                            (loc.clone(), "this is not a struct".to_string()),
                        ]));
                    },
                    o => {
                        return Err(Error::new(format!("illegal member access to {} {}", o, storage.name), vec![
                            (loc.clone(), "used here".to_string()),
                        ]));
                    }
                };

                let typ = match &storage.typ {
                    None => {
                        return Err(Error::new("unknown type cannot be accessed".to_string(), vec![
                               (lhs.loc().clone(), format!("type of '{}' is not known", storage.name)),
                        ]));
                    },
                    Some(t) => t,
                };

                if typ.name.0[1] == "ext" {
                    return Err(Error::Untrackable(format!("ext name {}", typ.name)));
                }

                let def = self.defs.get(&typ.name).cloned();
                let fields = match def {
                    Some(ast::Def::Struct{fields,..}) => {
                        fields
                    },
                    _ =>  {
                        return Err(Error::new(format!("'{}' is not a struct", typ.name), vec![
                            (lhs.loc().clone(), format!("'{}' is not accessible as struct", typ.name))
                        ]));
                    },
                };

                for field in &fields {
                    if &field.name == rhs {
                        if !value.contains_key(&field.name) {
                            let m_ptr  = self.storage.len();
                            self.storage.push(Storage{
                                typ:            Some(field.typed.clone()),
                                name:           Name::from(
                                    &format!("member access of {}@{}->{}", storage.name, ptr, field.name)
                                    ),
                                    stored_here:    field.loc.clone(),
                                    value:          Value::Uninitialized,
                                    tags:           field.tags.clone(),
                                    changed_here:   None,
                            });
                            value.insert(field.name.clone(), m_ptr);
                        }

                        return Ok(value[&field.name]);
                    }
                }
                return Err(Error::new(format!("struct '{}' has no member named '{}'", typ.name, rhs), vec![
                    (loc.clone(), format!("unresolveable member access of '{}' here", storage.name))
                ]));
            }


            ast::Expression::ArrayAccess {lhs, rhs, ..} => {
                self.check_bounds(lhs, rhs, true)?;
                let ptr_of_holder = self.check_expr(lhs)?;

                let index_ptr = self.check_expr(rhs)?;
                let index = match &self.storage[index_ptr].value {
                    Value::Literal(v) => {
                        if let Ok(v) = v.parse::<u64>() {
                            v.clone()
                        } else {
                            return Err(Error::Untrackable("array access with non static index".to_string()));
                        }
                    },
                    _ => {
                        return Err(Error::Untrackable("array access with non static index".to_string()));
                    }
                };


                match &self.storage[ptr_of_holder].value {
                    Value::Array(v) => {
                        Ok(*v.get(index as usize).expect("out of bound access should have been cought by check_bounds"))
                    },
                    Value::Address(to) => {
                        Ok(*to)
                    }
                    o => {
                        Err(Error::Untrackable(format!("index into {}", o)))
                    }
                }
            }

            ast::Expression::Literal { loc, v } => {
                let temp_ptr = self.local(None, Name::from(
                        &format!("literal {}", self.storage.len())),
                        loc.clone(),
                        Tags::new());
                self.write(temp_ptr, Value::Literal(v.clone()), &expr.loc());
                Ok(temp_ptr)
            },

            ast::Expression::Call { name, args: callargs, loc: callloc, .. } => {

                match name.as_ref() {
                    &ast::Expression::Name(ref t)  if t.name.0[0] == "len" => {
                        if callargs.len() != 1 {
                            return Err(Error::new("call argument count mismatch".to_string(), vec![
                                                  (name.loc().clone(), format!("builtin len() needs 1 argument, but you passed {}", callargs.len()))
                            ]));
                        }
                        let to = self.check_expr(&mut callargs[0])?;

                        match self.storage[to].value.clone() {
                            Value::Array(s) => {
                                let temp_ptr = self.local(None, Name::from(
                                        &format!("literal len of {} ({})", self.storage[to].name, self.storage.len())),
                                        callloc.clone(),
                                        Tags::new());

                                let lit = format!("{}", s.len());
                                *expr = ast::Expression::Literal{
                                    loc: callloc.clone(),
                                    v:   lit.clone(),
                                };
                                self.write(temp_ptr, Value::Literal(lit.clone()), &expr.loc());
                                return Ok(temp_ptr);
                            },
                            o => {
                                return Err(Error::new(format!("len of {} {} is not known at compile time", o, self.storage[to].name), vec![
                                    (callloc.clone(), "len on unsized object not valid here".to_string())
                                ]));
                            }
                        }
                    }
                    _ => ()
                };


                let name_ptr = self.check_expr(name)?;
                let (fname, ret, args, vararg) = match self.storage[name_ptr].value.clone() {
                    Value::Function{name: fname, ret, args, vararg} => {
                        (fname, ret, args, vararg)
                    }
                    Value::Untrackable(s) => {
                        return Err(Error::Untrackable(s.clone()));
                    },
                    _ => {
                        return Err(Error::new("call argument count mismatch".to_string(), vec![
                            (name.loc().clone(), format!("builtin len() needs 1 argument, but you passed {}", callargs.len()))
                        ]));
                    }
                };


                debug!("    checking function call {}", fname);

                let mut callargs_org = std::mem::replace(callargs, Vec::new());

                // generated arguments
                for i in 0..args.len() {
                    if let Some(cs) = args[i].tags.get("callsite_macro") {
                        let (v, loc) = cs.iter().next().unwrap();
                        let genarg = Box::new(ast::Expression::Literal{
                            loc: loc.clone(),
                            v:   v.clone(),
                        });
                        callargs.push(genarg);
                    } else {
                        if callargs_org.len() > 0 {
                            callargs.push(callargs_org.remove(0));
                        }
                    }
                }
                callargs.extend(callargs_org);

                if (!vararg && args.len() != callargs.len()) | (vararg && args.len() > callargs.len())   {
                    let mut es = vec![(name.loc().clone(), format!("function needs {} arguments, but you passed {}", args.len(),callargs.len() ))];
                    if let Some(typed) = &self.storage[name_ptr].typ {
                        es.push((typed.loc.clone(), format!("declared here")));
                    } else {
                        es.push((self.storage[name_ptr].stored_here.clone(),
                            format!("declaration tracking interrupted here in {}",
                                    self.storage[name_ptr].name)));
                    }
                    return Err(Error::new("call argument count mismatch".to_string(), es));
                }

                let mut argpos: HashMap<String, usize> = HashMap::new();
                for i in 0..args.len() {
                    argpos.insert(args[i].name.clone(),i);
                    // check value of expression, which will be copied into the functions scope
                    self.check_call_arg(&args[i], &mut callargs[i])?;
                }

                //double check  callargs because with varargs, there might be more
                for arg in callargs.iter_mut() {
                    let a = self.check_expr(arg)?;
                }

                //boundary checks
                for i in 0..args.len() {
                    if let Some(bounds) = args[i].tags.get("bound") {
                        for (bound, loc) in bounds {
                            let bound_by = match argpos.get(bound) {
                                None => {
                                    emit_error("bound to undeclared argument", &[
                                               (loc.clone(), format!("{} is not an argument", bound))
                                    ]);
                                    ABORT.store(true, Ordering::Relaxed);
                                    continue;
                                }
                                Some(v) => *v,
                            };
                            self.check_bounds(&mut callargs[bound_by].clone(), &mut callargs[i], false)?;
                        }
                    }
                }

                return match ret {
                    Some(ret) => {
                        let (return_scope_ptr, ret_typ)  = ret;
                        Ok(return_scope_ptr)
                    }
                    None => {
                        let temp_ptr = self.local(None, Name::from(
                                &format!("void function call return {} at {:?}", fname, exprloc.span.start_pos().line_col())),
                                exprloc.clone(),
                                Tags::new());
                        self.write(temp_ptr, Value::Void, &expr.loc());
                        Ok(temp_ptr)
                    },
                }
            }

            ast::Expression::InfixOperation { lhs, rhs, loc} => {
                let mut static_value = None;

                let lhs = self.check_expr(lhs)?;
                let lhs = match & self.storage[lhs].value {
                    Value::Literal(val) => {
                        if let Ok(n) = val.parse::<i64>() {
                            static_value = Some(n);
                        }
                    },
                    _ => (),
                };
                for (op, expr) in rhs {
                    let ptr = self.check_expr(expr)?;
                    match &self.storage[ptr].value {
                        Value::Literal(val) => {
                            if let Ok(n) = val.parse::<i64>() {
                                if let Some(a) = static_value {
                                    match op.0.as_str() {
                                        "+" => {
                                            static_value = Some(a + n);
                                        },
                                        "-" => {
                                            static_value = Some(a - n)
                                        }
                                        _ => {
                                            static_value = None;
                                        }
                                    }
                                }
                            }
                        },
                        _ => {
                            static_value = None;
                        }
                    };
                }

                let temp_ptr = self.local(None, Name::from(
                        &format!("value of math expression {}", self.storage.len())),
                        expr.loc().clone(),
                        Tags::new());

                if let Some(static_value) = static_value {
                    self.write(temp_ptr, Value::Literal(format!("{}", static_value)) , &expr.loc());
                } else {
                    self.write(temp_ptr, Value::Untrackable(format!("runtime math at {}", exprloc)), &expr.loc());
                }

                Ok(temp_ptr)
            }

            ast::Expression::Cast { expr, .. } => {
                //TODO
                self.check_expr(expr)
            }

            ast::Expression::UnaryPost {expr, ref mut op, ..}=> {
                self.check_expr(expr)
            }

            ast::Expression::UnaryPre {expr, op, loc }=> {
                if op == "&" {
                    let lf = self.check_expr(expr)?;
                    let temp_ptr = self.local(None, Name::from(
                            &format!("temporary access {}", self.storage.len())),
                            expr.loc().clone(),
                            Tags::new());
                    self.write(temp_ptr, Value::Address(lf), &expr.loc());
                    Ok(temp_ptr)

                } else if op == "*" {
                    let v = self.check_expr(expr)?;
                    self.deref(v, &exprloc)
                } else {
                    self.check_expr(expr)
                }
            }

            ast::Expression::StructInit {loc, typed, fields} => {
                let def = match self.defs.get(&typed.name) {
                    Some(ast::Def::Struct{fields,..}) => {
                        Some(fields.clone())
                    },
                    _ => None
                };

                let mut value = HashMap::new();

                for (field, expr) in fields {

                    let mut found_field = def.is_none();
                    let mut fieldtyped = None;
                    if let Some(def) = &def {
                        for fielddef in def {
                            if field == &fielddef.name {
                                fieldtyped = Some(fielddef.typed.clone());
                                found_field = true;
                            }
                        }
                    };

                    if !found_field {
                        return Err(Error::new(format!("{} does not have field {}", typed.name, field), vec![
                            (expr.loc().clone(), "in this struct initialization\n".to_string())
                        ]));
                    }

                    let temp_ptr = self.local(fieldtyped, Name::from(
                            &format!("struct init {}.{} {}", typed.name, field, self.storage.len())),
                            expr.loc().clone(),
                            Tags::new());
                    let to = self.check_expr(expr)?;
                    self.copy(to, temp_ptr, &expr.loc())?;
                    value.insert(field.clone(), temp_ptr);
                }

                let temp_ptr = self.local(None, Name::from(
                        &format!("struct init {} {}", typed.name, self.storage.len())),
                        expr.loc().clone(),
                        Tags::new());
                self.write(temp_ptr, Value::Struct(value), &exprloc);
                Ok(temp_ptr)
            }

            ast::Expression::ArrayInit {fields, ..} => {

                let mut value = Vec::new();

                let aptr = self.local(None, Name::from(
                        &format!("literal array {}", self.storage.len())),
                        exprloc.clone(),
                        Tags::new());

                for (i, expr) in fields.iter_mut().enumerate() {
                    let temp_ptr = self.local(None, Name::from(
                            &format!("literal array init {} {} {}", aptr, i, self.storage.len())),
                            expr.loc().clone(),
                            Tags::new());
                    let to = self.check_expr(expr)?;
                    self.copy(to, temp_ptr, &exprloc)?;
                    value.push(temp_ptr);
                }

                self.write(aptr, Value::Array(value), &exprloc);

                Ok(aptr)
            }
        }
    }

    fn check_block(&mut self, body: &mut ast::Block) {
        for stm in &mut body.statements {
            or_stop(self.check_stm(stm));
        }
    }

    fn check_stm(&mut self, stm: &mut ast::Statement) -> Result<(), Error> {
        match stm {
            ast::Statement::Mark{lhs, key, value, loc} => {
                let to = self.check_expr(lhs)?;
                let storage = &mut self.storage[to];

                if key == "safe" {
                    storage.tags.remove("unsafe", None);
                    storage.tags.insert("safe".to_string(), String::new(), loc.clone());
                    self.initialize(to, &loc, true);
                } else if key == "clean" || key == "pure"{
                    storage.tags.remove("marked", None);
                } else {
                    storage.tags.insert(key.clone(), value.clone(), loc.clone());
                }
                Ok(())
            }
            ast::Statement::CBlock{..} => {
                Err(Error::Untrackable("block of c code".to_string()))
            }
            ast::Statement::Unsafe(_) => {
                Err(Error::Untrackable("unsafe block".to_string()))
            }
            ast::Statement::Block(block) => {
                self.push("block");
                self.check_block(block);
                self.pop(&block.end);
                Ok(())
            }
            ast::Statement::Cond{body, expr,..}=> {
                self.push("if");
                if let Some(expr) = expr {
                    match self.check_expr(expr) {
                        Ok(_) => (),
                        Err(Error::Untrackable(e)) => {
                            emit_debug("untrackable conditional", &[
                                (expr.loc().clone(), format!("{}", e))
                            ]);
                        },
                        Err(e) => return Err(e),
                    };
                }
                self.check_block(body);
                self.pop(&body.end);
                Ok(())
            },
            ast::Statement::For{body, e1, e2, e3,..} => {
                self.push("for");
                for stm in e1 {
                    self.check_stm(stm)?;
                }
                for stm in e2 {
                    self.check_stm(stm)?;
                }
                for stm in e3 {
                    self.check_stm(stm)?;
                }
                self.check_block(body);
                self.pop(&body.end);
                Ok(())
            }
            ast::Statement::Expr{loc, expr} => {
                self.check_expr(expr)?;
                Ok(())
            }
            ast::Statement::Var{name, assign, tags, loc, typed, array, ..} => {
                let mut tags = tags.clone();
                tags.insert("stack".to_string(), String::new(), loc.clone());
                let ptr = self.local(Some(typed.clone()), Name::from(&*name), loc.clone(), tags.clone());


                if let Some(Some(expr)) = array {
                    debug!("    initializing {} with array expr", ptr);
                    let ptr2 = self.check_expr(expr)?;
                    match &self.storage[ptr2].value {
                        Value::Literal(val) => {
                            if let Ok(val) = val.parse::<u64>() {
                                let mut value = Vec::new();
                                for i in 0..val {
                                    let temp_ptr = self.local(None, Name::from(
                                            &format!("var array init {} {}", i, self.storage.len())),
                                            expr.loc().clone(),
                                            Tags::new());
                                    value.push(temp_ptr);
                                }
                                self.write(ptr, Value::Array(value), loc);
                            } else {
                                emit_warn("untrackable array size", &[
                                    (expr.loc().clone(), "cannot evaluate this literal at compile time")
                                ]);
                            }
                        },
                        o => {
                            emit_warn("untrackable array size", &[
                                (expr.loc().clone(), format!("cannot evaluate this {} at compile time", o))
                            ]);
                        }
                    }
                }

                if let Some(assign) = assign {
                    let rhs = self.check_expr(assign)?;
                    self.copy(rhs, ptr, loc)?;
                }
                Ok(())
            },
            ast::Statement::Assign{lhs, rhs, loc, ..} => {
                let rhs = self.check_expr(rhs)?;
                let lhs = self.check_expr(lhs)?;
                self.copy(rhs, lhs, loc)?;
                Ok(())
            },
            ast::Statement::Return{ref mut expr, loc} => {
                if let Some(ref mut expr) = expr {
                    let val = self.check_expr(expr)?;
                }
                Ok(())
            },
            ast::Statement::Switch{loc, expr, cases, default} => {
                let val = self.check_expr(expr);
                for (expr, block) in cases {
                    let val = self.check_expr(expr);
                    self.push("block");
                    self.check_block(block);
                    self.pop(&block.end);
                }
                if let Some(default) = default {
                    self.check_block(default);
                }
                Ok(())
            }
            ast::Statement::Goto{..}
            | ast::Statement::Label{..}
            | ast::Statement::Break{..}
            | ast::Statement::Continue{..}
            => {
                Ok(())
            },
        }
    }

    // bounds are on a storage holding an address,
    // saying how much further to the right of the address it is legal to read.
    fn check_bounds(&mut self, array: &mut ast::Expression, index: &mut ast::Expression, index_access: bool) -> Result<(), Error> {

        emit_debug("bounds",&[
            (index.loc().clone(), format!("checking if this index")),
            (array.loc().clone(), format!("will be within this array")),
        ]);

        let ptr         = self.check_expr(array)?;
        let storage     = &self.storage[ptr];
        let array_name  = self.storage[ptr].name.clone();
        let array_loc   = storage.stored_here.clone();


        let mut array_size = None;
        if let Value::Array(v) = &storage.value {
            array_size = Some(v.len() as u64)
        }

        if let Some(lens) = storage.tags.get("len").cloned() {
            for (len,loc) in lens {
                if let Ok(v) = len.parse::<u64>() {
                    array_size = Some(v);
                } else if let Ok(v) = self.check_name(&Name::from(&len), &loc) {
                    match &self.storage[v].value {
                        Value::Literal(val) => {
                            if let Ok(val) = val.parse::<u64>() {
                                array_size = Some(val);
                            } else {
                                emit_warn("len tag has no effect", &[
                                    (self.storage[v].stored_here.clone(), format!("{} is not parseable as integer", v)),
                                ]);
                            }
                        },
                        o => {
                            emit_warn("len tag has no effect", &[
                                (self.storage[v].stored_here.clone(), format!("{} {} is not a literal", self.storage[v].name, o)),
                            ]);
                        }
                    }
                } else {
                    emit_warn("len tag has no effect", &[
                        (loc.clone(), format!("{} is not an integer or local", len)),
                    ]);
                }
            }
        }


        let index_ptr = match self.check_expr(index) {
            Ok(v) => v,
            Err(Error::Untrackable(e)) => {
                return Err(Error::new("possibly unbounded array access".to_string(), vec![
                    (index.loc().clone(), format!("index is not known at compile time because it is: {}", e))
                ]));
            },
            Err(e) => return Err(e),
        };

        if let Some(bounds) = self.storage[index_ptr].tags.get("bound") {
            for (bound, loc) in bounds {
                if Name::from(bound) == array_name {
                    emit_debug("bounds",&[
                        (loc.clone(), format!("index is bound to array")),
                    ]);
                    return Ok(());
                }
            }
        }

        if let Some(array_size) = array_size {
            match &self.storage[index_ptr].value {
                Value::Literal(val) => {
                    if let Ok(v) = val.parse::<u64>() {
                        if if index_access { array_size <= v } else {array_size < v } {
                            return Err(Error::new("index exceeds array size".to_string(), vec![
                                (index.loc().clone(), format!("index {} would overflow array bound", v)),
                                (array_loc, "of this array".to_string())
                            ]));
                        }
                    } else {
                        emit_warn("array access with untrackable index", &[
                            (index.loc().clone(), "expression is not an integer"),
                        ]);
                    }
                    return Ok(());
                },
                _=> (),
            }
            return Err(Error::new("possibly unbounded array access".to_string(), vec![
                (index.loc().clone(), "index is not known at compile time\n".to_string())
            ]));
        } else {
            return Err(Error::new("possibly unbounded array access".to_string(), vec![
                (index.loc().clone(), format!("array length is not known at compile time\n\
                suggestion: add a bound<{}> tag to a value known to be safe at runtime", array_name
            ))]));
        }
    }
}




pub fn check(md: &mut flatten::Module) {
    debug!("lifetime checking {}", md.name);

    let mut stack = Stack::default();
    stack.push("static");

    for (name,loc) in &md.c_names {
        let ptr = stack.local(None, name.clone(), loc.clone(), Tags::new());
        stack.write(ptr, Value::Untrackable("c name".to_string()), &loc);
    }


    for (local,_,_) in &mut md.d {
        let localname = Name::from(&local.name);
        debug!("   def {}", localname);

        stack.defs.insert(Name::from(&local.name), local.def.clone());
        match &mut local.def {

            ast::Def::Testcase {fields, ..} => {
                for (name, expr) in fields {
                    match stack.check_expr(expr) {
                        Ok(v) => {
                            match &stack.storage[v].value {
                                Value::Literal(s) => {
                                    *expr = ast::Expression::Literal{
                                        loc: expr.loc().clone(),
                                        v: s.clone(),
                                    };
                                },
                                _ => {
                                    emit_error(format!("unable to evaluate testcase field {} at compile time", name), &[
                                        (expr.loc().clone(), "testcases must be completely static")
                                    ]);
                                    ABORT.store(true, Ordering::Relaxed);
                                }
                            }
                        }
                        Err(Error::Error{message, details}) => {
                            emit_error(message, &details);
                            ABORT.store(true, Ordering::Relaxed);
                        },
                        Err(Error::Untrackable(r)) => {
                            emit_error(format!("unable to evaluate testcase field {} at compile time: {}", name, r), &[
                                (expr.loc().clone(), "testcases must be completely static")
                            ]);
                            ABORT.store(true, Ordering::Relaxed);
                        }
                    };
                }
            },
            ast::Def::Struct {fields, packed} => {
                let localname = Name::from(&local.name);
                let ptr = stack.local(None, localname.clone(), local.loc.clone(), Tags::new());
                stack.write(ptr, Value::Untrackable("struct type".to_string()), &local.loc);
            },

            ast::Def::Macro{args, body} => {
                let localname = Name::from(&local.name);
                let ptr = stack.local(None, localname.clone(), local.loc.clone(), Tags::new());
                stack.write(ptr, Value::Untrackable("macro".to_string()), &local.loc);
            },
            ast::Def::Const{typed, expr} => {
                let localname = Name::from(&local.name);
                let ptr = stack.local(Some(typed.clone()), localname.clone(), local.loc.clone(), Tags::new());
                match stack.check_expr(expr) {
                    Ok(v) => {
                        stack.copy(v, ptr, &local.loc).ok();
                    }
                    Err(e) => {
                        stack.write(ptr, Value::Untrackable(format!("{:?}", e)), &local.loc);
                    }
                }

            },
            ast::Def::Static{typed, expr, storage, tags, array} => {
                let localname = Name::from(&local.name);
                let ptr = stack.local(Some(typed.clone()), localname.clone(), local.loc.clone(), tags.clone());
                stack.write(ptr, Value::Untrackable("static decl".to_string()), &local.loc);
            },
            ast::Def::Enum{names, ..} => {
                for (name,_) in names {
                    let mut localname = Name::from(&local.name);
                    localname.push(name.clone());
                    let ptr = stack.local(None, localname.clone(), local.loc.clone(), Tags::new());
                    //TODO value
                    stack.write(ptr, Value::Literal(String::new()), &local.loc);
                }
            },
            ast::Def::Fntype{args, ret, vararg, ..} => {
                let localname = Name::from(&local.name);
                let ptr = stack.local(None, localname.clone(), local.loc.clone(), Tags::new());
                stack.write(ptr, Value::Untrackable("fntype".to_string()), &local.loc);
            }
            ast::Def::Function{body, args, ret, vararg, ..} => {
                let ptr = stack.local(None, localname.clone(), local.loc.clone(), Tags::new());

                let ret = match ret {
                    None => None,
                    Some(ret) => {
                        let mut rlf = stack.local(None, Name::from(&format!("return value of {}", local.name)),
                            ret.typed.loc.clone(),
                            Tags::new()
                        );
                        stack.write(rlf, Value::Untrackable("return value".to_string()), &ret.typed.loc.clone());

                        for ast_ptr in ret.typed.ptr.iter() {
                            let mut tags = ast_ptr.tags.clone();
                            if !tags.contains_key("move") {
                                tags.insert("borrowed".to_string(), String::new(), ret.typed.loc.clone());
                            }
                            let storage = stack.local(
                                Some(ret.typed.clone()),
                                Name::from(&format!("return value of {} ({})", local.name, stack.storage.len())),
                                ret.typed.loc.clone(), tags);
                            stack.write(storage, Value::Address(rlf), &ret.typed.loc.clone());
                            stack.current_return_ptr = Some(storage);
                            rlf = storage;
                        }

                        Some((rlf, ret.typed.clone()))
                    }
                };

                stack.push(&local.name);

                for arg in args.iter() {
                    let argname = Name::from(&arg.name);
                    let mut argtyped = arg.typed.clone();

                    // the right hand side
                    let mut arglocal = stack.local(Some(arg.typed.clone()), argname.clone(), arg.loc.clone(), arg.tags.clone());
                    stack.write(arglocal, Value::Untrackable(format!("passed by value as {}", argname)), &arg.loc);

                    for ast_ptr in arg.typed.ptr.iter().rev() {
                        //stack.storage[arglocal].tags.insert("unsafe".to_string(), String::new(), arg.loc.clone());
                        argtyped.ptr.pop();

                        let mut body_tags = ast_ptr.tags.clone();
                        if !body_tags.contains_key("move") {
                            body_tags.insert("borrowed".to_string(), String::new(), arg.loc.clone());
                        }
                        //body_tags.insert("unsafe".to_string(), String::new(), arg.loc.clone());
                        let site = stack.local(
                            Some(argtyped.clone()),
                            Name::from(&format!("{}callsite of {}/{} ({})",
                                "pointer to ".repeat(argtyped.ptr.len()),
                                local.name, argname, arglocal)), ast_ptr.loc.clone(), body_tags.clone()
                        );

                        // local is actually a ptr to this newly created depth
                        stack.write(arglocal, Value::Address(site), &arg.loc);
                        // keep moving left
                        arglocal = site;
                    }
                }

                stack.check_block(body);


                stack.write(ptr, Value::Function{
                    name: localname.clone(),
                    ret,
                    args: args.clone(),
                    vararg: *vararg,
                }, &local.loc);

                stack.pop(&body.end);
                stack.current_return_ptr = None;

            },
        }
    }
    if ABORT.load(Ordering::Relaxed) {
        std::process::exit(9);
    }
}


fn or_stop<T>(t: Result<T, Error>) {
    match t {
        Ok(_) => (),
        Err(Error::Error{message, details}) => {
            emit_error(message, &details);
            ABORT.store(true, Ordering::Relaxed);
        },
        Err(Error::Untrackable(_)) => {
        }
    }
}

