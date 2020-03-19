use super::Error;
use super::flatten;
use crate::name::Name;
use std::collections::HashMap;
use crate::ast;
use ast::Tags;
use super::parser::{emit_debug};
use std::collections::HashSet;




#[derive(Clone)]
struct Storage {
    name:           Name,
    typed:          ast::Typed,
    declared:       ast::Location,
    complete:       flatten::TypeComplete,
}

#[derive(Default)]
struct Scope {
    name:       String,
    storage:    HashMap<Name, Storage>,
}

struct Stack {
    defs:   HashMap<Name, ast::Def>,
    stack:  Vec<Scope>,
    moretypevariants:   HashMap<Name, HashSet<u64>>,
}

impl Stack {
    fn new() -> Self {
        Self {
            defs:   HashMap::new(),
            stack:  Vec::new(),
            moretypevariants: HashMap::new(),
        }
    }
    fn push(&mut self, name: String) {
        debug!("  scope {}", name);
        self.stack.push(Scope{
            name,
            storage: HashMap::new(),
        });
    }

    fn pop(&mut self) {
        self.stack.pop();
    }

    fn cur(&mut self) -> &mut Scope {
        self.stack.last_mut().unwrap()
    }

    fn alloc(&mut self,
             name: Name,
             typed: ast::Typed,
             loc: ast::Location,
             _tags: ast::Tags,
             complete: &flatten::TypeComplete
    ) -> Result<(), Error> {

        match format!("{}", name).as_str() {
            "len" | "theory" | "safe" | "nullterm" => {
                if self.stack.len() > 1 {
                    return Err(Error::new(format!("redeclaration of builtin theory '{}'", name), vec![
                        (loc.clone(), "this declaration would shadow a builtin".to_string()),
                    ]));
                }
            },
            _ => {
            }
        }

        if let Some(prev) = self.cur().storage.get(&name).cloned() {
            if prev.complete == flatten::TypeComplete::Complete &&  complete == &flatten::TypeComplete::Complete{
                return Err(Error::new(format!("redeclation of local name '{}'", name), vec![
                    (loc.clone(), "this declaration would shadow a previous name".to_string()),
                    (prev.declared.clone(), "previous declaration".to_string())
                ]));
            }
        }

        self.cur().storage.insert(name.clone(), Storage{
            typed:      typed.clone(),
            name:       name,
            declared:   loc.clone(),
            complete:   complete.clone(),
        });

        Ok(())

    }
}

pub fn expand(module: &mut flatten::Module) -> Result<(), Error> {

    let mut stack = Stack::new();
    stack.push("global".to_string());


    // declaration run
    for (d,complete) in &mut module.d {
        stack.defs.insert(Name::from(&d.name), d.def.clone());

        match &mut d.def {
            ast::Def::Theory{..} => {
                stack.alloc(
                    Name::from(&d.name),
                    ast::Typed{
                        t:      ast::Type::Other(Name::from(&d.name.clone())),
                        ptr:    Vec::new(),
                        loc:    d.loc.clone(),
                        tail:   ast::Tail::None,
                    },
                    d.loc.clone(),
                    Tags::new(),
                    complete
                )?;
            },
            ast::Def::Function{args, callassert, callattests, calleffect, ..} => {
                stack.alloc(
                    Name::from(&d.name),
                    ast::Typed{
                        t:      ast::Type::Other(Name::from(&d.name.clone())),
                        ptr:    Vec::new(),
                        loc:    d.loc.clone(),
                        tail:   ast::Tail::None,
                    },
                    d.loc.clone(),
                    Tags::new(),complete
                )?;

            },
            ast::Def::Static {tags, typed, array, ..} => {
                let mut typed = typed.clone();
                if array.is_some() {
                    typed.ptr.push(ast::Pointer{
                        loc:  d.loc.clone(),
                        tags: Tags::new(),
                    });
                }
                stack.alloc(
                    Name::from(&d.name),
                    typed.clone(),
                    d.loc.clone(),
                    tags.clone(),
                    complete,
                )?;
            },
            ast::Def::Const { typed, .. } => {
                stack.alloc(
                    Name::from(&d.name),
                    typed.clone(),
                    d.loc.clone(), Tags::new(),
                    complete,
                )?;
            },
            ast::Def::Fntype {..} => {
                stack.alloc(
                    Name::from(&d.name),
                    ast::Typed{
                        t:      ast::Type::Other(Name::from(&d.name.clone())),
                        ptr:    Vec::new(),
                        loc:    d.loc.clone(),
                        tail:   ast::Tail::None,
                    },
                    d.loc.clone(), Tags::new(),
                    complete,
                )?;
            },
            ast::Def::Struct {fields, union, ..} => {
                stack.alloc(
                    Name::from(&d.name),
                    ast::Typed{
                        t:      ast::Type::Other(Name::from(&d.name.clone())),
                        ptr:    Vec::new(),
                        loc:    d.loc.clone(),
                        tail:   ast::Tail::None,
                    },
                    d.loc.clone(), Tags::new(),
                    complete,
                )?;
                if *union {
                    for field in fields {
                        //stack.cannot_drop_union(&field, &field.loc, 0)?;
                    }
                }
            },
            ast::Def::Enum{..} => {
                stack.alloc(
                    Name::from(&d.name),
                    ast::Typed{
                        t:      ast::Type::Other(Name::from(&d.name.clone())),
                        ptr:    Vec::new(),
                        loc:    d.loc.clone(),
                        tail:   ast::Tail::None,
                    },
                    d.loc.clone(), Tags::new(),
                    complete,
                )?;

                /*
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
                    stack.alloc(
                        localname,
                        t,
                        d.loc.clone(),
                        ast::Tags::new(),
                    )?;
                    value += 1;
                }
                */
            },
            ast::Def::Macro{..} => {
                stack.alloc(
                    Name::from(&d.name),
                    ast::Typed{
                        t:      ast::Type::Other(Name::from(&d.name.clone())),
                        ptr:    Vec::new(),
                        loc:    d.loc.clone(),
                        tail:   ast::Tail::None,
                    },
                    d.loc.clone(), Tags::new(),
                    complete,
                )?;
            },
            ast::Def::Testcase {..} => {},
            ast::Def::Include {..} => {},
        }
    }


    // definition run
    for (d, complete) in &mut module.d {
        match &mut d.def {
            ast::Def::Theory{..} => {},
            ast::Def::Function{args, body, callassert, callattests, calleffect, ..} => {

                for farg in args.iter_mut() {
                    if farg.typed.ptr.len() > 0 {

                        if Name::from(&d.name).0.last() != Some(&"borrow".to_string()) {
                            if let ast::Type::Other(name) = &farg.typed.t {
                                if let Some(ast::Def::Struct{impls,..}) = stack.defs.get(name) {
                                    if let Some((fnname,_)) = impls.get("borrow") {
                                        if let Some(ast::Def::Function{calleffect: calleffect2, callassert: callassert2, ..})
                                                = stack.defs.get(fnname) {

                                            // borrow where clauses are checked by expression expansion
                                            // but copy them to the body as asserts
                                            for effect in callassert2 {
                                                let mut effect = effect.clone();
                                                replace_named(
                                                    &mut effect,
                                                    &ast::Type::Other(Name::from("self")),
                                                    &ast::Type::Other(Name::from(&farg.name)),
                                                );
                                                replace_named(
                                                    &mut effect,
                                                    &ast::Type::Other(Name::from("return")),
                                                    &ast::Type::Other(Name::from(&farg.name)),
                                                );
                                                callattests.insert(0, effect.clone());
                                            }

                                            // functions borrowing something must behave like the  borrow
                                            for effect in calleffect2 {
                                                let mut effect = effect.clone();
                                                replace_named(
                                                    &mut effect,
                                                    &ast::Type::Other(Name::from("self")),
                                                    &ast::Type::Other(Name::from(&farg.name)),
                                                );
                                                replace_named(
                                                    &mut effect,
                                                    &ast::Type::Other(Name::from("return")),
                                                    &ast::Type::Other(Name::from(&farg.name)),
                                                );
                                                calleffect.insert(0, effect);
                                            }
                                        } else {
                                        }
                                    }
                                }
                            }
                        } else {
                            // if the function is an attestation of borrow
                            // make sure we don't expand its args recursively
                            farg.tags.insert("no-borrow-expand".to_string(), String::new(), ast::Location::builtin());
                        }


                        // safe is implicit unless the arg is marked unsafe
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
                        }


                    }
                }



                if complete == &flatten::TypeComplete::Complete {
                    stack.push(format!("{}", d.name));

                    for i in 0..args.len() {
                        let argname = Name::from(&args[i].name);
                        stack.alloc(
                            argname.clone(),
                            args[i].typed.clone(),
                            args[i].loc.clone(),
                            args[i].tags.clone(),
                            &flatten::TypeComplete::Complete,
                            )?;
                    }

                    stack.expand_scope(&mut body.statements)?;
                    body.statements.extend(stack.drop_fn(&body.end)?);

                    stack.pop();
                }


            },
            _ => (),
        }
    }


    for (t, vrs) in stack.moretypevariants {
        for variant in vrs {
            module.typevariants.entry(t.clone()).or_insert(HashSet::new()).insert(variant);
        }
    }


    Ok(())
}

impl Stack {
    fn expand_expr(&mut self, _expr: &mut ast::Expression) -> Result<(), Error> {
        // doesnt do anything yet
        return Ok(());

        /*


        match expr {
            ast::Expression::Name(_) => {}
            ast::Expression::MemberAccess {ref mut lhs, ..} => {
                self.expand_expr(lhs)?;
            }
            ast::Expression::ArrayAccess {ref mut lhs, ref mut rhs,..} => {
                self.expand_expr(lhs)?;
                self.expand_expr(rhs)?;
            }
            ast::Expression::LiteralString {..} => {},
            ast::Expression::LiteralChar {..} => {},
            ast::Expression::Literal{..} => {},
            ast::Expression::Call{ref mut name, ref mut args,..} => {
                self.expand_expr(name)?;
                for arg in args {
                    self.expand_expr(arg)?;
                }
            }
            ast::Expression::Infix {ref mut lhs, ref mut rhs, ..} => {
                self.expand_expr(lhs)?;
                self.expand_expr(rhs)?;
            }
            ast::Expression::Cast {ref mut expr,..} => {
                self.expand_expr(expr)?;
            }
            ast::Expression::UnaryPost {ref mut expr, ..} => {
                self.expand_expr(expr)?;
            }
            ast::Expression::UnaryPre {ref mut expr, ..} => {
                self.expand_expr(expr)?;
            }
            ast::Expression::StructInit {ref mut fields,..} => {
                for (_, expr) in fields {
                    self.expand_expr(expr)?;
                }
            }
            ast::Expression::ArrayInit {ref mut fields, ..} => {
                for expr in fields {
                    self.expand_expr(expr)?;
                }
            },
        }
        Ok(())

        */
    }

    fn expand_scope(&mut self, body: &mut Vec<Box<ast::Statement>>) -> Result<(), Error> {

        let mut i   = 0;
        let mut len = body.len();
        while i < len {
            match body[i].as_mut() {
                ast::Statement::Var{loc, typed, tags, name, array, assign, ..} => {
                    if let ast::Type::New = typed.t {

                        if !tags.contains("mut") {
                            tags.insert("mut".to_string(), String::new(), loc.clone());
                        }

                        if array.is_some() {
                            return Err(Error::new(format!("new stack initialization cannot be array"), vec![
                                (loc.clone(), "this new statement is invalid".to_string()),
                            ]));
                        }
                        if assign.is_none() {
                            return Err(Error::new(format!("new stack initialization requires function call to constructor"), vec![
                                (loc.clone(), "this new statement is uninitialized".to_string()),
                            ]));
                        }
                        let assign = assign.as_mut().unwrap();
                        if let ast::Expression::Call{loc, args, name: fname, ..} = assign {
                            if let ast::Expression::Name(ftyped) = fname.as_ref() {
                                if let ast::Type::Other(n) = &ftyped.t {
                                    if let Some(ast::Def::Function{args,..}) = self.defs.get(n) {
                                        if args.len() < 1 {
                                            return Err(Error::new(format!("new stack initialization requires function call to constructor"), vec![
                                                (loc.clone(), "first function argument must be self ptr".to_string()),
                                            ]));
                                        }
                                        let arg = args.first().unwrap();
                                        if arg.name != "self" {
                                            return Err(Error::new(format!("new stack initialization requires function call to constructor"), vec![
                                                (loc.clone(), "first function argument must be self".to_string()),
                                            ]));
                                        }
                                        if arg.typed.ptr.len() != 1 {
                                            return Err(Error::new(format!("new stack initialization requires function call to constructor"), vec![
                                                (loc.clone(), "first function argument must be self ptr".to_string()),
                                            ]));
                                        }
                                        let tail = typed.tail.clone();
                                        *typed = arg.typed.clone();
                                        typed.ptr = Vec::new();

                                        if let ast::Type::Other(tn) = &typed.t {
                                            if let ast::Tail::Static(f, _) = &tail {
                                                self.moretypevariants.entry(tn.clone()).or_insert(HashSet::new()).insert(*f);
                                            }
                                        }
                                        typed.tail = tail;
                                    } else {
                                        return Err(Error::new(format!("new stack initialization requires function call to constructor"), vec![
                                            (loc.clone(), "this new statement is invalid".to_string()),
                                        ]));
                                    }
                                } else {
                                    return Err(Error::new(format!("new stack initialization requires function call to constructor"), vec![
                                        (loc.clone(), "this new statement is invalid".to_string()),
                                    ]));
                                }
                            } else {
                                return Err(Error::new(format!("new stack initialization requires function call to constructor"), vec![
                                    (loc.clone(), "this new statement is uninitialized".to_string()),
                                ]));
                            }


                            args.insert(0, Box::new(ast::Expression::UnaryPre{
                                loc:    loc.clone(),
                                op:     ast::PrefixOperator::AddressOf,
                                expr:   Box::new(ast::Expression::Name(ast::Typed{
                                    t:      ast::Type::Other(Name::from(name.as_str())),
                                    ptr:    Vec::new(),
                                    tail:   ast::Tail::None,
                                    loc: loc.clone(),
                                })),
                            }));
                        } else {
                            return Err(Error::new(format!("new stack initialization requires function call to constructor"), vec![
                                (loc.clone(), "this new statement is invalid".to_string()),
                            ]));
                        }
                        let stm = Box::new(ast::Statement::Expr{
                            loc:    assign.loc().clone(),
                            expr:   assign.clone(),
                        });
                        *assign = ast::Expression::ArrayInit {
                            loc: loc.clone(),
                            fields: vec![
                                Box::new(ast::Expression::Literal{
                                    loc: loc.clone(),
                                    v:  "0".to_string(),
                                }),
                            ]
                        };
                        body.insert(i + 1, stm);
                        i   += 1;
                        len += 1;
                    } else {
                        let mut typed = typed.clone();
                        if array.is_some() {
                            typed.ptr.push(ast::Pointer{
                                loc: loc.clone(),
                                tags: Tags::new(),
                            });
                        }
                        self.alloc(Name::from(name.as_str()), typed, loc.clone(), tags.clone(), &flatten::TypeComplete::Complete)?;
                        if let Some(expr) = assign {
                            self.expand_expr(expr)?;
                        }
                    }
                }
                ast::Statement::If{branches}        => {
                    self.push("if".to_string());
                    for (_loc, _expr, block) in branches {
                        self.push("branch".to_string());
                        self.expand_scope(&mut block.statements)?;
                        self.pop();
                    }
                    self.pop();
                }
                ast::Statement::Expr{expr,..}      => {
                    self.expand_expr(expr)?;
                }
                ast::Statement::Return{loc,expr, .. }   => {
                    if let Some(expr) = expr {
                        self.expand_expr(expr)?;
                    }

                    let r = self.drop_fn(&loc)?;
                    for stm in r.into_iter().rev() {
                        body.insert(i, stm);
                        i   += 1;
                        len += 1;
                    }

                }
                ast::Statement::Label{..}           => {}
                ast::Statement::Mark{..} => {},
                ast::Statement::Switch{cases,  ..} => {
                    for (_, block) in cases {
                        self.push("case".to_string());
                        self.expand_scope(&mut block.statements)?;
                        self.pop();
                    }
                }
                ast::Statement::Assign{lhs, rhs,..} => {
                    self.expand_expr(lhs)?;
                    self.expand_expr(rhs)?;
                }

                ast::Statement::Continue{..} => {}
                ast::Statement::Break{loc} => {
                    let r = self.drop(&loc)?;
                    for stm in r.into_iter().rev() {
                        body.insert(i, stm);
                        i   += 1;
                        len += 1;
                    }
                }
                ast::Statement::Unsafe(block) | ast::Statement::Block(block) => {
                    self.push("block".to_string());
                    self.expand_scope(&mut block.statements)?;
                    block.statements.extend(self.drop(&block.end)?);
                    self.pop();
                }
                ast::Statement::For{e1,e2,e3,body, ..} => {
                    self.push("for loop".to_string());
                    self.expand_scope(e1)?;
                    if let Some(expr) = e2 {
                        self.expand_expr(expr)?;
                    }
                    self.expand_scope(e3)?;
                    self.expand_scope(&mut body.statements)?;
                    body.statements.extend(self.drop(&body.end)?);
                    self.pop();
                }
                ast::Statement::While{body, expr, ..} => {
                    self.push("while loop".to_string());
                    self.expand_scope(&mut body.statements)?;
                    body.statements.extend(self.drop(&body.end)?);
                    self.pop();
                    self.expand_expr(expr)?;
                }
                ast::Statement::CBlock{..} => {}
            }


            i+=1;
        }
        Ok(())
    }

    fn drop_fn(&mut self, loc: &ast::Location) -> Result<Vec<Box<ast::Statement>>, Error> {
        let mut r = Vec::new();
        for i in (1..self.stack.len()).rev() {
            r.extend(self.drop_frame(loc, i)?);
        }
        Ok(r)
    }
    fn drop(&mut self, loc: &ast::Location) -> Result<Vec<Box<ast::Statement>>, Error> {
        self.drop_frame(loc, self.stack.len() - 1)
    }

    fn drop_frame(&mut self, loc: &ast::Location, frame: usize) -> Result<Vec<Box<ast::Statement>>, Error> {
        let mut r = Vec::new();
        for (name, storage) in &self.stack[frame].storage {

            //TODO also drop owned pointers some day

            if storage.typed.ptr.len() != 0 {
                continue
            }

            let accesslocal = ast::Expression::UnaryPre {
                loc:    loc.clone(),
                op:     ast::PrefixOperator::AddressOf,
                expr:   Box::new(ast::Expression::Name(ast::Typed{
                    t:      ast::Type::Other(name.clone()),
                    ptr:    Vec::new(),
                    loc:    loc.clone(),
                    tail:   ast::Tail::None,
                }))
            };
            //r.extend(self.drop_local(loc, &storage.typed, accesslocal, format!("(&{})", name))?);
        }
        Ok(r)
    }


}




fn replace_named(expr: &mut ast::Expression, replacefrom: &ast::Type, replacewith: &ast::Type) {
    match expr {
        ast::Expression::Name(ref mut t) => {
            if &t.t == replacefrom {
                t.t = replacewith.clone();
            }
        }
        ast::Expression::MemberAccess {ref mut lhs, ..} => {
            replace_named(lhs, replacefrom, replacewith);
        }
        ast::Expression::ArrayAccess {ref mut lhs, ref mut rhs,..} => {
            replace_named(lhs, replacefrom, replacewith);
            replace_named(rhs, replacefrom, replacewith);
        }
        ast::Expression::LiteralString {..} => {},
        ast::Expression::LiteralChar {..} => {},
        ast::Expression::Literal{..} => {},
        ast::Expression::Call{ref mut name, ref mut args,..} => {
            replace_named(name, replacefrom, replacewith);
            for arg in args {
                replace_named(arg, replacefrom, replacewith);
            }
        }
        ast::Expression::Infix {ref mut lhs, ref mut rhs, ..} => {
            replace_named(lhs, replacefrom, replacewith);
            replace_named(rhs, replacefrom, replacewith);
        }
        ast::Expression::Cast {ref mut expr,..} => {
            replace_named(expr, replacefrom, replacewith);
        }
        ast::Expression::UnaryPost {ref mut expr, ..} => {
            replace_named(expr, replacefrom, replacewith);
        }
        ast::Expression::UnaryPre {ref mut expr, ..} => {
            replace_named(expr, replacefrom, replacewith);
        }
        ast::Expression::StructInit {ref mut fields,..} => {
            for (_, expr) in fields {
                replace_named(expr, replacefrom, replacewith);
            }
        }
        ast::Expression::ArrayInit {ref mut fields, ..} => {
            for expr in fields {
                replace_named(expr, replacefrom, replacewith);
            }
        },
    }
}
