use super::Error;
use super::flatten;
use crate::name::Name;
use std::collections::HashMap;
use crate::ast;
use ast::Tags;
use super::parser::{emit_warn, emit_debug};




#[derive(Clone)]
struct Storage {
    name:           Name,
    typed:          ast::Typed,
    declared:       ast::Location,
}

#[derive(Default)]
struct Scope {
    name:       String,
    storage:    HashMap<Name, Storage>,
}

struct Stack {
    defs:   HashMap<Name, ast::Def>,
    stack:  Vec<Scope>,
}

impl Stack {
    fn new() -> Self {
        Self {
            defs:   HashMap::new(),
            stack:  Vec::new(),
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

    fn alloc(&mut self, name: Name, typed: ast::Typed, loc: ast::Location, tags: ast::Tags) -> Result<(), Error> {

        match format!("{}", name).as_str() {
            "len" | "theory" | "safe" => {
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
            return Err(Error::new(format!("redeclation of local name '{}'", name), vec![
                (loc.clone(), "this declaration would shadow a previous name".to_string()),
                (prev.declared.clone(), "previous declaration".to_string())
            ]));
        }

        self.cur().storage.insert(name.clone(), Storage{
            typed:      typed.clone(),
            name:       name,
            declared:   loc.clone(),
        });

        Ok(())

    }
}

pub fn expand(module: &mut flatten::Module) -> Result<(), Error> {

    let mut stack = Stack::new();
    stack.push("global".to_string());


    // declaration run
    for (d,_,defined_here) in &mut module.d {
        stack.defs.insert(Name::from(&d.name), d.def.clone());

        match &mut d.def {
            ast::Def::Theory{args, ret, attr} => {
                let sym = stack.alloc(
                    Name::from(&d.name),
                    ast::Typed{
                        t:      ast::Type::Other(Name::from(&d.name.clone())),
                        ptr:    Vec::new(),
                        loc:    d.loc.clone(),
                        tail:   ast::Tail::None,
                    },
                    d.loc.clone(),
                    Tags::new()
                )?;
            },
            ast::Def::Function{args, body, vararg, ret, callassert, calleffect, ..} => {
                stack.alloc(
                    Name::from(&d.name),
                    ast::Typed{
                        t:      ast::Type::Other(Name::from(&d.name.clone())),
                        ptr:    Vec::new(),
                        loc:    d.loc.clone(),
                        tail:   ast::Tail::None,
                    },
                    d.loc.clone(),
                    Tags::new()
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
                        }
                    }
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
                stack.alloc(
                    Name::from(&d.name),
                    typed.clone(),
                    d.loc.clone(),
                    tags.clone(),
                )?;
            },
            ast::Def::Const { typed, expr} => {
                stack.alloc(
                    Name::from(&d.name),
                    typed.clone(),
                    d.loc.clone(), Tags::new()
                )?;
            },
            ast::Def::Fntype {ret,args,attr,vararg,..} => {
                stack.alloc(
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
            ast::Def::Struct {fields, packed, tail, union, impls } => {
                stack.alloc(
                    Name::from(&d.name),
                    ast::Typed{
                        t:      ast::Type::Other(Name::from(&d.name.clone())),
                        ptr:    Vec::new(),
                        loc:    d.loc.clone(),
                        tail:   ast::Tail::None,
                    },
                    d.loc.clone(), Tags::new()
                )?;
                if *union {
                    for field in fields {
                        stack.cannot_drop_union(&field, &field.loc)?;
                    }
                }
            },
            ast::Def::Enum{names} => {
                stack.alloc(
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
                    stack.alloc(
                        localname,
                        t,
                        d.loc.clone(),
                        ast::Tags::new(),
                    )?;
                    value += 1;
                }
            },
            ast::Def::Macro{args, body} => {
                stack.alloc(
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
            ast::Def::Testcase {..} => {
            },
        }
    }


    // definition run
    for (d,_,defined_here) in &mut module.d {
        match &mut d.def {
            ast::Def::Theory{args, ret, attr} => {},
            ast::Def::Function{args, body, vararg, ret, callassert, calleffect, ..} => {
                if !*defined_here {
                    continue;
                }

                stack.push(format!("{}", d.name));

                for i in 0..args.len() {
                    let argname = Name::from(&args[i].name);
                    stack.alloc(argname.clone(), args[i].typed.clone(), args[i].loc.clone(), args[i].tags.clone())?;
                }

                stack.expand_scope(&mut body.statements)?;
                body.statements.extend(stack.drop_fn(&body.end)?);


                stack.pop();
            },
            _ => (),
        }
    }


    Ok(())
}

impl Stack {
    fn expand_scope(&mut self, body: &mut Vec<Box<ast::Statement>>) -> Result<(), Error> {

        let mut i   = 0;
        let mut len = body.len();
        while i < len {
            match body[i].as_mut() {
                ast::Statement::Var{loc, typed, tags, name, array, assign} => {
                    let mut typed = typed.clone();
                    if array.is_some() {
                        typed.ptr.push(ast::Pointer{
                            loc: loc.clone(),
                            tags: Tags::new(),
                        });
                    }
                    self.alloc(Name::from(name.as_str()), typed, loc.clone(), tags.clone())?;
                }
                ast::Statement::If{branches}        => {
                    self.push("if".to_string());
                    for (loc, expr, block) in branches {
                        self.push("branch".to_string());
                        self.expand_scope(&mut block.statements)?;
                        self.pop();
                    }
                    self.pop();
                }
                ast::Statement::Expr{expr, ..}      => {}
                ast::Statement::Return{loc, expr}   => {
                    let r = self.drop_fn(&loc)?;
                    for stm in r.into_iter().rev() {
                        body.insert(i, stm);
                        i   += 1;
                        len += 1;
                    }
                }
                ast::Statement::Label{..}           => {}
                ast::Statement::Mark{..} => {},
                ast::Statement::Switch{expr, cases, default, ..} => {
                    for (_, block) in cases {
                        self.push("case".to_string());
                        self.expand_scope(&mut block.statements)?;
                        self.pop();
                    }
                }
                ast::Statement::Assign{loc, lhs, op, rhs} => {}
                ast::Statement::Continue{loc} => {}
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
                ast::Statement::For{e1,e2,e3,body} => {
                    self.push("for loop".to_string());
                    self.expand_scope(e1)?;
                    self.expand_scope(e3)?;
                    self.expand_scope(&mut body.statements)?;
                    body.statements.extend(self.drop(&body.end)?);
                    self.pop();
                }
                ast::Statement::While{expr, body} => {
                    self.push("while loop".to_string());
                    self.expand_scope(&mut body.statements)?;
                    body.statements.extend(self.drop(&body.end)?);
                    self.pop();
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
            r.extend(self.drop_local(loc, &storage.typed, accesslocal)?);
        }
        Ok(r)
    }


    fn drop_local(&self, loc: &ast::Location, typed: &ast::Typed, expr: ast::Expression) -> Result<Vec<Box<ast::Statement>>, Error> {
        let mut v = Vec::new();
        if let ast::Type::Other(name) = &typed.t {
            if let Some(ast::Def::Struct{impls,fields,..}) = self.defs.get(name) {
                if let Some((fnname,_)) = impls.get("drop") {
                    emit_debug(format!("drop {}", typed), &[(loc.clone(), "here")]);
                    let call = ast::Expression::Call {
                        loc:            loc.clone(),
                        name:           Box::new(ast::Expression::Name(ast::Typed{
                            t:      ast::Type::Other(fnname.clone()),
                            ptr:    Vec::new(),
                            loc:    loc.clone(),
                            tail:   ast::Tail::None,
                        })),
                        args:           vec![Box::new(expr.clone())],
                        expanded:       false,
                        emit:           ast::EmitBehaviour::Default,
                    };
                    let stm = Box::new(ast::Statement::Expr{
                        expr: call,
                        loc:  loc.clone(),
                    });
                    v.push(stm);
                }
                for field in fields {
                    let accesslocal = ast::Expression::UnaryPre {
                        loc:    loc.clone(),
                        op:     ast::PrefixOperator::AddressOf,
                        expr: Box::new(ast::Expression::MemberAccess {
                            loc:    loc.clone(),
                            lhs:    Box::new(expr.clone()),
                            op:     "->".to_string(),
                            rhs:    field.name.clone(),
                        }),
                    };
                    v.extend(self.drop_local(loc, &field.typed, accesslocal)?);
                }
            }
        }
        Ok(v)
    }


    fn cannot_drop_union(&self, field: &ast::Field, in_union: &ast::Location) -> Result<(), Error> {
        if let ast::Type::Other(name) = &field.typed.t {
            if let Some(ast::Def::Struct{impls,fields,..}) = self.defs.get(name) {
                if let Some((_,loc)) = impls.get("drop") {
                    return Err(Error::new(format!("struct {} cannot be used in a union because it has a drop implementation", name), vec![
                                          (in_union.clone(), format!("union field {} cannot be dropped safely", field.name)),
                                          (loc.clone(), "because of a drop implementation here".to_string()),
                    ]));
                }
                for field in fields {
                    self.cannot_drop_union(field, in_union)?;
                }

            }
        }
        Ok(())
    }


}
