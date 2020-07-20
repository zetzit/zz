use super::flatten;
use super::Error;
use crate::ast;
use crate::name::Name;
use ast::Tags;
use std::collections::HashMap;

#[derive(Clone)]
struct Storage {
    name: Name,
    typed: ast::Typed,
    declared: ast::Location,
    complete: flatten::TypeComplete,
}

#[derive(Default)]
struct Scope {
    #[allow(unused)]
    name: String,
    storage: HashMap<Name, Storage>,
}

struct Stack {
    defs: HashMap<Name, ast::Def>,
    stack: Vec<Scope>,
    moretypevariants: HashMap<Name, HashMap<u64, ast::Location>>,
}

impl Stack {
    fn new() -> Self {
        Self {
            defs: HashMap::new(),
            stack: Vec::new(),
            moretypevariants: HashMap::new(),
        }
    }
    fn push(&mut self, name: String) {
        debug!("  scope {}", name);
        self.stack.push(Scope {
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

    fn alloc(
        &mut self,
        name: Name,
        typed: ast::Typed,
        loc: ast::Location,
        _tags: ast::Tags,
        complete: &flatten::TypeComplete,
    ) -> Result<(), Error> {
        match format!("{}", name).as_str() {
            "len" | "theory" | "safe" | "nullterm" => {
                if self.stack.len() > 1 {
                    return Err(Error::new(
                        format!("redeclaration of builtin theory '{}'", name),
                        vec![(
                            loc.clone(),
                            "this declaration would shadow a builtin".to_string(),
                        )],
                    ));
                }
            }
            _ => {}
        }

        if let Some(prev) = self.cur().storage.get(&name).cloned() {
            if prev.complete == flatten::TypeComplete::Complete
                && complete == &flatten::TypeComplete::Complete
            {
                return Err(Error::new(
                    format!("redeclation of local name '{}'", name),
                    vec![
                        (
                            loc.clone(),
                            "this declaration would shadow a previous name".to_string(),
                        ),
                        (prev.declared.clone(), "previous declaration".to_string()),
                    ],
                ));
            }
        }

        self.cur().storage.insert(
            name.clone(),
            Storage {
                typed: typed.clone(),
                name: name,
                declared: loc.clone(),
                complete: complete.clone(),
            },
        );

        Ok(())
    }

    pub fn struct_final_tail_type(
        &self,
        fields: &Vec<ast::Field>,
        tail: &ast::Tail,
    ) -> Result<Option<Box<ast::Typed>>, Error> {
        // find final tail type
        if let Some(field) = fields.last() {
            match &field.typed.tail {
                // String+ string;
                ast::Tail::Dynamic(_) => {
                    if let ast::Tail::Dynamic(_) = tail {
                    } else {
                        return Err(Error::new(
                            format!("undeclared nested tail"),
                            vec![(
                                field.loc.clone(),
                                format!(
                                    "nested tail but parent type not declared as having a tail"
                                ),
                            )],
                        ));
                    }
                    if let ast::Type::Other(name) = &field.typed.t {
                        match self.defs.get(name) {
                            Some(ast::Def::Struct { fields, tail, .. }) => {
                                return self.struct_final_tail_type(fields, tail);
                            }
                            other => {
                                return Err(Error::new(
                                    format!("unavailable type used as tail"),
                                    vec![(
                                        field.loc.clone(),
                                        format!("cannot use {} as struct: {:?}", name, other),
                                    )],
                                ));
                            }
                        }
                    } else {
                        return Err(Error::new(
                            format!("flat type used as tail"),
                            vec![(
                                field.loc.clone(),
                                format!("type {} has no tail", field.typed),
                            )],
                        ));
                    }
                }

                // no tail because last field is statically sized
                // String+100 string;
                ast::Tail::Static(_, _) => {
                    if tail != &ast::Tail::None {
                        return Err(Error::new(
                            format!("struct declared as having a tail, but has no tail"),
                            vec![(field.loc.clone(), format!("this is not a tail"))],
                        ));
                    }
                }
                ast::Tail::None => {
                    if let ast::Array::Unsized = field.array {
                        return Ok(Some(Box::new(field.typed.clone())));
                    } else {
                        if tail != &ast::Tail::None {
                            return Err(Error::new(
                                format!("struct declared as having a tail, but has no tail"),
                                vec![(field.loc.clone(), format!("this is not a tail"))],
                            ));
                        }
                    }
                }
                ast::Tail::Bind(_, _) => {
                    return Err(Error::new(
                        format!("tail sized binding on struct declaration is invalid"),
                        vec![(
                            field.loc.clone(),
                            format!("tail must be static number or unsized"),
                        )],
                    ));
                }
            }
        }
        Ok(None)
    }
}

pub fn expand(module: &mut flatten::Module) -> Result<(), Error> {
    let mut stack = Stack::new();
    stack.push("global".to_string());

    // declaration run
    for (d, complete) in &mut module.d {
        stack.defs.insert(Name::from(&d.name), d.def.clone());

        match &mut d.def {
            ast::Def::Theory { .. } => {
                stack.alloc(
                    Name::from(&d.name),
                    ast::Typed {
                        t: ast::Type::Other(Name::from(&d.name.clone())),
                        ptr: Vec::new(),
                        loc: d.loc.clone(),
                        tail: ast::Tail::None,
                    },
                    d.loc.clone(),
                    Tags::new(),
                    complete,
                )?;
            }
            ast::Def::Function {
                ..
            } => {
                stack.alloc(
                    Name::from(&d.name),
                    ast::Typed {
                        t: ast::Type::Other(Name::from(&d.name.clone())),
                        ptr: Vec::new(),
                        loc: d.loc.clone(),
                        tail: ast::Tail::None,
                    },
                    d.loc.clone(),
                    Tags::new(),
                    complete,
                )?;
            }
            ast::Def::Static {
                tags, typed, array, ..
            } => {
                let mut typed = typed.clone();
                match array {
                    ast::Array::None => (),
                    _ => {
                        typed.ptr.push(ast::Pointer {
                            loc: d.loc.clone(),
                            tags: Tags::new(),
                        });
                    }
                }

                stack.alloc(
                    Name::from(&d.name),
                    typed.clone(),
                    d.loc.clone(),
                    tags.clone(),
                    complete,
                )?;
            }
            ast::Def::Const { typed, .. } => {
                stack.alloc(
                    Name::from(&d.name),
                    typed.clone(),
                    d.loc.clone(),
                    Tags::new(),
                    complete,
                )?;
            }
            ast::Def::Closure { .. } => {
                stack.alloc(
                    Name::from(&d.name),
                    ast::Typed {
                        t: ast::Type::Other(Name::from(&d.name.clone())),
                        ptr: Vec::new(),
                        loc: d.loc.clone(),
                        tail: ast::Tail::None,
                    },
                    d.loc.clone(),
                    Tags::new(),
                    complete,
                )?;
            }
            ast::Def::Struct { .. } => {
                stack.alloc(
                    Name::from(&d.name),
                    ast::Typed {
                        t: ast::Type::Other(Name::from(&d.name.clone())),
                        ptr: Vec::new(),
                        loc: d.loc.clone(),
                        tail: ast::Tail::None,
                    },
                    d.loc.clone(),
                    Tags::new(),
                    complete,
                )?;
            }
            ast::Def::Symbol { .. } => {
                stack.alloc(
                    Name::from(&d.name),
                    ast::Typed {
                        t: ast::Type::Other(Name::from(&d.name.clone())),
                        ptr: Vec::new(),
                        loc: d.loc.clone(),
                        tail: ast::Tail::None,
                    },
                    d.loc.clone(),
                    Tags::new(),
                    complete,
                )?;
            }
            ast::Def::Enum { .. } => {
                stack.alloc(
                    Name::from(&d.name),
                    ast::Typed {
                        t: ast::Type::Other(Name::from(&d.name.clone())),
                        ptr: Vec::new(),
                        loc: d.loc.clone(),
                        tail: ast::Tail::None,
                    },
                    d.loc.clone(),
                    Tags::new(),
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
            }
            ast::Def::Flags { .. } => {
            }
            ast::Def::Macro { .. } => {
                stack.alloc(
                    Name::from(&d.name),
                    ast::Typed {
                        t: ast::Type::Other(Name::from(&d.name.clone())),
                        ptr: Vec::new(),
                        loc: d.loc.clone(),
                        tail: ast::Tail::None,
                    },
                    d.loc.clone(),
                    Tags::new(),
                    complete,
                )?;
            }
            ast::Def::Testcase { .. } => {}
            ast::Def::Include { .. } => {}
        }
    }

    // definition run
    for (d, complete) in &mut module.d {
        match &mut d.def {
            ast::Def::Theory { .. } => {}
            ast::Def::Struct {
                fields,
                union,
                tail,
                ..
            } => {
                if *union {
                    for _field in fields.iter() {
                        //stack.cannot_drop_union(&field, &field.loc, 0)?;
                    }
                }

                let dump_fucking_rust = tail.clone();
                if let ast::Tail::Dynamic(ref mut fin) = tail {
                    let ff = stack.struct_final_tail_type(fields, &dump_fucking_rust)?;
                    *fin = ff;
                }
            }
            ast::Def::Function {
                args,
                body,
                callassert,
                callattests,
                calleffect,
                ..
            } => {
                for farg in args.iter_mut() {
                    if farg.typed.ptr.len() > 0 {
                        if Name::from(&d.name).0.last() != Some(&"borrow".to_string()) {
                            if let ast::Type::Other(name) = &farg.typed.t {
                                if let Some(ast::Def::Struct { impls, .. }) = stack.defs.get(name) {
                                    if let Some((fnname, _)) = impls.get("borrow") {
                                        if let Some(ast::Def::Function {
                                            calleffect: calleffect2,
                                            callassert: callassert2,
                                            ..
                                        }) = stack.defs.get(fnname)
                                        {
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
                            farg.tags.insert(
                                "no-borrow-expand".to_string(),
                                String::new(),
                                ast::Location::builtin(),
                            );
                        }

                        // safe is implicit unless the arg is marked unsafe
                        if !farg.tags.contains("unsafe") {
                            let loc = farg.typed.ptr[0].loc.clone();
                            let ast_safe = ast::Expression::Name(ast::Typed {
                                t: ast::Type::Other(Name::from("safe")),
                                ptr: Vec::new(),
                                loc: loc.clone(),
                                tail: ast::Tail::None,
                            });
                            let ast_argname = ast::Expression::Name(ast::Typed {
                                t: ast::Type::Other(Name::from(&farg.name)),
                                ptr: Vec::new(),
                                loc: loc.clone(),
                                tail: ast::Tail::None,
                            });
                            let ast_call = ast::Expression::Call {
                                loc: loc.clone(),
                                name: Box::new(ast_safe),
                                args: vec![Box::new(ast_argname)],
                                expanded: true,
                                emit: ast::EmitBehaviour::Default,
                            };
                            callassert.insert(0, ast_call.clone());
                        }
                    }
                }

                if complete == &flatten::TypeComplete::Complete {

                    for (_, branch_expr, body) in &mut body.branches {

                        if let Some(expr) = branch_expr {
                            stack.expand_expr(expr)?;
                        }

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
                }
            }
            _ => (),
        }
    }

    for (t, vrs) in stack.moretypevariants {
        for variant in vrs {
            module
                .typevariants
                .entry(t.clone())
                .or_insert(HashMap::new())
                .insert(variant.0, variant.1);
        }
    }

    Ok(())
}

impl Stack {
    fn expand_expr(&mut self, expr: &mut ast::Expression) -> Result<(), Error> {
        match expr {
            ast::Expression::Call {
                ref mut name,
                ref mut args,
                ..
            } => {
                self.expand_expr(name)?;
                for arg in args {
                    self.expand_expr(arg)?;
                }

                if let ast::Expression::Name(ftyped) = name.as_ref() {
                    if let ast::Type::Other(n) = &ftyped.t {
                        if let Some(ast::Def::Closure {  .. }) = self.defs.get(n) {
                            panic!("beep boop");
                        }
                    }
                }
            }
            ast::Expression::Name(_) => {}
            ast::Expression::MemberAccess { ref mut lhs, .. } => {
                self.expand_expr(lhs)?;
            }
            ast::Expression::ArrayAccess {
                ref mut lhs,
                ref mut rhs,
                ..
            } => {
                self.expand_expr(lhs)?;
                self.expand_expr(rhs)?;
            }
            ast::Expression::LiteralString { .. } => {}
            ast::Expression::LiteralChar { .. } => {}
            ast::Expression::Literal { .. } => {}
            ast::Expression::Infix {
                ref mut lhs,
                ref mut rhs,
                ..
            } => {
                self.expand_expr(lhs)?;
                self.expand_expr(rhs)?;
            }
            ast::Expression::Cast { ref mut expr, .. } => {
                self.expand_expr(expr)?;
            }
            ast::Expression::UnaryPost { ref mut expr, .. } => {
                self.expand_expr(expr)?;
            }
            ast::Expression::UnaryPre { ref mut expr, .. } => {
                self.expand_expr(expr)?;
            }
            ast::Expression::StructInit { ref mut fields, .. } => {
                for (_, expr) in fields {
                    self.expand_expr(expr)?;
                }
            }
            ast::Expression::ArrayInit { ref mut fields, .. } => {
                for expr in fields {
                    self.expand_expr(expr)?;
                }
            }
            ast::Expression::MacroCall { ref mut args, .. } => {
                for arg in args {
                    self.expand_expr(arg)?;
                }
            }
            ast::Expression::Unsafe { .. } => {}
            ast::Expression::Cpp{expr, ..} => {
                self.expand_expr(expr)?;
            }
        }
        Ok(())
    }

    fn expand_scope(&mut self, body: &mut Vec<Box<ast::Statement>>) -> Result<(), Error> {
        let mut i = 0;
        let mut len = body.len();
        while i < len {
            match body[i].as_mut() {
                ast::Statement::Var {
                    loc,
                    typed,
                    tags,
                    name,
                    array,
                    assign,
                    ..
                } => {
                    if let ast::Type::New = typed.t {
                        if !tags.contains("mut") {
                            tags.insert("mut".to_string(), String::new(), loc.clone());
                        }

                        if array.is_some() {
                            return Err(Error::new(
                                format!("new stack initialization cannot be array"),
                                vec![(loc.clone(), "this new statement is invalid".to_string())],
                            ));
                        }
                        if assign.is_none() {
                            return Err(Error::new(format!("new stack initialization requires function call to constructor"), vec![
                                (loc.clone(), "this new statement is uninitialized".to_string()),
                            ]));
                        }
                        let assign = assign.as_mut().unwrap();
                        if let ast::Expression::Call {
                            loc,
                            args,
                            name: fname,
                            ..
                        } = assign
                        {
                            let mut nuargpos = None;

                            let mut fname = fname.clone();
                            self.expand_expr(&mut fname)?;


                            if let ast::Expression::Name(ftyped) = fname.as_ref() {
                                if let ast::Type::Other(n) = &ftyped.t {
                                    if let Some(ast::Def::Function { args, .. }) = self.defs.get(n)
                                    {
                                        for (n, arg) in args.iter().enumerate() {
                                            if let Some(v) = arg.tags.get("new") {
                                                for (_,v) in v {
                                                    return Err(Error::new(format!("invalid use of new tag"), vec![
                                                                          (v.clone(), "a local scope variable cannot be \"new\". you probably wanted the tag on the pointer".to_string()),
                                                    ]));
                                                }
                                            }

                                            if arg.typed.ptr.len() < 1 {
                                                continue
                                            }

                                            if !arg.typed.ptr[0].tags.contains("new") {
                                                continue;
                                            }

                                            if ! (n == args.len() - 1 || n == 0) {
                                                return Err(Error::new(format!("new must be first or last argument"), vec![
                                                    (arg.loc.clone(), "cannot insert new into the middle or arglist".to_string()),
                                                ]));
                                            }

                                            if nuargpos.is_some() {
                                                return Err(Error::new(format!("only one argument can be new"), vec![
                                                    (arg.loc.clone(), "second new argument".to_string()),
                                                ]));
                                            }

                                            if arg.typed.ptr.len() != 1 {
                                                return Err(Error::new(format!("new stack initialization requires function call to constructor"), vec![
                                                    (loc.clone(), "incorrect new argument pointer length".to_string()),
                                                ]));
                                            }
                                            let tail = typed.tail.clone();
                                            *typed = arg.typed.clone();
                                            typed.ptr = Vec::new();

                                            if let ast::Type::Other(tn) = &typed.t {
                                                if let ast::Tail::Static(f, tvloc) = &tail {
                                                    self.moretypevariants
                                                        .entry(tn.clone())
                                                        .or_insert(HashMap::new())
                                                        .insert(*f, tvloc.clone());
                                                }
                                            }
                                            typed.tail = tail;
                                            nuargpos = Some(n);
                                        }

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
                                    (loc.clone(), format!("this new statement is {:?}", fname)),
                                ]));
                            }

                            if nuargpos.is_none() {
                                return Err(Error::new(format!("new stack initialization requires function call to constructor"), vec![
                                    (loc.clone(), "function has no argument with a \"new\" pointer".to_string()),
                                ]));
                            }

                            let nua = Box::new(ast::Expression::UnaryPre {
                                loc: loc.clone(),
                                op: ast::PrefixOperator::AddressOf,
                                expr: Box::new(ast::Expression::Name(ast::Typed {
                                    t: ast::Type::Other(Name::from(name.as_str())),
                                    ptr: Vec::new(),
                                    tail: ast::Tail::None,
                                    loc: loc.clone(),
                                })),
                            });
                            if nuargpos.unwrap() == 0 {
                                args.insert(0, nua);
                            } else {
                                args.push(nua);
                            }
                        } else {
                            return Err(Error::new(format!("new stack initialization requires function call to constructor"), vec![
                                (loc.clone(), "this new statement is invalid".to_string()),
                            ]));
                        }
                        let stm = Box::new(ast::Statement::Expr {
                            loc: assign.loc().clone(),
                            expr: assign.clone(),
                        });
                        *assign = ast::Expression::ArrayInit {
                            loc: loc.clone(),
                            fields: vec![Box::new(ast::Expression::Literal {
                                loc: loc.clone(),
                                v: "0".to_string(),
                            })],
                        };
                        body.insert(i + 1, stm);
                        i += 1;
                        len += 1;
                    } else {
                        let mut typed = typed.clone();
                        if array.is_some() {
                            typed.ptr.push(ast::Pointer {
                                loc: loc.clone(),
                                tags: Tags::new(),
                            });
                        }
                        self.alloc(
                            Name::from(name.as_str()),
                            typed,
                            loc.clone(),
                            tags.clone(),
                            &flatten::TypeComplete::Complete,
                        )?;
                        if let Some(expr) = assign {
                            self.expand_expr(expr)?;
                        }
                    }
                }
                ast::Statement::If { branches } => {
                    self.push("if".to_string());
                    for (_loc, _expr, block) in branches {
                        self.push("branch".to_string());
                        self.expand_scope(&mut block.statements)?;
                        self.pop();
                    }
                    self.pop();
                }
                ast::Statement::Expr { expr, .. } => {
                    self.expand_expr(expr)?;
                }
                ast::Statement::Return { loc, expr, .. } => {
                    if let Some(expr) = expr {
                        self.expand_expr(expr)?;
                    }

                    let r = self.drop_fn(&loc)?;
                    for stm in r.into_iter().rev() {
                        body.insert(i, stm);
                        i += 1;
                        len += 1;
                    }
                }
                ast::Statement::Label { .. } => {}
                ast::Statement::Mark { .. } => {}
                ast::Statement::Switch { cases, .. } => {
                    for (_, block) in cases {
                        self.push("case".to_string());
                        self.expand_scope(&mut block.statements)?;
                        self.pop();
                    }
                }
                ast::Statement::Assign { lhs, rhs, .. } => {
                    self.expand_expr(lhs)?;
                    self.expand_expr(rhs)?;
                }

                ast::Statement::Continue { .. } => {}
                ast::Statement::Break { loc } => {
                    let r = self.drop(&loc)?;
                    for stm in r.into_iter().rev() {
                        body.insert(i, stm);
                        i += 1;
                        len += 1;
                    }
                }
                ast::Statement::Unsafe(block) | ast::Statement::Block(block) => {
                    self.push("block".to_string());
                    self.expand_scope(&mut block.statements)?;
                    block.statements.extend(self.drop(&block.end)?);
                    self.pop();
                }
                ast::Statement::For {
                    e1, e2, e3, body, ..
                } => {
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
                ast::Statement::While { body, expr, .. } => {
                    self.push("while loop".to_string());
                    self.expand_scope(&mut body.statements)?;
                    body.statements.extend(self.drop(&body.end)?);
                    self.pop();
                    self.expand_expr(expr)?;
                }
                ast::Statement::CBlock { .. } => {}
                ast::Statement::MacroCall {  .. } => {}
            }

            i += 1;
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

    fn drop_frame(
        &mut self,
        loc: &ast::Location,
        frame: usize,
    ) -> Result<Vec<Box<ast::Statement>>, Error> {
        let r = Vec::new();
        for (name, storage) in &self.stack[frame].storage {
            //TODO also drop owned pointers some day

            if storage.typed.ptr.len() != 0 {
                continue;
            }

            let _accesslocal = ast::Expression::UnaryPre {
                loc: loc.clone(),
                op: ast::PrefixOperator::AddressOf,
                expr: Box::new(ast::Expression::Name(ast::Typed {
                    t: ast::Type::Other(name.clone()),
                    ptr: Vec::new(),
                    loc: loc.clone(),
                    tail: ast::Tail::None,
                })),
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
        ast::Expression::MemberAccess { ref mut lhs, .. } => {
            replace_named(lhs, replacefrom, replacewith);
        }
        ast::Expression::ArrayAccess {
            ref mut lhs,
            ref mut rhs,
            ..
        } => {
            replace_named(lhs, replacefrom, replacewith);
            replace_named(rhs, replacefrom, replacewith);
        }
        ast::Expression::LiteralString { .. } => {}
        ast::Expression::LiteralChar { .. } => {}
        ast::Expression::Literal { .. } => {}
        ast::Expression::Call {
            ref mut name,
            ref mut args,
            ..
        } => {
            replace_named(name, replacefrom, replacewith);
            for arg in args {
                replace_named(arg, replacefrom, replacewith);
            }
        }
        ast::Expression::MacroCall { ref mut args, .. } => {
            for arg in args {
                replace_named(arg, replacefrom, replacewith);
            }
        }
        ast::Expression::Infix {
            ref mut lhs,
            ref mut rhs,
            ..
        } => {
            replace_named(lhs, replacefrom, replacewith);
            replace_named(rhs, replacefrom, replacewith);
        }
        ast::Expression::Cast { ref mut expr, .. } => {
            replace_named(expr, replacefrom, replacewith);
        }
        ast::Expression::Unsafe { ref mut expr, .. } => {
            replace_named(expr, replacefrom, replacewith);
        }
        ast::Expression::UnaryPost { ref mut expr, .. } => {
            replace_named(expr, replacefrom, replacewith);
        }
        ast::Expression::UnaryPre { ref mut expr, .. } => {
            replace_named(expr, replacefrom, replacewith);
        }
        ast::Expression::StructInit { ref mut fields, .. } => {
            for (_, expr) in fields {
                replace_named(expr, replacefrom, replacewith);
            }
        }
        ast::Expression::ArrayInit { ref mut fields, .. } => {
            for expr in fields {
                replace_named(expr, replacefrom, replacewith);
            }
        }
        ast::Expression::Cpp{..} => {}
    }
}
