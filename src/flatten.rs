use super::abs::Ext;
use super::ast;
use super::loader;
use super::name::Name;
use super::parser::emit_error;
use std::collections::HashMap;
use std::collections::HashSet;
use std::path::PathBuf;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum TypeComplete {
    Incomplete = 1,
    Complete = 2,
}

#[derive(Clone, Debug, Default)]
struct TypeSet(pub HashMap<Name, TypeComplete>);
impl TypeSet {
    pub fn insert(&mut self, name: Name, tc: TypeComplete) {
        match self.0.get(&name) {
            Some(cc) if cc >= &tc => {}
            _ => {
                self.0.insert(name, tc);
            }
        };
    }
}

#[derive(Clone, Default)]
pub struct Module {
    pub name: Name,
    pub sources: HashSet<PathBuf>,
    pub c_names: HashMap<Name, ast::Location>,

    pub d: Vec<(ast::Local, TypeComplete)>,

    pub aliases: HashMap<Name, String>,
    pub deps: HashSet<Name>,

    pub typevariants: HashMap<Name, HashMap<u64, ast::Location>>,
}

#[derive(Clone)]
struct Local {
    decl_deps: Vec<(Name, TypeComplete, ast::Location)>,
    impl_deps: Vec<(Name, TypeComplete, ast::Location)>,
    use_deps: Vec<(Name, ast::Location)>,
    ast: Option<ast::Local>,
    in_scope_here: ast::Location,
}

#[derive(Default)]
struct Collector {
    typevariants: HashMap<Name, HashMap<u64, ast::Location>>,
}

#[derive(Default)]
struct Locals(HashMap<Name, Local>);

fn tag_deps(_cr: &mut Collector, tags: &ast::Tags) -> Vec<(Name, TypeComplete, ast::Location)> {
    let mut r = Vec::new();

    for (_, vals) in tags.0.iter() {
        for (k, loc) in vals.iter() {
            let name = Name::from(k);
            if name.is_absolute() && name.len() > 2 {
                r.push((name, TypeComplete::Incomplete, loc.clone()));
            }
        }
    }
    r
}

fn type_deps(cr: &mut Collector, typed: &ast::Typed) -> Vec<(Name, TypeComplete, ast::Location)> {
    let mut r = Vec::new();
    if let ast::Type::Other(name) = &typed.t {
        let cc = if typed.ptr.len() > 0 {
            TypeComplete::Incomplete
        } else {
            TypeComplete::Complete
        };
        r.push((name.clone(), cc, typed.loc.clone()));
    }
    for ptr in &typed.ptr {
        r.extend(tag_deps(cr, &ptr.tags));
    }

    if let ast::Tail::Static(v, vloc) = &typed.tail {
        if let ast::Type::Other(name) = &typed.t {
            cr.typevariants
                .entry(name.clone())
                .or_default()
                .insert(v.clone(), vloc.clone());
        }
    }

    r
}

fn stm_deps(cr: &mut Collector, stm: &ast::Statement) -> Vec<(Name, TypeComplete, ast::Location)> {
    match stm {
        ast::Statement::Mark { .. } | ast::Statement::Label { .. } => Vec::new(),
        ast::Statement::Block(b2) => block_deps(cr, b2),
        ast::Statement::Unsafe(b2) => block_deps(cr, b2),
        ast::Statement::Switch {
            expr,
            cases,
            default,
            ..
        } => {
            let mut deps = expr_deps(cr, expr);
            for (exprs, body) in cases {
                for expr in exprs {
                    deps.extend(expr_deps(cr, expr));
                }
                deps.extend(block_deps(cr, body));
            }
            if let Some(default) = default {
                deps.extend(block_deps(cr, default));
            }
            deps
        }
        ast::Statement::For { e1, e2, e3, body } => {
            let mut deps = Vec::new();
            for s in e1 {
                deps.extend(stm_deps(cr, s));
            }
            for s in e2 {
                deps.extend(expr_deps(cr, s));
            }
            for s in e3 {
                deps.extend(stm_deps(cr, s));
            }
            deps.extend(block_deps(cr, body));
            deps
        }
        ast::Statement::While { expr, body } => {
            let mut deps = Vec::new();
            deps.extend(expr_deps(cr, expr));
            deps.extend(block_deps(cr, body));
            deps
        }
        ast::Statement::If { branches } => {
            let mut deps = Vec::new();
            for (_, expr, body) in branches {
                if let Some(expr) = expr {
                    deps.extend(expr_deps(cr, expr));
                }
                deps.extend(block_deps(cr, body));
            }
            deps
        }
        ast::Statement::Assign { lhs, rhs, .. } => {
            let mut deps = Vec::new();
            deps.extend(expr_deps(cr, lhs));
            deps.extend(expr_deps(cr, rhs));
            deps
        }
        ast::Statement::Var {
            assign,
            typed,
            array,
            ..
        } => {
            let mut deps = Vec::new();
            if let Some(array) = &array {
                if let Some(array) = &array {
                    deps.extend(expr_deps(cr, array));
                }
            }
            if let Some(assign) = &assign {
                deps.extend(expr_deps(cr, assign));
            }
            deps.extend(type_deps(cr, &typed));
            deps
        }
        ast::Statement::Expr { expr, .. } => expr_deps(cr, expr),
        ast::Statement::Return { expr, .. } => {
            if let Some(expr) = expr {
                expr_deps(cr, expr)
            } else {
                Vec::new()
            }
        }
        ast::Statement::Continue { .. } => Vec::new(),
        ast::Statement::Break { .. } => Vec::new(),
        ast::Statement::CBlock { .. } => Vec::new(),
        ast::Statement::MacroCall {
            loc, name, args, ..
        } => {
            let mut v = Vec::new();
            v.push((name.clone(), TypeComplete::Incomplete, loc.clone()));
            for arg in args {
                v.extend(expr_deps(cr, arg));
            }
            v
        }
    }
}

fn block_deps(cr: &mut Collector, block: &ast::Block) -> Vec<(Name, TypeComplete, ast::Location)> {
    let mut deps = Vec::new();
    for stm in &block.statements {
        deps.extend(stm_deps(cr, stm));
    }
    deps
}

fn expr_deps(
    cr: &mut Collector,
    expr: &ast::Expression,
) -> Vec<(Name, TypeComplete, ast::Location)> {
    match expr {
        ast::Expression::ArrayInit { fields, .. } => {
            let mut v = Vec::new();
            for expr in fields {
                v.extend(expr_deps(cr, expr));
            }
            v
        }
        ast::Expression::StructInit { typed, fields, .. } => {
            let mut v = Vec::new();
            v.extend(type_deps(cr, &typed));
            for (_, expr) in fields {
                v.extend(expr_deps(cr, expr));
            }
            v
        }
        ast::Expression::Name(name) => {
            if let ast::Type::Other(n) = &name.t {
                if n.len() > 2 {
                    vec![(n.clone(), TypeComplete::Incomplete, name.loc.clone())]
                } else {
                    Vec::new()
                }
            } else {
                Vec::new()
            }
        }
        ast::Expression::Cast { expr, into, .. } => {
            let mut v = Vec::new();
            if let ast::Type::Other(n) = &into.t {
                let cc = if into.ptr.len() > 0 {
                    TypeComplete::Incomplete
                } else {
                    TypeComplete::Complete
                };
                v.push((n.clone(), cc, into.loc.clone()));
            }
            v.extend(expr_deps(cr, expr));
            v
        }
        ast::Expression::Unsafe { expr, into, .. } => {
            let mut v = Vec::new();
            if let ast::Type::Other(n) = &into.t {
                let cc = if into.ptr.len() > 0 {
                    TypeComplete::Incomplete
                } else {
                    TypeComplete::Complete
                };
                v.push((n.clone(), cc, into.loc.clone()));
            }
            v.extend(expr_deps(cr, expr));
            v
        }
        ast::Expression::Literal { .. }
        | ast::Expression::LiteralString { .. }
        | ast::Expression::LiteralChar { .. } => Vec::new(),
        ast::Expression::Call { name, args, .. } => {
            let mut v = Vec::new();
            v.extend(expr_deps(cr, name));
            for arg in args {
                v.extend(expr_deps(cr, arg));
            }
            v
        }
        ast::Expression::MacroCall {
            loc, name, args, ..
        } => {
            let mut v = Vec::new();
            v.push((name.clone(), TypeComplete::Incomplete, loc.clone()));
            for arg in args {
                v.extend(expr_deps(cr, arg));
            }
            v
        }
        ast::Expression::UnaryPost { expr, .. } => expr_deps(cr, expr),
        ast::Expression::UnaryPre { expr, .. } => expr_deps(cr, expr),
        ast::Expression::MemberAccess { lhs, .. } => expr_deps(cr, lhs),
        ast::Expression::ArrayAccess { lhs, rhs, .. } => {
            let mut v = Vec::new();
            v.extend(expr_deps(cr, lhs));
            v.extend(expr_deps(cr, rhs));
            v
        }
        ast::Expression::Infix { lhs, rhs, .. } => {
            let mut v = Vec::new();
            v.extend(expr_deps(cr, lhs));
            v.extend(expr_deps(cr, rhs));
            v
        }
    }
}

pub fn flatten(md: &ast::Module, all_modules: &HashMap<Name, loader::Module>, ext: Ext) -> Module {
    debug!("flatten {}", md.name);

    let mut flat = Module::default();
    flat.name = md.name.clone();
    flat.sources = md.sources.clone();

    let mut collected = Locals::default();

    let mut injected: HashMap<Name, Vec<(Name, ast::Location)>> = Default::default();

    let mut incomming: Vec<(Name, ast::Location)> = Vec::new();

    let mut thisobject = TypeSet::default();

    // functions which cannot be forward declared
    let mut forceinline = HashSet::new();

    let mut collector = Collector::default();
    let cr = &mut collector;

    for local in &md.locals {
        let mut ns = md.name.clone();
        ns.push(local.name.clone());
        debug!("  local from abs.md: {}", local.name);
        thisobject.insert(ns.clone(), TypeComplete::Complete);
        incomming.push((ns, local.loc.clone()));
    }

    let mut incomming_imports = Vec::new();
    for import in &md.imports {
        incomming_imports.push(import.clone());
    }

    let mut visited: HashSet<Name> = HashSet::new();

    while incomming.len() > 0 {
        for (name, loc) in std::mem::replace(&mut incomming, Vec::new()) {
            if !visited.insert(name.clone()) {
                continue;
            }
            debug!("  localizing {}", name);

            if !name.is_absolute() {
                emit_error(
                    format!("undefined type '{}' during flatten of '{}'", name, md.name),
                    &[(
                        loc.clone(),
                        &format!("type '{}' unavailable in this scope", name),
                    )],
                );
                std::process::exit(9);
            }

            let mut module_name = name.clone();
            let mut local_name = module_name.pop().unwrap();
            let mut expecting_sub_type = false;
            let mut local = None;

            if name.0[1] == "ext" {
                local = ext.ext.lock().unwrap().get(&module_name).cloned();
                if local.is_none() {
                    emit_error(
                        format!(
                            "ICE ext module {} unavable or somehow we're missing local {}",
                            module_name, local_name
                        ),
                        &[(
                            loc.clone(),
                            &format!("type '{}' unavailable in this scope", name),
                        )],
                    );
                    std::process::exit(9);
                }
            } else {
                let module = loop {
                    let module = if module_name == md.name {
                        &md
                    } else {
                        match all_modules.get(&module_name) {
                            None => {
                                // try moving left
                                if module_name.len() > 2 {
                                    local_name = module_name.pop().unwrap();
                                    expecting_sub_type = true;
                                    if let Some(loader::Module::ZZ(ast)) =
                                        all_modules.get(&module_name)
                                    {
                                        break ast;
                                    } else {
                                        continue;
                                    }
                                }

                                emit_error(
                                    format!("ice: unknown module {}", module_name),
                                    &[(
                                        loc.clone(),
                                        &format!("type '{}' unavailable in this scope", name),
                                    )],
                                );
                                std::process::exit(9);
                            }
                            Some(loader::Module::C(_)) => panic!("not implemented"),
                            Some(loader::Module::ZZ(ast)) => ast,
                        }
                    };
                    break module;
                };

                // find the local we're looking for
                for local2 in &module.locals {
                    if local2.name == local_name {
                        local = Some(local2.clone());
                    } else {
                        //TODO
                        //the individual import is from a time where things worked differently.
                        //it's actually easier now to just pull in the entire module
                        //so we get impl functions
                        //which we're going to do here as a quick fix
                        let mut nnn = module_name.clone();
                        nnn.push(local2.name.clone());
                        incomming.push((nnn, loc.clone()));
                    }
                }

                flat.deps.insert(module_name.clone());
            };

            // should have been cought by abs
            let local = match local {
                Some(v) => v,
                None => {
                    emit_error(
                        format!("module {} does not contain {}", module_name, local_name),
                        &[(
                            loc.clone(),
                            &format!("type '{}' unavailable in this scope", name),
                        )],
                    );
                    std::process::exit(9);
                }
            };

            let mut ast = local.clone();
            let ast_name = local.name.clone();

            let mut decl_deps: Vec<(Name, TypeComplete, ast::Location)> = Vec::new();
            let mut impl_deps: Vec<(Name, TypeComplete, ast::Location)> = Vec::new();

            match &local.def {
                ast::Def::Enum { names, .. } => {
                    let mut ns = module_name.clone();
                    ns.push(ast_name.clone());

                    expecting_sub_type = false;
                    //flat.aliases.insert(name.clone(), format!("enum {}", name.0[1..].join("_")));
                    for (subname, _) in names {
                        let mut name = name.clone();
                        name.push(subname.clone());
                        collected.0.insert(
                            name,
                            Local {
                                impl_deps: Vec::new(),
                                decl_deps: vec![(ns.clone(), TypeComplete::Complete, loc.clone())],
                                use_deps: Vec::new(),
                                ast: None,
                                in_scope_here: loc.clone(),
                            },
                        );
                    }
                    forceinline.insert(name.clone());
                }
                ast::Def::Symbol {} => {
                    forceinline.insert(name.clone());
                }
                ast::Def::Static { typed, expr, .. } => {
                    decl_deps.extend(type_deps(cr, &typed));
                    decl_deps.extend(expr_deps(cr, expr));
                    forceinline.insert(name.clone());
                }
                ast::Def::Const { typed, expr, .. } => {
                    decl_deps.extend(type_deps(cr, &typed));
                    decl_deps.extend(expr_deps(cr, expr));
                    forceinline.insert(name.clone());
                }
                ast::Def::Function {
                    ret,
                    args,
                    body,
                    callassert,
                    calleffect,
                    attr,
                    ..
                } => {
                    if let Some(ret) = ret {
                        decl_deps.extend(type_deps(cr, &ret.typed));
                    }

                    for (i, arg) in args.into_iter().enumerate() {
                        decl_deps.extend(type_deps(cr, &arg.typed));
                        decl_deps.extend(tag_deps(cr, &arg.tags));

                        if arg.name == "self" {
                            if i != 0 {
                                emit_error(
                                    format!("self arg must be first"),
                                    &[(arg.loc.clone(), "self argument in wrong position")],
                                );
                                std::process::exit(9);
                            }
                            if let ast::Type::Other(name) = &arg.typed.t {
                                let mut ns = module_name.clone();
                                ns.push(local.name.clone());

                                injected
                                    .entry(name.clone())
                                    .or_default()
                                    .push((ns, local.loc.clone()));
                            }
                        }
                    }

                    for expr in callassert {
                        decl_deps.extend(expr_deps(cr, expr));
                    }

                    for expr in calleffect {
                        decl_deps.extend(expr_deps(cr, expr));
                    }

                    impl_deps.extend(block_deps(cr, body));

                    if attr.contains_key("inline") {
                        forceinline.insert(name.clone());
                    }
                }
                ast::Def::Closure { ret, args, .. } => {
                    if let Some(ret) = ret {
                        decl_deps.extend(type_deps(cr, &ret.typed));
                    }
                    for arg in args {
                        decl_deps.extend(type_deps(cr, &arg.typed));
                        decl_deps.extend(tag_deps(cr, &arg.tags));
                    }
                }
                ast::Def::Theory { ret, args, .. } => {
                    if let Some(ret) = ret {
                        decl_deps.extend(type_deps(cr, &ret.typed));
                    }
                    for arg in args {
                        decl_deps.extend(type_deps(cr, &arg.typed));
                        decl_deps.extend(tag_deps(cr, &arg.tags));
                    }
                }
                ast::Def::Struct { fields, .. } => {
                    for field in fields {
                        impl_deps.extend(type_deps(cr, &field.typed));

                        if let ast::Array::Sized(ref expr) = &field.array {
                            impl_deps.extend(expr_deps(cr, expr));
                        }
                    }

                    //all structs eventually need to be emitted complete before function body
                    thisobject.insert(name.clone(), TypeComplete::Complete);
                }
                ast::Def::Macro { body, .. } => {
                    decl_deps.extend(block_deps(cr, body));
                    forceinline.insert(name.clone());
                }
                ast::Def::Testcase { fields, .. } => {
                    for (_, expr) in fields {
                        decl_deps.extend(expr_deps(cr, expr));
                    }
                    forceinline.insert(name.clone());
                }
                ast::Def::Include {
                    needs,
                    expr,
                    inline,
                    ..
                } => {
                    for (typed, _) in needs {
                        decl_deps.extend(type_deps(cr, &typed));
                    }

                    if *inline {
                        flat.sources.insert(PathBuf::from(&expr));
                    }
                    forceinline.insert(name.clone());
                }
            }

            if expecting_sub_type {
                panic!(
                    "ice: incorrectly resolved '{}' as local '{}' in module '{}' ",
                    name, local_name, module_name
                )
            }

            for (dep, _complete, loc) in &impl_deps {
                incomming.push((dep.clone(), loc.clone()));
            }
            for (dep, _complete, loc) in &decl_deps {
                incomming.push((dep.clone(), loc.clone()));
            }

            let ns = if module_name.0[1] == "ext" {
                Name::from(&ast_name)
            } else {
                let mut ns = module_name.clone();
                ns.push(ast_name.clone());
                ast.name = ns.to_string();
                ns
            };

            debug!("  done local: {}", ns);
            collected.0.insert(
                ns,
                Local {
                    decl_deps,
                    impl_deps,
                    use_deps: Vec::new(),
                    ast: Some(ast),
                    in_scope_here: loc,
                },
            );
        }

        for import in std::mem::replace(&mut incomming_imports, Vec::new()) {
            assert!(import.name.is_absolute(), "ice: not abs: {}", import.name);
            match all_modules.get(&import.name) {
                None => {
                    debug!("    < none {} (inline? {})", import.name, import.inline);
                    if import.name.0[1] == "ext" {
                        for (local, _) in &import.local {
                            let mut nn = import.name.clone();
                            nn.push(local.clone());
                            flat.c_names.insert(nn, import.loc.clone());
                        }

                    //ext.insert(import.name.clone(), ast::Local {
                    //    name:       import.name.to_string(),
                    //    vis:        ast::Visibility::Object,
                    //    loc:        import.loc.clone(),
                    //    def: ast::Def::Include {
                    //        expr:       import.name.0[2].clone(),
                    //        loc:        import.loc.clone(),
                    //        fqn:        import.name.clone(),
                    //        inline:     import.inline,
                    //        needs:      import.needs.clone(),
                    //    },
                    //});
                    } else if import.name == md.name {
                        // self import. do nothing
                        debug!("  self {} ", md.name);
                    } else {
                        panic!("ice: unknown module {}", import.name);
                    }
                }
                Some(loader::Module::C(path)) => {
                    debug!("    < C {}", import.name);
                    let mut included_names = Vec::new();
                    flat.sources.insert(path.clone());
                    for (local, _) in &import.local {
                        let mut nn = import.name.clone();
                        nn.push(local.clone());
                        included_names.push(nn.clone());
                        flat.c_names.insert(nn, import.loc.clone());
                    }

                    //ext.insert(import.name.clone(), ast::Local {
                    //    name:       import.name.to_string(),
                    //    vis:        ast::Visibility::Object,
                    //    loc:        import.loc.clone(),
                    //    def: ast::Def::Include {
                    //        expr:       import.name.0[2].clone(),
                    //        loc:        import.loc.clone(),
                    //        fqn:        import.name.clone(),
                    //        inline:     import.inline,
                    //        needs:      import.needs.clone(),
                    //    },
                    //});
                }
                Some(loader::Module::ZZ(ast)) => {
                    debug!("    < mod {}", import.name);
                    flat.deps.insert(ast.name.clone());
                    if import.local.len() == 0 {
                        for local in &ast.locals {
                            if local.vis != ast::Visibility::Object {
                                let mut ns = ast.name.clone();
                                ns.push(local.name.clone());
                                incomming.push((ns, import.loc.clone()));
                            }
                        }
                    } else {
                        for (local, import_as) in &import.local {
                            debug!("      < {}", local);
                            let mut nn = import.name.clone();
                            nn.push(local.clone());
                            incomming.push((nn.clone(), import.loc.clone()));
                            if let Some(import_as) = import_as {
                                flat.aliases.insert(nn, import_as.clone());
                            }
                        }
                    }
                }
            }
        }
    }

    // collect implementation dependencies into this object
    for (name, complete) in std::mem::replace(&mut thisobject, TypeSet::default()).0 {
        let n = collected.0.get(&name).unwrap();
        debug!("  collecting impl deps for thisobj : {}", name);
        thisobject.insert(name.clone(), complete.clone());

        if complete == TypeComplete::Complete {
            for (mut dep, complete, _) in n.impl_deps.clone() {
                if dep.0[1] == "ext" {
                    if dep.0.len() > 3 {
                        dep.0.truncate(3);
                    }
                }

                debug!("      <I   {} {:?}", dep, complete);
                thisobject.insert(dep.clone(), complete);
            }
        }
        for (mut dep, complete, _) in n.decl_deps.clone() {
            if dep.0[1] == "ext" {
                if dep.0.len() > 3 {
                    dep.0.truncate(3);
                }
            }
            debug!("      <D   {} {:?}", dep, complete);
            thisobject.insert(dep.clone(), complete);
        }
    }

    // drag along injections
    for (name, deps) in injected {
        if let Some(local) = collected.0.get_mut(&name) {
            if let Some(ast) = &mut local.ast {
                if let ast::Def::Struct { impls, .. } = &mut ast.def {
                    for dep in &deps {
                        impls.insert((dep.0).0.last().unwrap().clone(), dep.clone());
                    }
                }
            }
            local.use_deps.extend(deps);
        }
    }

    // recursively find all dependencies
    for (name, complete) in std::mem::replace(&mut thisobject, TypeSet::default()).0 {
        let mut visited = HashSet::new();
        dependency_visit(
            &mut visited,
            &collected.0,
            name,
            &complete,
            &mut thisobject,
            0,
        );
    }

    // sort dependencies

    let mut sorted = Vec::new();
    let mut sorted_mark = HashMap::new();
    let mut more = HashSet::new();
    for (name, complete) in &thisobject.0 {
        sort_visit(
            &mut sorted,
            &mut sorted_mark,
            &mut collected.0,
            name.clone(),
            complete.clone(),
            None,
            0,
            &forceinline,
            &mut more,
        );
    }
    for name in std::mem::replace(&mut more, HashSet::new()) {
        sort_visit(
            &mut sorted,
            &mut sorted_mark,
            &mut collected.0,
            name.clone(),
            TypeComplete::Incomplete,
            None,
            0,
            &forceinline,
            &mut more,
        );
    }

    debug!("sorted");

    for (name, mut complete, l) in sorted {
        if let Some(ast) = &l.ast {
            match ast.def {
                ast::Def::Function { .. } | ast::Def::Static { .. } => {
                    if !forceinline.contains(&name) {
                        if !thisobject.0.contains_key(&name) {
                            complete = TypeComplete::Incomplete;
                        }
                    }
                }
                _ => (),
            }
        }

        debug!(" {} {:?}", name, complete);
        for (dep, complete, _) in &l.decl_deps {
            debug!("    <D {:?} {}", complete, dep);
        }
        for (dep, complete, _) in &l.impl_deps {
            debug!("    <I {:?} {}", complete, dep);
        }

        if let Some(ast) = l.ast {
            flat.d.push((ast.clone(), complete));
        }
    }

    flat.typevariants = collector.typevariants;
    flat
}

fn sort_visit(
    sorted: &mut Vec<(Name, TypeComplete, Local)>,
    sorted_mark: &mut HashMap<Name, TypeComplete>,
    unsorted: &mut HashMap<Name, Local>,
    mut name: Name,
    mut complete: TypeComplete,
    here: Option<&ast::Location>,
    depth: usize,
    forceinline: &HashSet<Name>,
    more: &mut HashSet<Name>, // discover more dependencies
) {
    if name.0[1] == "ext" {
        if name.0.len() > 3 {
            name.0.truncate(3);
        }
    }

    debug!(
        "  {} sort_visit: {} {:?}",
        " ".repeat(depth),
        name,
        complete
    );

    if forceinline.contains(&name) {
        debug!("  {} forced inline", " ".repeat(depth));
        complete = TypeComplete::Complete;
    }

    if let Some(cc) = sorted_mark.get(&name) {
        if cc >= &complete {
            return;
        }
    }

    let n = match complete {
        TypeComplete::Incomplete => match unsorted.get(&name) {
            Some(n) => n.clone(),
            None => {
                debug!("  {} skipped (incomplete and not found", " ".repeat(depth));
                return;
            }
        },
        TypeComplete::Complete => match unsorted.remove(&name) {
            Some(v) => v,
            None => {
                let mut estack = Vec::new();
                if let Some(here) = here {
                    estack.push((here.clone(), format!("type incomplete in this scope")));
                }
                emit_error(
                    format!("recursive type {} will never complete", name),
                    &estack,
                );

                debug!("sorted:");
                for (name, _, _) in sorted {
                    debug!("  {}", name);
                }
                debug!("unsorted:");
                for (name, _) in unsorted {
                    debug!("  {}", name);
                }
                std::process::exit(10);
            }
        },
    };

    if complete == TypeComplete::Complete {
        for (dep, complete, loc) in &n.impl_deps {
            if dep == &name {
                continue;
            }
            sort_visit(
                sorted,
                sorted_mark,
                unsorted,
                dep.clone(),
                complete.clone(),
                Some(loc),
                depth + 1,
                forceinline,
                more,
            );
        }
    }

    for (dep, complete, loc) in &n.decl_deps {
        sort_visit(
            sorted,
            sorted_mark,
            unsorted,
            dep.clone(),
            complete.clone(),
            Some(loc),
            depth + 1,
            forceinline,
            more,
        );
    }

    debug!("  {} < marked : {} {:?}", " ".repeat(depth), name, complete);
    match sorted_mark.get(&name) {
        Some(cc) if cc >= &complete => {}
        _ => {
            sorted.push((name.clone(), complete.clone(), n.clone()));
        }
    };
    sorted_mark.insert(name.clone(), complete.clone());

    for (dep, _) in &n.use_deps {
        more.insert(dep.clone());
    }
}

fn dependency_visit(
    visited: &mut HashSet<Name>,
    unsorted: &HashMap<Name, Local>,
    mut name: Name,
    complete: &TypeComplete,
    thisobject: &mut TypeSet,
    depth: usize,
) {
    if name.0[1] == "ext" {
        if name.0.len() > 3 {
            name.0.truncate(3);
        }
    }

    if !visited.insert(name.clone()) {
        return;
    }

    thisobject.insert(name.clone(), complete.clone());

    let n = match unsorted.get(&name) {
        Some(v) => v,
        None => {
            return;
        }
    };

    for (dep, _, _) in &n.decl_deps {
        dependency_visit(
            visited,
            unsorted,
            dep.clone(),
            &TypeComplete::Incomplete,
            thisobject,
            depth + 1,
        );
    }

    for (dep, _) in &n.use_deps {
        dependency_visit(
            visited,
            unsorted,
            dep.clone(),
            &TypeComplete::Incomplete,
            thisobject,
            depth + 1,
        );
    }
}
