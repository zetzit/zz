use super::ast;
use super::name::Name;
use super::loader;
use super::parser::emit_error;
use std::collections::HashMap;
use std::collections::HashSet;
use std::path::PathBuf;
use super::abs::Ext;


#[derive(Clone, Default)]
pub struct Module {
    pub name:           Name,
    pub sources:        HashSet<PathBuf>,
    pub c_names:        HashMap<Name, ast::Location>,

                                        //vv declare in this object, define in this object
    pub d:              Vec<(ast::Local, bool,                       bool)>,


    pub aliases:        HashMap<Name, String>,
    pub deps:           HashSet<Name>,

    pub typevariants:   HashMap<Name, HashSet<u64>>,
}

#[derive(Clone)]
struct Local {
    decl_deps:          Vec<(Name, ast::Location)>,
    impl_deps:          Vec<(Name, ast::Location)>,
    use_deps:           Vec<(Name, ast::Location)>,
    ast:                Option<ast::Local>,
    in_scope_here:      ast::Location,
}


#[derive(Default)]
struct Collector {
    typevariants:   HashMap<Name, HashSet<u64>>,
}

#[derive(Default)]
struct Locals (HashMap<Name, Local>);

fn tag_deps(_cr: &mut Collector, tags: &ast::Tags) ->  Vec<(Name, ast::Location)> {
    let mut r = Vec::new();

    for (_,vals) in tags.0.iter() {
        for (k,loc) in vals.iter() {
            let name = Name::from(k);
            if name.is_absolute() && name.len() > 2 {
                r.push((name,loc.clone()));
            }
        }
    }
    r
}

fn type_deps(cr: &mut Collector, typed: &ast::Typed) -> Vec<(Name, ast::Location)> {
    let mut r = Vec::new();
    if let ast::Type::Other(name) = &typed.t {
        r.push((name.clone(), typed.loc.clone()));
    }
    for ptr in &typed.ptr {
        r.extend(tag_deps(cr, &ptr.tags));
    }

    if let ast::Tail::Static(v,_) = &typed.tail {
        if let ast::Type::Other(name) = &typed.t {
            cr.typevariants.entry(name.clone()).or_default().insert(v.clone());
        }
    }

    r
}

fn stm_deps(cr: &mut Collector, stm: &ast::Statement) -> Vec<(Name, ast::Location)> {
    match stm {
        ast::Statement::Mark{..} | ast::Statement::Label{..} => {
            Vec::new()
        },
        ast::Statement::Block(b2) => {
            block_deps(cr, b2)
        },
        ast::Statement::Unsafe(b2) => {
            block_deps(cr, b2)
        },
        ast::Statement::Switch{expr, cases, default, ..} => {
            let mut deps = expr_deps(cr, expr);
            for (_,body) in cases {
                deps.extend(block_deps(cr, body));
            }
            if let Some(default) =  default {
                deps.extend(block_deps(cr, default));
            }
            deps
        },
        ast::Statement::For{e1,e2,e3, body} => {
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
        },
        ast::Statement::While{expr,body} => {
            let mut deps = Vec::new();
            deps.extend(expr_deps(cr, expr));
            deps.extend(block_deps(cr, body));
            deps
        },
        ast::Statement::If{branches} => {
            let mut deps = Vec::new();
            for (_, expr, body) in branches {
                if let Some(expr) = expr {
                    deps.extend(expr_deps(cr, expr));
                }
                deps.extend(block_deps(cr, body));
            }
            deps
        },
        ast::Statement::Assign{lhs, rhs, ..}  => {
            let mut deps = Vec::new();
            deps.extend(expr_deps(cr, lhs));
            deps.extend(expr_deps(cr, rhs));
            deps
        },
        ast::Statement::Var{assign, typed, array, ..}  => {
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
        },
        ast::Statement::Expr{expr, ..} => {
            expr_deps(cr, expr)
        }
        ast::Statement::Return {expr, ..} => {
            if let Some(expr) = expr {
                expr_deps(cr, expr)
            } else {
                Vec::new()
            }
        },
        ast::Statement::Continue{..} => {
            Vec::new()
        },
        ast::Statement::Break{..} => {
            Vec::new()
        },
        ast::Statement::CBlock{..} => {
            Vec::new()
        }
    }
}

fn block_deps(cr: &mut Collector,block: &ast::Block) -> Vec<(Name, ast::Location)> {
    let mut deps = Vec::new();
    for stm in &block.statements {
        deps.extend(stm_deps(cr, stm));
    }
    deps
}


fn expr_deps(cr: &mut Collector, expr: &ast::Expression) -> Vec<(Name, ast::Location)> {
    match expr {
        ast::Expression::ArrayInit{fields,..}  => {
            let mut v = Vec::new();
            for expr in fields {
                v.extend(expr_deps(cr, expr));
            }
            v
        },
        ast::Expression::StructInit{typed, fields,..}  => {
            let mut v = Vec::new();
            v.extend(type_deps(cr, &typed));
            for (_, expr) in fields {
                v.extend(expr_deps(cr, expr));
            }
            v
        },
        ast::Expression::Name(name)  => {
            if let ast::Type::Other(n) = &name.t {
                if n.len() > 2 {
                    vec![(n.clone(), name.loc.clone())]
                } else {
                    Vec::new()
                }
            } else {
                Vec::new()
            }
        },
        ast::Expression::Cast{expr, into,..} => {
            let mut v = Vec::new();
            if let ast::Type::Other(n) = &into.t {
                v.push((n.clone(), into.loc.clone()));
            }
            v.extend(expr_deps(cr, expr));
            v
        }
        ast::Expression::Literal {..} | ast::Expression::LiteralString {..} | ast::Expression::LiteralChar {..}=> {
            Vec::new()
        }
        ast::Expression::Call { name, args, ..} => {
            let mut v = Vec::new();
            v.extend(expr_deps(cr, name));
            for arg in args {
                v.extend(expr_deps(cr, arg));
            }
            v
        },
        ast::Expression::UnaryPost{expr, ..} => {
            expr_deps(cr, expr)
        }
        ast::Expression::UnaryPre{expr, ..} => {
            expr_deps(cr, expr)
        }
        ast::Expression::MemberAccess {lhs, ..} => {
            expr_deps(cr, lhs)
        }
        ast::Expression::ArrayAccess {lhs, rhs,.. } => {
            let mut v = Vec::new();
            v.extend(expr_deps(cr, lhs));
            v.extend(expr_deps(cr, rhs));
            v
        }
        ast::Expression::Infix {lhs, rhs,.. } => {
            let mut v = Vec::new();
            v.extend(expr_deps(cr, lhs));
            v.extend(expr_deps(cr, rhs));
            v
        }
    }
}

pub fn flatten(md: &mut ast::Module, all_modules: &HashMap<Name, loader::Module>, ext: &Ext) -> Module {
    debug!("flatten {}", md.name);

    let mut flat    = Module::default();
    flat.name       = md.name.clone();
    flat.sources    = md.sources.clone();

    let mut collected   = Locals::default();
    let mut injected    : HashMap<Name, Vec<(Name, ast::Location)>> = Default::default();
    let mut incomming   : Vec<(Name, ast::Location)>  = Vec::new();
    let mut thisobject  = HashSet::new();
    let mut collector   = Collector::default();
    let cr = &mut collector;


    for local in &md.locals {
        let mut ns = md.name.clone();
        ns.push(local.name.clone());
        debug!("  local from abs.md: {}", local.name);
        thisobject.insert(ns.clone());
        incomming.push((ns, local.loc.clone()));
    }

    let mut incomming_imports = Vec::new();
    for import in std::mem::replace(&mut md.imports, Vec::new()) {
        incomming_imports.push(import);
    }

    let mut visited : HashSet<Name> = HashSet::new();

    while incomming.len() > 0 {
        for (name, loc) in std::mem::replace(&mut incomming, Vec::new()) {
            if !visited.insert(name.clone())  {
                continue;
            }
            debug!("  localizing {}", name);

            if !name.is_absolute() {
                emit_error(format!("undefined type '{}' during flatten of '{}'", name, md.name), &[
                       (loc.clone(), &format!("type '{}' unavailable in this scope", name)),
                ]);
                std::process::exit(9);
            }


            let mut module_name = name.clone();
            let mut local_name = module_name.pop().unwrap();
            let mut expecting_sub_type = false;
            let mut local = None;

            if name.0[1] == "ext" {
                local = ext.ext.get(&module_name);
                if local.is_none() {
                    emit_error(format!("ICE ext module {} unavable or somehow we're missing local {}", module_name, local_name ), &[
                        (loc.clone(), &format!("type '{}' unavailable in this scope", name)),
                    ]);
                    std::process::exit(9);
                }
            } else {
                let module = loop {
                    let module = if module_name == md.name { &md } else {match all_modules.get(&module_name) {
                        None => {
                            // try moving left
                            if module_name.len() > 2{
                                local_name = module_name.pop().unwrap();
                                expecting_sub_type = true;
                                if let Some(loader::Module::ZZ(ast)) = all_modules.get(&module_name) {
                                    break ast;
                                } else {
                                    continue;
                                }
                            }

                            emit_error(format!("ice: unknown module {}", module_name), &[
                                       (loc.clone(), &format!("type '{}' unavailable in this scope", name)),
                            ]);
                            std::process::exit(9);
                        },
                        Some(loader::Module::C(_)) => panic!("not implemented"),
                        Some(loader::Module::ZZ(ast)) => ast,
                    }};
                    break module;
                };

                // find the local we're looking for
                for local2 in &module.locals {
                    if local2.name == local_name {
                        local = Some(local2);
                        break;
                    }
                };

            };

            // should have been cought by abs
            let local = match local {
                Some(v) => v,
                None => {
                    emit_error(format!("module {} does not contain {}", module_name, local_name ), &[
                               (loc.clone(), &format!("type '{}' unavailable in this scope", name)),
                    ]);
                    std::process::exit(9);
                }
            };

            let mut ast = local.clone();
            let ast_name = local.name.clone();

            let mut decl_deps : Vec<(Name, ast::Location)> = Vec::new();
            let mut impl_deps : Vec<(Name, ast::Location)> = Vec::new();
            match &local.def {
                ast::Def::Enum{names, ..} => {
                    expecting_sub_type = false;
                    //flat.aliases.insert(name.clone(), format!("enum {}", name.0[1..].join("_")));
                    for (subname, _) in names {
                        let mut name = name.clone();
                        name.push(subname.clone());
                        collected.0.insert(name, Local{
                            impl_deps:      Vec::new(),
                            decl_deps:      Vec::new(),
                            use_deps:       Vec::new(),
                            ast:            None,
                            in_scope_here:  loc.clone(),
                        });
                    }
                }
                ast::Def::Static{typed,expr,..} => {
                    decl_deps.extend(type_deps(cr, &typed));
                    decl_deps.extend(expr_deps(cr, expr));
                }
                ast::Def::Const{typed,expr, ..} => {
                    decl_deps.extend(type_deps(cr, &typed));
                    decl_deps.extend(expr_deps(cr, expr));
                }
                ast::Def::Function{ret, args, body, callassert, calleffect, .. } => {
                    if let Some(ret) = ret {
                        decl_deps.extend(type_deps(cr, &ret.typed));
                    }

                    for (i,arg) in args.into_iter().enumerate() {
                        decl_deps.extend(type_deps(cr, &arg.typed));
                        decl_deps.extend(tag_deps(cr, &arg.tags));

                        if arg.name == "self" {
                            if i != 0 {
                                emit_error(format!("self arg must be first"), &[
                                    (arg.loc.clone(), "self argument in wrong position"),
                                ]);
                                std::process::exit(9);

                            }
                            if let ast::Type::Other(name) = &arg.typed.t {

                                let mut ns = module_name.clone();
                                ns.push(local.name.clone());

                                injected.entry(name.clone()).or_default()
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
                }
                ast::Def::Fntype{ret, args, ..} => {
                    if let Some(ret) = ret {
                        decl_deps.extend(type_deps(cr, &ret.typed));
                    }
                    for arg in args {
                        decl_deps.extend(type_deps(cr, &arg.typed));
                        decl_deps.extend(tag_deps(cr, &arg.tags));
                    }
                }
                ast::Def::Theory{ret, args, ..} => {
                    if let Some(ret) = ret {
                        decl_deps.extend(type_deps(cr, &ret.typed));
                    }
                    for arg in args {
                        decl_deps.extend(type_deps(cr, &arg.typed));
                        decl_deps.extend(tag_deps(cr, &arg.tags));
                    }
                }
                ast::Def::Struct{fields,..} => {
                    for field in fields {
                        decl_deps.extend(type_deps(cr, &field.typed));
                        if let Some(ref expr) = &field.array {
                            if let Some(ref expr) = expr {
                                decl_deps.extend(expr_deps(cr, expr));
                            }
                        }
                    }
                }
                ast::Def::Macro{body, ..} => {
                    decl_deps.extend(block_deps(cr, body));
                }
                ast::Def::Testcase{fields, ..} => {
                    for (_, expr) in fields {
                        decl_deps.extend(expr_deps(cr, expr));
                    }
                }
                ast::Def::Include{needs, expr, inline,.. } => {
                    for (typed,_) in needs {
                        decl_deps.extend(type_deps(cr, &typed));
                    }

                    if *inline {
                        flat.sources.insert(PathBuf::from(&expr));
                    }
                }
            }

            if expecting_sub_type {
                panic!("ice: incorrectly resolved '{}' as local '{}' in module '{}' ", name, local_name, module_name)
            }

            for (dep,loc) in &impl_deps  {
                incomming.push((dep.clone(), loc.clone()));
            }
            for (dep,loc) in &decl_deps  {
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
            collected.0.insert(ns, Local{
                decl_deps,
                impl_deps,
                use_deps:  Vec::new(),
                ast: Some(ast),
                in_scope_here: loc,
            });
        }


        for import in std::mem::replace(&mut incomming_imports, Vec::new()) {
            assert!(import.name.is_absolute(), "ice: not abs: {}", import.name);
            match all_modules.get(&import.name) {
                None => {
                    debug!("    < none {} (inline? {})", import.name, import.inline);
                    if import.name.0[1] == "ext" {
                        for (local,_) in &import.local {
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
                },
                Some(loader::Module::C(path)) => {
                    debug!("    < C {}", import.name);
                    let mut included_names = Vec::new();
                    flat.sources.insert(path.clone());
                    for (local,_) in &import.local {
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


    let impl_in_thisobject = std::mem::replace(&mut thisobject, HashSet::new());
    // collect implementation dependencies into this object
    for name in &impl_in_thisobject {
        let n = collected.0.get(&name).unwrap();
        debug!("  thisobj from impl_in_thisobject: {}", name);
        thisobject.insert(name.clone());
        for (mut dep,_) in n.impl_deps.clone() {
            if dep.0[1] == "ext" {
                if dep.0.len() > 3 {
                    dep.0.truncate(3);
                }
            }

            debug!("      < : {}", dep);
            thisobject.insert(dep.clone());
        }
        for (mut dep,_) in n.decl_deps.clone() {
            if dep.0[1] == "ext" {
                if dep.0.len() > 3 {
                    dep.0.truncate(3);
                }
            }
            debug!("      < : {}", dep);
            thisobject.insert(dep.clone());
        }
    }



    // drag along injections
    for (name, deps) in injected {
        if let Some(local) = collected.0.get_mut(&name) {
            if let Some(ast) = &mut local.ast {
                if let ast::Def::Struct{impls,..} = &mut ast.def {
                    for dep in &deps {
                        impls.insert((dep.0).0.last().unwrap().clone(), dep.clone());
                    }
                }
            }
            local.use_deps.extend(deps);
        }
    }

    // recursively find all declaration dependencies
    for name in std::mem::replace(&mut thisobject, HashSet::new()) {
        let mut visited = HashSet::new();
        decl_visit(
            &mut visited,
            &collected.0,
            name,
            &mut thisobject,
            0);
    }


    let mut sorted          = Vec::new();
    let mut sorted_mark     = HashSet::new();
    for name in &thisobject {
        sort_visit(
            &mut sorted,
            &mut sorted_mark,
            &mut collected.0 ,
            name.clone(),
            None,
            false,
            0,
            None,
        );
    }





    debug!("sorted");

    for (name, l) in sorted {
        let decl_in_obj = thisobject.contains(&name);
        let def_in_obj  = impl_in_thisobject.contains(&name);
        debug!(" {} {}", name, if decl_in_obj {"<this object"} else {""});
        for (dep, _) in &l.decl_deps {
            debug!("    <D {}", dep);
        }
        for (dep, _) in &l.impl_deps {
            debug!("    <I {}", dep);
        }

        if let Some(ast) = l.ast {
            flat.d.push((ast.clone(), decl_in_obj, def_in_obj));
        }
    }

    flat.typevariants = collector.typevariants;
    flat
}

fn sort_visit(
        sorted:             &mut Vec<(Name,Local)>,
        sorted_mark:        &mut HashSet<Name>,
        unsorted:           &mut HashMap<Name, Local>,
        mut name:           Name,
        here:               Option<&ast::Location>,
        allow_recursion:    bool,
        depth:              usize,
        mut use_deps:       Option<&mut Vec<(Name, ast::Location)>>
)
{
    if name.0[1] == "ext" {
        if name.0.len() > 3 {
            name.0.truncate(3);
        }
    }

    if sorted_mark.contains(&name) {
        return;
    }

    debug!("  {} sort_visit: {} {}", " ".repeat(depth), name, if allow_recursion { "(recursion ok)"} else {""});

    let n = match unsorted.remove(&name) {
        Some(v) => v,
        None => {
            if allow_recursion {
                return;
            }
            let mut estack = Vec::new();
            if let Some(here) = here {
                estack.push((here.clone(), format!("type incomplete in this scope")));
            }
            emit_error(format!("recursive type {} will never complete", name), &estack);

            debug!("sorted:");
            for (name, _) in sorted{
                debug!("  {}", name);
            }
            debug!("unsorted:");
            for (name, _) in unsorted {
                debug!("  {}", name);
            }
            std::process::exit(10);
        },
    };


    let mut use_deps_here = Vec::new();


    for (dep,loc) in &n.impl_deps {
        if dep == &name {
            continue;
        }
        sort_visit(sorted, sorted_mark, unsorted, dep.clone(), Some(loc), true, depth+1, Some(&mut use_deps_here));
    }

    for (dep,loc) in &n.decl_deps {
        sort_visit(sorted, sorted_mark, unsorted, dep.clone(), Some(loc), false, depth+1, Some(&mut use_deps_here));
    }


    sorted_mark.insert(name.clone());
    sorted.push((name.clone(), n.clone()));

    for (dep,loc) in &use_deps_here {
        sort_visit(sorted, sorted_mark, unsorted, dep.clone(), Some(loc), true, depth+1, None);
    }


    if let Some(use_deps) = use_deps.as_mut() {
        use_deps.extend(n.use_deps.clone());
    }
}

fn decl_visit(
        visited:    &mut HashSet<Name>,
        unsorted:   &HashMap<Name, Local>,
        mut name:   Name,
        thisobject: &mut HashSet<Name>,
        depth:      usize,
) {
    if name.0[1] == "ext" {
        if name.0.len() > 3 {
            name.0.truncate(3);
        }
    }

    if !visited.insert(name.clone()) {
        return;
    }

    thisobject.insert(name.clone());

    let n = match unsorted.get(&name) {
        Some(v) => v,
        None => {
            return;
        },
    };

    for (dep,_) in &n.decl_deps {
        decl_visit(visited, unsorted, dep.clone(), thisobject, depth+1);
    }

    for (dep,_) in &n.use_deps {
        decl_visit(visited, unsorted, dep.clone(), thisobject, depth+1);
    }
}

impl Module {
    pub fn is_newer_than(&self, target: &str) -> bool {
        let itarget = match std::fs::metadata(&target) {
            Ok(v)  => v,
            Err(_) => return true,
        };
        let itarget = itarget.modified().expect(&format!("cannot stat {}", target));

        for source in &self.sources {
            let isource = std::fs::metadata(source).expect(&format!("cannot stat {:?}", source));

            let isource = isource.modified().expect(&format!("cannot stat {:?}", source));

            if isource > itarget {
                return true;
            }
        }
        return false;
    }
}
