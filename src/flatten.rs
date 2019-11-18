use super::ast;
use super::name::Name;
use super::loader;
use super::parser::emit_error;
use std::collections::HashMap;
use std::collections::HashSet;
use std::path::PathBuf;


#[derive(Clone, Default)]
pub struct Module {
    pub name:           Name,
    pub sources:        HashSet<PathBuf>,
    pub c_names:        HashMap<Name, ast::Location>,

                                        //vv declare in this object, define in this object
    pub d:              Vec<(ast::Local, bool,                       bool)>,
    pub cincludes:      Vec<ast::Include>,


    pub aliases:        HashMap<Name, String>,
    pub deps:           HashSet<Name>,

    pub typevariants:   HashMap<Name, HashSet<u64>>,
}

struct Local {
    decl_deps:          Vec<(Name, ast::Location)>,
    impl_deps:          Vec<(Name, ast::Location)>,
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
        ast::Statement::Mark{lhs, .. } => {
            expr_deps(cr, lhs)
        },
        ast::Statement::Goto{..} |  ast::Statement::Label{..} => {
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
            for (loc, expr, body) in branches {
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
        ast::Expression::StaticError{message,loc}  => {
            emit_error(format!("ICE: {}", message), &[
                (loc.clone(), "here")
            ]);
            std::process::exit(9);
        },
    }
}

pub fn flatten(md: &mut ast::Module, all_modules: &HashMap<Name, loader::Module>) -> Module {
    debug!("flatten {}", md.name);

    let mut flat    = Module::default();
    flat.name       = md.name.clone();
    flat.sources    = md.sources.clone();

    let mut collected = Locals::default();
    let mut incomming = Vec::new();
    let mut inlined_includes = HashMap::new();
    let mut thisobject  = HashSet::new();
    let mut collector   = Collector::default();
    let cr = &mut collector;

    for local in &md.locals {
        let mut ns = md.name.clone();
        ns.push(local.name.clone());
        thisobject.insert(ns.clone());
        incomming.push((ns, local.loc.clone()));
    }

    let mut incomming_imports = Vec::new();
    for import in std::mem::replace(&mut md.imports, Vec::new()) {
        incomming_imports.push(import);
    }

    let mut recursion_guard : HashSet<Name> = HashSet::new();

    while incomming.len() > 0 {
        for (name, loc) in std::mem::replace(&mut incomming, Vec::new()) {
            if !recursion_guard.insert(name.clone())  {
                continue;
            }
            debug!("  localizing {}", name);

            if !name.is_absolute() {
                emit_error(format!("undefined type '{}' during flatten of '{}'", name, md.name), &[
                       (loc.clone(), &format!("type '{}' unavailable in this scope", name)),
                ]);
                std::process::exit(9);
            }



            if name.0[1] == "ext" {
                //TODO
                collected.0.insert(name.clone(), Local{
                    impl_deps: Vec::new(),
                    decl_deps: Vec::new(),
                    ast:  None,
                    in_scope_here: loc,
                });
                continue
            }

            let mut module_name = name.clone();
            let mut local_name = module_name.pop().unwrap();
            let mut expecting_sub_type = false;

            let module = loop {
                let module = if module_name == md.name { &md } else {match all_modules.get(&module_name) {
                    None => {
                        // try moving left
                        if module_name.len() > 3{
                            local_name = module_name.pop().unwrap();
                            expecting_sub_type = true;
                            if let Some(loader::Module::ZZ(ast)) = all_modules.get(&module_name) {
                                break ast;
                            } else {
                                continue;
                            }
                        }

                        panic!("ice: unknown module {}", module_name)
                    },
                    Some(loader::Module::C(_)) => panic!("not implemented"),
                    Some(loader::Module::ZZ(ast)) => ast,
                }};
                break module;
            };

            // find the local we're looking for
            let mut local = None;
            for local2 in &module.locals {
                if local2.name == local_name {
                    local = Some(local2);
                    break;
                }
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
                    for arg in args {
                        decl_deps.extend(type_deps(cr, &arg.typed));
                        decl_deps.extend(tag_deps(cr, &arg.tags));
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


            let mut ns = module_name.clone();
            ns.push(local.name.clone());
            let mut ast = local.clone();
            ast.name = ns.to_string();
            collected.0.insert(ns, Local{
                decl_deps,
                impl_deps,
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
                        if import.inline {
                            inlined_includes.insert(import.name.0[2].clone(), import);
                        }
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
                    if import.inline {
                        inlined_includes.insert(import.name.0[2].clone(), import);
                    }
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
        thisobject.insert(name.clone());
        for (dep,_) in &n.impl_deps {
            thisobject.insert(dep.clone());
        }
        for (dep,_) in &n.decl_deps {
            thisobject.insert(dep.clone());
        }
    }

    // recursively find all declaration dependencies
    for name in std::mem::replace(&mut thisobject, HashSet::new()) {
        let mut visited = HashSet::new();
        decl_visit(
            &mut visited,
            &collected.0,
            &name,
            &mut thisobject,
            0);
    }


    let mut sorted          = Vec::new();
    let mut sorted_mark     = HashSet::new();
    for name in &thisobject {
        sort_visit(
            &mut sorted,
            &mut sorted_mark,
            &mut collected.0 , &name,
        );
    }





    debug!("sorted");

    let mut included = HashMap::new();
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
        if let Some(ast) = &l.ast {
            flat.d.push((ast.clone(), decl_in_obj, def_in_obj));
        } else if name.0[1] == "ext" {
            let mut fqn = name.0.clone();
            fqn.pop();

            if decl_in_obj {
                included.entry(name.0[2].clone()).or_insert(ast::Include{
                    expr: name.0[2].clone(),
                    loc:  l.in_scope_here.clone(),
                    fqn:  Name(fqn),
                    inline: inlined_includes.contains_key(&name.0[2]),
                });
            }

        }
    }

    flat.cincludes = included.values().cloned().collect();


    /*
    for (_ ,i) in inlined_includes {
        if included.insert(i.name.0[2].clone()) {
            let mut fqn = i.name.0.clone();
            fqn.pop();
            let inc = ast::Include{
                expr: i.name.0[2].clone(),
                loc:  i.loc.clone(),
                fqn:  Name(fqn),
                inline: true,
            };
            flat.d.push((D::Include(inc), false));
        }
    }
    */

    flat.typevariants = collector.typevariants;
    flat
}

fn sort_visit(
        sorted: &mut Vec<(Name,Local)>,
        sorted_mark: &mut HashSet<Name>,
        unsorted: &mut HashMap<Name, Local>,
        name: &Name,
) {
    if sorted_mark.contains(name) {
        return;
    }
    let n = match unsorted.remove(name) {
        Some(v) => v,
        None => {
            eprintln!("recursive type {} will never complete.\ndecl_sorted:", name);
            eprintln!("sorted:");
            for (name, _) in sorted{
                eprintln!("  {}", name);
            }
            eprintln!("unsorted:");
            for (name, _) in unsorted {
                eprintln!("  {}", name);
            }
            std::process::exit(10);
        },
    };

    for (dep,_) in &n.impl_deps {
        sort_visit(sorted, sorted_mark, unsorted, dep);
    }

    for (dep,_) in &n.decl_deps {
        sort_visit(sorted, sorted_mark, unsorted, dep);
    }

    sorted_mark.insert(name.clone());
    sorted.push((name.clone(), n));
}

fn decl_visit(
        visited:    &mut HashSet<Name>,
        unsorted:   &HashMap<Name, Local>,
        name:       &Name,
        thisobject: &mut HashSet<Name>,
        depth:      usize,
) {

    if !visited.insert(name.clone()) {
        return;
    }

    thisobject.insert(name.clone());

    let n = match unsorted.get(name) {
        Some(v) => v,
        None => {
            return;
        },
    };

    for (dep,_) in &n.decl_deps {
        decl_visit(visited, unsorted, dep, thisobject, depth+1);
    }

}
