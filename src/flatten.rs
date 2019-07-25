use super::ast;
use super::name::Name;
use super::loader;
use std::collections::HashMap;
use std::collections::HashSet;
use std::path::PathBuf;

#[derive(Clone)]
pub enum D {
    Include(ast::Include),
    Local(ast::Local),
}

#[derive(Clone, Default)]
pub struct Module {
    pub name:           Name,
    pub sources:        HashSet<PathBuf>,
    pub d:              Vec<D>,
    pub short_names:    HashSet<Name>,
    pub aliases:        HashMap<Name, String>,
    pub deps:           HashSet<Name>,
}


struct Local {
    deps:               Vec<Name>,
    ast:                Option<ast::Local>,
    in_scope_here:      ast::Location,
}

#[derive(Default)]
struct Locals (HashMap<Name, Local>);


fn stm_deps(stm: &ast::Statement) -> Vec<Name> {
    match stm {
        ast::Statement::Goto{..} |  ast::Statement::Label{..} => {
            Vec::new()
        },
        ast::Statement::Block(b2) => {
            block_deps(b2)
        },
        ast::Statement::For{e1,e2,e3, body} => {
            let mut deps = Vec::new();
            if let Some(s) = e1 {
                deps.extend(stm_deps(s));
            }
            if let Some(s) = e2 {
                deps.extend(stm_deps(s));
            }
            if let Some(s) = e3 {
                deps.extend(stm_deps(s));
            }
            deps.extend(block_deps(body));
            deps
        },
        ast::Statement::Cond{expr, body, ..} => {
            let mut deps = Vec::new();
            for expr in expr {
                deps.extend(expr_deps(expr));
            }
            deps.extend(block_deps(body));
            deps
        },
        ast::Statement::Assign{lhs, rhs, ..}  => {
            let mut deps = Vec::new();
            deps.extend(expr_deps(lhs));
            deps.extend(expr_deps(rhs));
            deps
        },
        ast::Statement::Var{assign, typeref, array, ..}  => {
            let mut deps = Vec::new();
            if let Some(array) = &array {
                deps.extend(expr_deps(array));
            }
            if let Some(assign) = &assign {
                deps.extend(expr_deps(assign));
            }
            deps.push(typeref.name.clone());
            deps
        },
        ast::Statement::Expr{expr, ..} => {
            expr_deps(expr)
        }
        ast::Statement::Return {expr, ..} => {
            expr_deps(expr)
        }
    }
}

fn block_deps(block: &ast::Block) -> Vec<Name> {
    let mut deps = Vec::new();
    for stm in &block.statements {
        deps.extend(stm_deps(stm));
    }
    deps
}


fn expr_deps(expr: &ast::Expression) -> Vec<Name> {
    match expr {
        ast::Expression::Name(name)  => {
            if name.name.len() > 2 {
                vec![name.name.clone()]
            } else {
                Vec::new()
            }
        },
        ast::Expression::Cast{expr, into} => {
            let mut v = Vec::new();
            v.push(into.name.clone());
            v.extend(expr_deps(expr));
            v
        }
        ast::Expression::Literal {..} => {
            Vec::new()
        }
        ast::Expression::Call { name, args, ..} => {
            let mut v = Vec::new();
            if name.name.len() > 2 {
                v.push(name.name.clone());
            }
            for arg in args {
                v.extend(expr_deps(arg));
            }
            v
        },
        ast::Expression::UnaryPost{expr, ..} => {
            expr_deps(expr)
        }
        ast::Expression::UnaryPre{expr, ..} => {
            expr_deps(expr)
        }
        ast::Expression::MemberAccess {lhs, ..} => {
            expr_deps(lhs)
        }
        ast::Expression::ArrayAccess {lhs, rhs,.. } => {
            let mut v = Vec::new();
            v.extend(expr_deps(lhs));
            v.extend(expr_deps(rhs));
            v
        }
        ast::Expression::InfixOperation {lhs, rhs,.. } => {
            let mut v = Vec::new();
            v.extend(expr_deps(lhs));
            for (_, rhs) in rhs {
                v.extend(expr_deps(rhs));
            }
            v
        }
    }
}

pub fn flatten(md: &mut ast::Module, all_modules: &HashMap<Name, loader::Module>) -> Module {
    debug!("flatten {}", md.name);

    let mut flat    = Module::default();
    flat.name       = md.name.clone();
    flat.sources    = md.sources.clone();

    let mut collected = Locals::default();
    let mut incomming = Vec::new();

    for local in &md.locals {
        let mut ns = md.name.clone();
        ns.push(local.name.clone());
        flat.short_names.insert(ns.clone());
        incomming.push((ns, local.loc.clone()));
    }



    let mut incomming_imports = Vec::new();
    for import in std::mem::replace(&mut md.imports, Vec::new()) {
        incomming_imports.push((import, true));
    }

    let mut recursion_guard : HashSet<Name> = HashSet::new();

    while incomming.len() > 0 {
        for (name, loc) in std::mem::replace(&mut incomming, Vec::new()) {
            if !recursion_guard.insert(name.clone())  {
                continue;
            }
            debug!("  localizing {}", name);

            assert!(name.is_absolute(), "is not absolute: {}", name);
            let mut module_name = name.clone();
            let local_name = module_name.pop().unwrap();


            if module_name.0[1] == "libc" {
                //TODO
                collected.0.insert(name.clone(), Local{
                    deps: Vec::new(),
                    ast:  None,
                    in_scope_here: loc,
                });
                continue
            }

            let module = if module_name == md.name { &md } else {match all_modules.get(&module_name) {
                None => {
                    panic!("ice: unknown module {}", module_name)
                },
                Some(loader::Module::C(_)) => panic!("not implemented"),
                Some(loader::Module::ZZ(ast)) => ast,
            }};

            // find the local we're looking for
            let mut local = None;
            for local2 in &module.locals {
                if local2.name == local_name {
                    local = Some(local2);
                    break;
                }
            };

            // should have been cought by abs
            let local = local.expect(&format!("ICE: module {} does not contain {}", module_name, local_name));


            let mut deps : Vec<Name> = Vec::new();
            match &local.def {
                ast::Def::Static{typeref,expr,..} => {
                    deps.push(typeref.name.clone());
                    deps.extend(expr_deps(expr));
                }
                ast::Def::Const{typeref,expr, ..} => {
                    deps.push(typeref.name.clone());
                    deps.extend(expr_deps(expr));
                }
                ast::Def::Function{ret, args,body, ..} => {
                    if let Some(ret) = ret {
                        deps.push(ret.typeref.name.clone());
                    }
                    for arg in args {
                        deps.push(arg.typeref.name.clone());
                    }
                    deps.extend(block_deps(body));
                }
                ast::Def::Struct{fields,..} => {
                    for field in fields {
                        deps.push(field.typeref.name.clone());
                        if let Some(ref array) = &field.array {
                            if let ast::Value::Name(ref name) = array {
                                deps.push(name.name.clone());
                            }
                        }
                    }
                }
                ast::Def::Macro{body, ..} => {
                    deps.extend(block_deps(body));
                }
            }

            for dep in &deps  {
                incomming.push((dep.clone(), loc.clone()));
            }


            let mut ns = module_name.clone();
            ns.push(local.name.clone());
            let mut ast = local.clone();
            ast.name = ns.to_string();
            collected.0.insert(ns, Local{
                deps,
                ast: Some(ast),
                in_scope_here: loc,
            });
        }


        for (import, short_names) in std::mem::replace(&mut incomming_imports, Vec::new()) {
            assert!(import.name.is_absolute(), "ice: not abs: {}", import.name);
            match all_modules.get(&import.name) {
                None => {
                    debug!("    < none {}", import.name);
                    if import.name.0[1] == "libc" {
                        md.includes.push(ast::Include{
                            loc: import.loc.clone(),
                            expr: format!("<{}.h>", import.name.0[2..].join("/")),
                        });
                        for (local,_) in &import.local {
                            let mut nn = import.name.clone();
                            nn.push(local.clone());
                            flat.short_names.insert(nn);
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
                    flat.sources.insert(path.clone());
                    flat.d.push(D::Include(ast::Include{
                        loc: import.loc.clone(),
                        expr: format!("{:?}", path),
                    }));
                    flat.short_names.insert(import.name.clone());
                    for (local,_) in &import.local {
                        let mut nn = import.name.clone();
                        nn.push(local.clone());
                        flat.short_names.insert(nn);
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
                        if short_names {
                            flat.short_names.insert(import.name.clone());
                        }
                    } else {
                        for (local, import_as) in &import.local {
                            debug!("      < {}", local);
                            let mut nn = import.name.clone();
                            nn.push(local.clone());
                            incomming.push((nn.clone(), import.loc.clone()));
                            if short_names {
                                flat.short_names.insert(nn.clone());
                            }
                            if let Some(import_as) = import_as {
                                flat.aliases.insert(nn, import_as.clone());
                            }
                        }
                    }
                }
            }
        }
    }



    let mut included = HashSet::new();
    for inc in &md.includes {
        if included.insert(inc.expr.clone()) {
            flat.d.push(D::Include(inc.clone()));
        }
    }



    // dependency sort
    let mut sorted = Vec::new();
    let mut sorted_mark = HashSet::new();

    let keys : Vec<Name> = collected.0.keys().cloned().collect();
    while collected.0.len() > 0 {
        for name in &keys {
            if collected.0.contains_key(&name) {
                sort_visit(&mut sorted, &mut sorted_mark, &mut collected.0 , &name);
            }
        }
    }


    debug!("sorted");
    for (name, l) in sorted {
        debug!(" {} ", name);
        for dep in &l.deps {
            debug!("    < {}", dep);
        }
        if let Some(ast) = &l.ast {
            flat.d.push(D::Local(ast.clone()));
        } else {
            assert!(name.0[1] == "libc");
            let inc = ast::Include{
                expr: format!("<{}.h>", name.0[2..name.len()-1].join("/")),
                loc:  l.in_scope_here.clone(),
            };

            if included.insert(inc.expr.clone()) {
                flat.d.push(D::Include(inc));
            }

        }
    }


    flat
}

fn sort_visit(sorted: &mut Vec<(Name,Local)>,
              sorted_mark: &mut HashSet<Name>,
              unsorted: &mut HashMap<Name, Local>,
              name: &Name) {
    if sorted_mark.contains(name) {
        return;
    }
    let n = unsorted.remove(name).expect(&format!("recursive type {} will never complete", name));
    for dep in &n.deps {
        sort_visit(sorted, sorted_mark, unsorted, dep);
    }
    sorted_mark.insert(name.clone());
    sorted.push((name.clone(), n));
}

