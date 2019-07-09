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
    pub deps:           HashSet<Name>,
}


struct Local {
    deps:               Vec<Name>,
    ast:                Option<ast::Local>,
    in_scope_here:      ast::Location,
}

#[derive(Default)]
struct Locals (HashMap<Name, Local>);


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
                ast::Def::Static{typeref,..} => {
                    deps.push(typeref.name.clone());
                }
                ast::Def::Const{typeref,..} => {
                    deps.push(typeref.name.clone());
                }
                ast::Def::Function{ret, args,..} => {
                    if let Some(ret) = ret {
                        deps.push(ret.typeref.name.clone());
                    }
                    for arg in args {
                        deps.push(arg.typeref.name.clone());
                    }
                }
                ast::Def::Struct{fields,..} => {
                    for field in fields {
                        deps.push(field.typeref.name.clone());
                    }
                }
                ast::Def::Macro{imports,..} => {
                    for import in imports {
                        incomming_imports.push((import.clone(), false));
                    }
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
                    if import.name.0[1] == "libc" {
                        md.includes.push(ast::Include{
                            loc: import.loc.clone(),
                            expr: format!("<{}.h>", import.name.0[2]),
                        });
                        for local in &import.local {
                            let mut nn = import.name.clone();
                            nn.push(local.clone());
                            flat.short_names.insert(nn);
                        }
                    } else {
                        panic!("ice: unknown module {}", import.name);
                    }
                },
                Some(loader::Module::C(path)) => {
                    flat.sources.insert(path.clone());
                    flat.d.push(D::Include(ast::Include{
                        loc: import.loc.clone(),
                        expr: format!("{:?}", path),
                    }));
                    flat.short_names.insert(import.name.clone());
                    for local in &import.local {
                        let mut nn = import.name.clone();
                        nn.push(local.clone());
                        flat.short_names.insert(nn);
                    }
                }
                Some(loader::Module::ZZ(ast)) => {
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
                        for local in &import.local {
                            let mut nn = import.name.clone();
                            nn.push(local.clone());
                            incomming.push((nn.clone(), import.loc.clone()));
                            if short_names {
                                flat.short_names.insert(nn);
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


    for (name, l) in sorted {
        debug!("   {} ", name);
        for dep in &l.deps {
            debug!("      < {}", dep);
        }
        if let Some(ast) = &l.ast {
            flat.d.push(D::Local(ast.clone()));
        } else {
            assert!(name.0[1] == "libc");
            let inc = ast::Include{
                expr: format!("<{}.h>", name.0[2]),
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

