use super::ast;
use super::name::Name;
use super::loader;
use std::collections::HashMap;
use std::collections::HashSet;
use std::path::PathBuf;

pub enum D {
    Include(ast::Include),
    Local(ast::Local),
}

#[derive(Default)]
pub struct Module {
    pub name:       Name,
    pub sources:    HashSet<PathBuf>,
    pub d:          Vec<D>,
}


struct Local {
    deps: Vec<Name>,
    ast:  Option<ast::Local>,
}

#[derive(Default)]
struct Locals (HashMap<Name, Local>);


pub fn flatten(md: &mut ast::Module, all_modules: &HashMap<Name, loader::Module>) -> Module {
    debug!("flatten {}", md.name);

    let mut collected = Locals::default();
    let mut incomming = Vec::new();
    for local in &md.locals {
        let mut ns = md.name.clone();
        ns.push(local.name.clone());
        incomming.push(ns);
    }

    while incomming.len() > 0 {
        for name in std::mem::replace(&mut incomming, Vec::new()) {
            assert!(name.is_absolute());
            let mut module_name = name.clone();
            let local_name = module_name.pop().unwrap();


            if module_name.0[1] == "libc" {
                //TODO
                collected.0.insert(name.clone(), Local{
                    deps: Vec::new(),
                    ast:  None,
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
                ast::Def::Macro{..} => {}
            }

            for dep in &deps  {
                incomming.push(dep.clone());
            }

            let mut ns = module_name.clone();
            ns.push(local.name.clone());
            collected.0.insert(ns, Local{
                deps,
                ast: Some(local.clone()),
            });
        }
    }

    let mut flat    = Module::default();
    flat.name       = md.name.clone();
    flat.sources    = md.sources.clone();

    for inc in &md.includes {
        flat.d.push(D::Include(inc.clone()));
    }


    //TODO: dependency sort
    for (name, l) in &collected.0 {
        debug!("   {} ", name);
        for dep in &l.deps {
            debug!("      < {}", dep);
        }
        if let Some(ast) = &l.ast {
            flat.d.push(D::Local(ast.clone()));
        }
    }


    flat
}

