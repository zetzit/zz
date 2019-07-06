use std::collections::HashMap;
use super::ast;
use super::parser;
use std::path::PathBuf;
use std::collections::HashSet;


// TODO check import visibility

#[derive(Clone)]
pub enum Dependency {
    Resolved {
        fqn: Vec<String>,
    },
    NeedLocal {
        ast: ast::TypeUse,
    },
}
impl std::fmt::Debug for Dependency {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Dependency::Resolved{fqn,..} => {
                write!(f, "{}", fqn.join("::"))?;
            }
            Dependency::NeedLocal{ast,..} => {
                write!(f, "?{}", ast.name)?;
            }
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct Global {
    ast:  Option<ast::Local>,
    deps: Vec<Dependency>,
}

impl std::fmt::Debug for Global {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", match self.ast {
            None => "none",
            Some(ref local) => {
                match local.def {
                    ast::Def::Static {..} => "static",
                    ast::Def::Const {..} => "const",
                    ast::Def::Function {..} => "function",
                    ast::Def::Struct {..} => "struct",
                    ast::Def::Macro {..} => "macro",
                }
            },
        })?;
        write!(f, " {:?}", self.deps)
    }
}

pub struct Flatten{
    modules:    HashMap<Vec<String>, InterModule>,
    table:      HashMap<Vec<String>, Global>,
}

pub struct Scoped{
    in_scope_here:  ast::Location,
    fqn:            Vec<String>,
}

#[derive(Default)]
pub struct InterModule {
    includes:    HashMap<String, ast::Include>,
    sources:     HashSet<PathBuf>,

    locals:      HashMap<String, ast::Location>,
    imports:     Vec<ast::Import>,
    scope:       HashMap<String, Scoped>,
}


pub struct FlatLocal {
    pub foreign:    bool,
    pub ast:        ast::Local,
}

pub struct FlatModule {
    pub includes:   HashMap<String, ast::Include>,
    pub sources:    HashSet<PathBuf>,

    pub locals:     Vec<FlatLocal>,
}


impl InterModule {
    pub fn emit_local(&mut self, name: &str, source_loc: &ast::Location) {
        if let Some(previous) = self.locals.insert(name.to_string(), source_loc.clone()) {
            if &previous != source_loc {
                error!("conflicting definition of '{}'\n{}\n{}",
                       name,
                       parser::make_error(source_loc, "redefined here"),
                       parser::make_error(&previous, "already defined here"),
                       );
                std::process::exit(9);
            }
        }
    }
}


impl Flatten {
    pub fn new(modules: HashMap<String, ast::Module>) -> Self {
        let mut table  = HashMap::new();
        let mut mm2 : HashMap<Vec<String>, InterModule>    = HashMap::new();

        for (module_name, module) in modules {
            debug!("flatten {}", module_name);

            let mut m2 = InterModule::default();
            m2.sources = module.sources;
            for ast in module.includes {
                m2.includes.insert(ast.expr.clone(), ast);
            }

            for ast in module.locals {
                let mut deps = Vec::new();
                match &ast.def {
                    ast::Def::Static{typeref,..} => {
                        deps.push(Dependency::NeedLocal{
                            ast: typeref.clone(),
                        });
                    }
                    ast::Def::Const{typeref,..} => {
                        deps.push(Dependency::NeedLocal{
                            ast: typeref.clone(),
                        });
                    }
                    ast::Def::Function{ret, args,..} => {
                        if let Some(ret) = ret {
                            deps.push(Dependency::NeedLocal{
                                ast: ret.typeref.clone(),
                            });
                        }
                        for arg in args {
                            deps.push(Dependency::NeedLocal{
                                ast: arg.typeref.clone(),
                            });
                        }
                    }
                    ast::Def::Struct{fields,..} => {
                        for field in fields {
                            deps.push(Dependency::NeedLocal{
                                ast: field.typeref.clone(),
                            });
                        }
                    }
                    ast::Def::Macro{imports,..} => {
                        for dep in imports {
                            deps.push(Dependency::Resolved{
                                fqn: dep.namespace.clone(),
                            });
                        }
                    }
                    _ => {},
                }

                let mut ns : Vec<String> = module_name.split("::").map(|s|s.to_string()).collect();
                ns.push(ast.name.clone());
                table.insert(ns.clone(), Global{
                    ast: Some(ast.clone()),
                    deps,
                });

                m2.emit_local(&ast.name, &ast.loc);
            }


            for ast in module.imports {
                m2.imports.push(ast);
            }

            mm2.insert(module_name.split("::").map(|s|s.to_string()).collect(), m2);
        }


        debug!("global type lookup: {:#?}", table.iter().map(|(k,v)|
            format!("{} => {:?}", k.join("::"), v)).collect::<Vec<String>>());



        // partially resolve local scope
        for (module_name, module) in &mut mm2 {
            debug!("type resolve {:?}", module_name);

            // ireference the global symbol for each local
            for (local_name, source_loc) in &module.locals {
                let mut fqn = module_name.clone();
                fqn.push(local_name.to_string());
                table.get(&fqn).expect(&format!("ice: {:?} not in global" , fqn));
                module.scope.insert(local_name.clone(), Scoped{
                    fqn,
                    in_scope_here: source_loc.clone(),
                });
            }

            debug!("presolved locals: {:#?}", module.scope.iter().map(|(k,v)|
                format!("{} => {:?}", k, v.fqn)).collect::<Vec<String>>());

            // for each global in scope, find all dependencies, but only locally
            for (local_name,_) in &module.locals {
                let fqn = module.scope.get(local_name).unwrap().fqn.clone();
                let mut g  =  table.remove(&fqn).unwrap();

                for dep in &mut g.deps {
                    match dep {
                        Dependency::Resolved{..} => {

                        },
                        Dependency::NeedLocal{ast} => {
                            match ast.name.as_str() {
                                "char"
                                    | "int"
                                    | "float"
                                    | "double"
                                    => {
                                        *dep = Dependency::Resolved{fqn: vec![
                                            "libc".into(), ast.name.as_str().to_string()]
                                        };
                                        continue;
                                    }
                                _ => (),
                            }

                            if &ast.name == local_name {
                                error!("self referencing type '{}'\n{}",
                                       ast.name,
                                       parser::make_error(&ast.loc, "first used in this scope"),
                                       );
                                std::process::exit(9);
                            }

                            let mut fqn2 = module_name.clone();
                            fqn2.push(ast.name.clone());
                            if let Some(_) = table.get(&fqn2) {
                                *dep = Dependency::Resolved{fqn:fqn2};
                            };
                        }
                    }
                }

                table.insert(fqn, g);
            }

            debug!("resolved locals: {:#?}", module.scope.iter().map(|(k,v)|
                format!("{} => {:?}", k, v.fqn)).collect::<Vec<String>>());
        }

        debug!("locally resolved global table: {:#?}", table.iter().map(|(k,v)|
            format!("{} => {:?}", k.join("::"), v)).collect::<Vec<String>>());



        //imports
        let mut any_import_completed : bool;
        loop {
            any_import_completed = false;
            for module_name in mm2.keys().cloned().collect::<Vec<Vec<String>>>().into_iter() {
                debug!("imports {:?}", module_name);
                let mut module = mm2.remove(&module_name).unwrap();
                for import in std::mem::replace(&mut module.imports, Vec::new()) {

                    if import.namespace.len() < 2 {
                        panic!("ice: import {:?} is not a valid name" , import.namespace);
                    }

                    if import.namespace.get(0) == Some(&String::from("libc")) {
                        module.scope.insert(import.namespace.last().unwrap().clone(), Scoped{
                            fqn: import.namespace.clone(),
                            in_scope_here: import.loc.clone(),
                        });
                        continue;
                    }

                    let mut ns = import.namespace.clone();
                    let name = ns.pop().unwrap();

                    if ns == module_name {
                        error!("circular import '{}'\n{}",
                               name,
                               parser::make_error(&import.loc, format!("cannot import '{}' into context '{}'",
                                                  import.namespace.join("::"), module_name.join("::"))),
                               );
                        std::process::exit(9);
                    }

                    let imported_module = mm2.get(&ns).expect(&format!("ice: module {:?} not resolved" , ns));


                    if &name == "*" {
                        error!("import * doesnt work yet \n{}",
                               parser::make_error(&import.loc, "in this import"),
                               );
                        std::process::exit(9);
                    } else {
                        let foreign_fqn = match imported_module.scope.get(&name) {
                            None => {
                                error!("'{}' not defined in module '{}' \n{}",
                                       name, ns.join("::"),
                                       parser::make_error(&import.loc, "in this import"),
                                       );
                                std::process::exit(9);
                            },
                            Some(v) => v,
                        }.fqn.clone();

                        let scoped = Scoped{
                            fqn:            foreign_fqn.clone(),
                            in_scope_here:  import.loc.clone(),
                        };

                        if let Some(previous) = module.scope.insert(name.clone(), scoped) {
                            if previous.fqn != foreign_fqn {
                                if let Some(local) = module.locals.get(&name) {
                                    error!("conflicting declaration of '{}' in scope '{}'\n{}\n{}",
                                           name, module_name.join("::"),
                                           parser::make_error(&import.loc, "if we would import here"),
                                           parser::make_error(local, "already declared here"),
                                           );
                                } else {
                                    error!("conflicting declaration of '{}' in scope '{}' \n{}",
                                           name, module_name.join("::"),
                                           parser::make_error(&import.loc, "if we would import here"),
                                           );
                                }
                                std::process::exit(9);
                            }
                        }

                        let g = table.get(&foreign_fqn).expect(&format!("ice: {:?} not in global" , foreign_fqn));
                        let mut all_resolved = true;
                        let mut dep_imports = Vec::new();
                        for dep in &g.deps {
                            match dep {
                                Dependency::NeedLocal{..} => {
                                    debug!("cannot complete import of {:?} yet because of missing dep {:?}", foreign_fqn, dep);
                                    all_resolved = false;
                                },
                                Dependency::Resolved{fqn, ..}  => {
                                    if fqn == &foreign_fqn {
                                        panic!("ICE: {:?} has itself as dependency", fqn);
                                    }
                                    let mut im2 = import.clone();
                                    im2.namespace = fqn.clone();
                                    dep_imports.push(im2);
                                    debug!("dependency import {:?} => {:?}", foreign_fqn, fqn);
                                },
                            }
                        }
                        if all_resolved {
                            any_import_completed = true;
                            module.imports.extend(dep_imports);
                            debug!("completed import of {:?} into {}", foreign_fqn, module_name.join("::"));
                        } else {
                            module.imports.push(import);
                        }
                    }
                }
                debug!("post import locals: {:#?}", module.scope.iter().map(|(k,v)|
                    format!("{} => {:?}", k, v.fqn)).collect::<Vec<String>>());
                mm2.insert(module_name, module);
            }

            if !any_import_completed{
                break;
            }




            // local resolving round post imports
            for (module_name, module) in &mut mm2 {
                for (local, scoped) in &module.scope {
                    if scoped.fqn.get(0) == Some(&String::from("libc")) {
                        continue
                    }
                    let mut g = table.remove(&scoped.fqn).expect(&format!("ice: {:?} not in global", scoped.fqn));
                    for dep in &mut g.deps {
                        if let Dependency::NeedLocal{ast} = dep {
                            if let Some(scoped2) = module.scope.get(&ast.name) {
                                *dep = Dependency::Resolved{fqn: scoped2.fqn.clone()};
                            }
                        }
                    }
                    table.insert(scoped.fqn.clone(), g);
                }
            }

        }



        // last round of local resolving
        for (module_name, module) in &mut mm2 {
            debug!("type check {:?}", module_name);
            debug!("presolved locals: {:#?}", module.scope.iter().map(|(k,v)|
                format!("{} => {:?}", k, v.fqn)).collect::<Vec<String>>());

            for (local, scoped) in &module.scope {

                if scoped.fqn.get(0) == Some(&String::from("libc")) {
                    continue
                }

                let mut g = table.remove(&scoped.fqn).expect(&format!("ice: {:?} not in global", scoped.fqn));
                for dep in &mut g.deps {
                    match dep {
                        Dependency::Resolved{..} => {

                        },
                        Dependency::NeedLocal{ast} => {
                            if let Some(fqn) = module.scope.get(&ast.name) {
                                *dep = Dependency::Resolved{fqn: scoped.fqn.clone()};
                            } else {
                                error!("undefined type '{}' in the context '{}'\n{}",
                                       ast.name,
                                       module_name.join("::"),
                                       parser::make_error(&ast.loc, "first used in this scope"),
                                       );
                                std::process::exit(9);
                            }
                        }
                    }
                }
                table.insert(scoped.fqn.clone(), g);
            }

            debug!("resolved locals: {:#?}", module.scope.iter().map(|(k,v)|
                format!("{} => {:?}", k, v.fqn)).collect::<Vec<String>>());
        }


        debug!("final resolved global table: {:#?}", table.iter().map(|(k,v)|
            format!("{} => {:?}", k.join("::"), v)).collect::<Vec<String>>());


        Self {
            modules: mm2,
            table,
        }
    }

    pub fn run(mut self) -> HashMap<Vec<String>, FlatModule> {
        std::mem::replace(&mut self.modules, HashMap::new()).into_iter().map(|(name, module)|{

            let mut includes =  module.includes;
            let mut flatlocals = Vec::new();

            for (local, scoped) in &module.scope {
                if scoped.fqn.get(0) == Some(&String::from("libc")) {
                    if scoped.fqn.len() == 3 {
                        includes.insert(scoped.fqn[1].clone(), ast::Include{
                            expr: format!("<{}.h>", scoped.fqn[1]),
                            loc:  scoped.in_scope_here.clone(),
                        });
                    }
                    continue
                }
                let g = self.table.get(&scoped.fqn).expect(&format!("ice: {:?} not in global", scoped.fqn));
                match &g.ast {
                    None => {
                        panic!("ice: no ast while flattening '{:?}' into module '{:?}'", scoped.fqn, name);
                    }
                    Some(ast) => {
                        let local_type = scoped.fqn[0] == name[0] && scoped.fqn[1] == name[1];
                        flatlocals.push(FlatLocal{
                            foreign: !local_type,
                            ast: ast.clone(),
                        });
                    }
                }
            }

            (name, FlatModule{
                locals: flatlocals, 
                includes,
                sources:  module.sources,
            })
        }).collect()
    }
}

