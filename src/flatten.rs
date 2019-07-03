use std::collections::HashMap;
use super::ast;
use super::parser;
use super::parser::Rule;


#[derive(Clone)]
pub enum Dependency {
    Resolved {
        name: Vec<String>,
    },
    NeedLocal {
        ast: ast::TypeUse,
    },
}
impl std::fmt::Debug for Dependency {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Dependency::Resolved{name,..} => {
                write!(f, "{}", name.join("::"))?;
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
        write!(f, "=> {:?}", self.deps)
    }
}

pub struct Flatten{
    modules:    HashMap<Vec<String>, Module>,
    table:      HashMap<Vec<String>, Global>,
}

#[derive(Default)]
pub struct Module {
    pub includes:    HashMap<String, ast::Include>,


    locals:          HashMap<String, ast::Location>,
    imports:         Vec<ast::Import>,
    scope:           HashMap<String, Vec<String>>,
}


impl Module {
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
        let mut mm2 : HashMap<Vec<String>, Module>    = HashMap::new();

        for (module_name, module) in modules {
            debug!("flatten {}", module_name);

            let mut m2 = Module::default();
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
                                name: dep.namespace.clone(),
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
                /*
                if ast.namespace.first().map(|s|s.as_str()) == Some("libc") {
                    let mut ns = ast.namespace.clone();
                    ns.remove(0);
                    let typ = ns.pop().unwrap();

                    if typ == "*" {
                        error!("cannot import * from a c header\n{}",
                               parser::make_error(&ast.loc, "in this import"),
                               );
                        std::process::exit(9);
                    }


                    let mut ns2 = module.namespace.clone();
                    ns2.push(typ.clone());
                    table.insert(ns2, Global{
                        ast: Some(ast.clone()),
                        deps: Vec::new(),
                    });

                    m2.emit_local(&typ, &ast.loc);
                    let ast = ast::Include{
                        expr: format!("<{}.h>", ns.join("/")),
                        loc: ast.loc.clone(),
                    };
                    m2.includes.insert(typ, ast);
                } else {
                    m2.imports.push(ast);
                }
                */
                m2.imports.push(ast);
            }

            mm2.insert(module_name.split("::").map(|s|s.to_string()).collect(), m2);
        }


        debug!("global type lookup: {:#?}", table.iter().map(|(k,v)|
            format!("{} => {:?}", k.join("::"), v)).collect::<Vec<String>>());



        // partially resolve local scope (without imports)
        for (module_name, module) in &mut mm2 {
            debug!("type resolve {:?}", module_name);

            // ireference the global symbol for each local
            for (local_name, source_loc) in &module.locals {
                let mut fqn = module_name.clone();
                fqn.push(local_name.to_string());
                table.get(&fqn).expect(&format!("ice: {:?} not in global" , fqn));
                module.scope.insert(local_name.clone(), fqn);
            }

            debug!("presolved locals: {:#?}", module.scope.iter().map(|(k,v)|
                format!("{} => {:?}", k, v)).collect::<Vec<String>>());

            // for each global in scope, find all dependencies, but only locally
            for (local_name,_) in &module.locals {
                let mut fqn = module.scope.get(local_name).unwrap().clone();
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
                                        *dep = Dependency::Resolved{name: vec![
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

                            let mut fqn = module_name.clone();
                            fqn.push(ast.name.clone());
                            if let Some(_) = table.get(&fqn) {
                                *dep = Dependency::Resolved{name: fqn};
                            };
                        }
                    }
                }

                table.insert(fqn, g);
            }

            debug!("resolved locals: {:#?}", module.scope.iter().map(|(k,v)|
                format!("{} => {:?}", k, v)).collect::<Vec<String>>());
        }

        debug!("locally resolved global table: {:#?}", table.iter().map(|(k,v)|
            format!("{} => {:?}", k.join("::"), v)).collect::<Vec<String>>());



        //imports
        for module_name in mm2.keys().cloned().collect::<Vec<Vec<String>>>().into_iter() {
            debug!("imports {:?}", module_name);
            let mut module = mm2.remove(&module_name).unwrap();
            for import in std::mem::replace(&mut module.imports, Vec::new()) {

                if import.namespace.get(0) == Some(&String::from("libc")) {
                    module.scope.insert(import.namespace.last().unwrap().clone(), import.namespace.clone());
                    continue;
                }

                let mut ns = import.namespace.clone();
                let name = ns.pop().unwrap();

                if ns == module_name {
                    error!("self referencing import '{}'\n{}",
                           name,
                           parser::make_error(&import.loc, "cannot import module of same name"),
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
                    let mut foreign_fqn = match imported_module.scope.get(&name) {
                        None => {
                            error!("'{}' not defined in module '{}' \n{}",
                                   name, ns.join("::"),
                                   parser::make_error(&import.loc, "in this import"),
                                   );
                            std::process::exit(9);
                        },
                        Some(v) => v,
                    }.clone();


                    if let Some(previous_fqn) = module.scope.insert(name.clone(), foreign_fqn.clone()) {
                        if previous_fqn != foreign_fqn {
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


                    TODO, so what about the dependencies of what we just imported....
                }
            }
            debug!("post import locals: {:#?}", module.scope.iter().map(|(k,v)|
                format!("{} => {:?}", k, v)).collect::<Vec<String>>());
            mm2.insert(module_name, module);
        }



        // last round of local resolving
        for (module_name, module) in &mut mm2 {
            debug!("type check {:?}", module_name);
            debug!("presolved locals: {:#?}", module.scope.iter().map(|(k,v)|
                format!("{} => {:?}", k, v)).collect::<Vec<String>>());

            for (local, fqn) in &module.scope {

                if fqn.get(0) == Some(&String::from("libc")) {
                    continue
                }

                let mut g = table.remove(fqn).expect(&format!("ice: {:?} not in global", fqn));
                for dep in &mut g.deps {
                    match dep {
                        Dependency::Resolved{..} => {

                        },
                        Dependency::NeedLocal{ast} => {
                            if let Some(_) = module.scope.get(&ast.name) {
                                *dep = Dependency::Resolved{name: vec![ast.name.as_str().to_string()]};
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
                table.insert(fqn.clone(), g);
            }

            debug!("resolved locals: {:#?}", module.scope.iter().map(|(k,v)|
                format!("{} => {:?}", k, v)).collect::<Vec<String>>());
        }


        debug!("final resolved global table: {:#?}", table.iter().map(|(k,v)|
            format!("{} => {:?}", k.join("::"), v)).collect::<Vec<String>>());


        Self {
            modules: mm2,
            table,
        }
    }

    pub fn run(self) -> HashMap<Vec<String>, Module> {
        self.modules
    }
}

