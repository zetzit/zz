/// make all names in a module absolute

use super::ast;
use super::parser;
use std::collections::HashMap;
use super::name::Name;
use super::loader;
use std::sync::atomic::{AtomicBool, Ordering};

static ABORT: AtomicBool = AtomicBool::new(false);


struct InScope {
    name:       Name,
    loc:        ast::Location,
    is_module:  bool,
}

#[derive(Default)]
struct Scope {
    v: HashMap<String, InScope>,
}

impl Scope{
    pub fn insert(&mut self, local: String, fqn: Name, loc: &ast::Location, is_module: bool) {
        if let Some(previous) = self.v.get(&local) {
            if !is_module || !previous.is_module || fqn != previous.name {
                error!("conflicting local name '{}' \n{}\n{}",
                       local,
                       parser::make_error(&loc, "declared here"),
                       parser::make_error(&previous.loc, "also declared here"),
                       );
                std::process::exit(9);
            }
        }
        self.v.insert(local, InScope{
            name:       fqn,
            loc:        loc.clone(),
            is_module,
        });
    }

    pub fn abs(&self, t: &mut ast::TypeUse) {
        if t.name.is_absolute() {
            return;
        }

        match t.name.to_string().as_str() {
            "char"
            | "void"
            | "int"
            | "float"
            | "double"
            => {
                let nuname = Name(vec![
                    String::new(),
                    "libc".to_string(),
                    "stddef".to_string(),
                    t.name.to_string(),
                ]);
                debug!("  {} => {}", t.name, nuname);
                t.name = nuname;
                return
            }
            _ => (),
        };

        let mut rhs : Vec<String> = t.name.0.clone();
        let lhs = rhs.remove(0);

        match self.v.get(&lhs) {
            None => {
                error!("undefined type '{}' \n{}",
                       t.name,
                       parser::make_error(&t.loc, "used in this scope"),
                       );
                ABORT.store(true, Ordering::Relaxed);
            },
            Some(v) => {
                if rhs.len() != 0  && !v.is_module {
                    error!("resolving '{}' as member is not possible \n{}",
                           t.name,
                           parser::make_error(&t.loc, format!("'{}' is not a module", lhs))
                           );
                    ABORT.store(true, Ordering::Relaxed);
                }

                if rhs.len() != 0 && v.name.0[1] == "libc" {
                    error!("'{}' cannot be used as qualified name\n{}\n{}",
                           v.name,
                           parser::make_error(&t.loc, format!("'{}' is a c header", lhs)),
                           parser::make_error(&v.loc, format!("suggestion: add '{}' to this import", rhs.join("::")))
                           );
                    ABORT.store(true, Ordering::Relaxed);
                }

                if rhs.len() == 0 && v.is_module {
                    error!("cannot use module '{}' as a type\n{}\n{}",
                           v.name,
                           parser::make_error(&t.loc, format!("cannot use module '{}' as a type", t.name)),
                           parser::make_error(&v.loc, format!("if you wanted to import '{}' as a type, use ::{{{}}} here", t.name, t.name)),
                           );
                    ABORT.store(true, Ordering::Relaxed);
                }

                let mut vv = v.name.clone();
                vv.0.extend(rhs);
                debug!("  {} => {}", t.name, vv);
                t.name = vv;
            }
        }
    }
}


fn abs_import(imported_from: &Name, import: &ast::Import, all_modules: &HashMap<Name, loader::Module>) -> Name {
    if import.name.is_absolute() {
        if all_modules.contains_key(&import.name) {
            debug!("  import abs {} => {}", import.name, import.name);
            return import.name.clone();
        }
    } else {
        if let Some("libc") = import.name.0.first().map(|s|s.as_str()) {
            let mut n2 = import.name.clone();
            n2.0.insert(0, String::new());
            debug!("  import libc {} => {}", import.name, n2);
            return n2;
        }

        let mut search = imported_from.clone();
        search.pop();
        search.0.extend(import.name.0.clone());
        if all_modules.contains_key(&search) {
            debug!("  import rel {} => {}", import.name, search);
            return search;
        }


        let mut search = import.name.clone();
        search.0.insert(0, String::new());
        if all_modules.contains_key(&search) {
            debug!("  import aabs {} => {}", import.name, search);
            return search;
        }
    }

    error!("cannot find module '{}' \n{}",
           import.name,
           parser::make_error(&import.loc, "imported here"),
           );
    std::process::exit(9);
}

fn check_abs_available(fqn: &Name, this_vis: &ast::Visibility, all_modules: &HashMap<Name, loader::Module>, loc: &ast::Location, selfname: &Name) {
    if !fqn.is_absolute() {
        ABORT.store(true, Ordering::Relaxed);
        return;
    }

    let mut module_name = fqn.clone();
    let local_name = module_name.pop().unwrap();

    if module_name.0[1] == "libc" {
        //TODO
        return
    }
    if &module_name == selfname {
        return;
    }

    let module = match all_modules.get(&module_name) {
        None => {
            error!("ICE: cannot find module '{}' while checking '{}' \n{}",
                   module_name, selfname,
                   parser::make_error(&loc, "imported here"),
                   );
            std::process::exit(9);
        },
        Some(loader::Module::C(_)) => return,
        Some(loader::Module::ZZ(v)) => v,
    };

    for local2 in &module.locals {
        if local2.name == local_name {
            if local2.vis == ast::Visibility::Object {
                error!("the type '{}' in '{}' is private \n{}\n{}",
                       local_name, module_name,
                       parser::make_error(&loc, "cannot use private type"),
                       parser::make_error(&local2.loc, "add 'pub' to share this type"),
                       );
                ABORT.store(true, Ordering::Relaxed);
            }
            if this_vis == &ast::Visibility::Export && local2.vis != ast::Visibility::Export {
                error!("the type '{}' in '{}' is not exported \n{}\n{}",
                       local_name, module_name,
                       parser::make_error(&loc, "cannot use an unexported type here"),
                       parser::make_error(&local2.loc, "suggestion: export this type"),
                       );
                ABORT.store(true, Ordering::Relaxed);
            }
            return;
        }
    };

    error!("module '{}' does not contain '{}' \n{}",
           module_name, local_name,
           parser::make_error(&loc, "imported here"),
           );
    ABORT.store(true, Ordering::Relaxed);

}


pub fn abs(md: &mut ast::Module, all_modules: &HashMap<Name, loader::Module>) {
    debug!("abs {}", md.name);

    let mut scope = Scope::default();

    for import in &mut md.imports {
        let fqn  = abs_import(&md.name, &import, all_modules);

        if import.local.len() == 0 {
            scope.insert(import.name.0.last().unwrap().clone(), fqn.clone(), &import.loc, true);
        } else {
            for local in &import.local {
                let mut nn = fqn.clone();
                nn.push(local.clone());
                check_abs_available(&nn, &import.vis, all_modules, &import.loc, &md.name);
                scope.insert(local.clone(), nn, &import.loc, false);
            }
        }
        import.name = fqn;
    }

    // round one, just get all local defs
    for ast in &mut md.locals {
        let mut ns = md.name.clone();
        ns.0.push(ast.name.clone());
        scope.insert(ast.name.clone(), ns, &ast.loc, false);
    }

    // round two, make all dependencies absolute
    for ast in &mut md.locals {
        match &mut ast.def {
            ast::Def::Static{typeref,..} => {
                scope.abs(typeref);
                check_abs_available(&typeref.name, &ast.vis, all_modules, &typeref.loc, &md.name);
            }
            ast::Def::Const{typeref,..} => {
                scope.abs(typeref);
                check_abs_available(&typeref.name, &ast.vis, all_modules, &typeref.loc, &md.name);
            }
            ast::Def::Function{ret, args,..} => {
                if let Some(ret) = ret {
                    scope.abs(&mut ret.typeref);
                    check_abs_available(&ret.typeref.name, &ast.vis, all_modules, &ret.typeref.loc, &md.name);
                }
                for arg in args {
                    scope.abs(&mut arg.typeref);
                    check_abs_available(&arg.typeref.name, &ast.vis, all_modules, &arg.typeref.loc, &md.name);
                }
            }
            ast::Def::Struct{fields,..} => {
                for field in fields {
                    scope.abs(&mut field.typeref);
                    check_abs_available(&field.typeref.name, &ast.vis, all_modules, &field.typeref.loc, &md.name);
                }
            }
            ast::Def::Macro{imports,..} => {
                for import in imports {
                    let fqn  = abs_import(&md.name, &import, all_modules);
                    import.name = fqn;
                }
            }
        }
    }

    if ABORT.load(Ordering::Relaxed) {
        std::process::exit(9);
    }

}
