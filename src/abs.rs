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

    pub fn abs(&self, t: &mut ast::Typed , inbody: bool) {
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
                if inbody {
                    if t.name.len() > 1 {
                        error!("possibly undefined name '{}' \n{}",
                              t.name,
                              parser::make_error(&t.loc, "cannot use :: notation to reference names not tracked by zz"),
                              );
                        ABORT.store(true, Ordering::Relaxed);
                    }
                } else {
                    error!("undefined name '{}' \n{}",
                           t.name,
                           parser::make_error(&t.loc, "used in this scope"),
                           );
                    ABORT.store(true, Ordering::Relaxed);
                }
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

        // self
        if &import.name == imported_from  {
            debug!("  import self abs {}", import.name);
            return import.name.clone();
        }
    } else {
        if let Some("libc") = import.name.0.first().map(|s|s.as_str()) {
            let mut n2 = import.name.clone();
            n2.0.insert(0, String::new());
            debug!("  import libc {} => {}", import.name, n2);
            return n2;
        }

        // root/current_module/../search
        let mut search = imported_from.clone();
        search.pop();
        search.0.extend(import.name.0.clone());
        if all_modules.contains_key(&search) {
            debug!("  import rel {} => {}", import.name, search);
            return search;
        }

        // /search
        let mut search = import.name.clone();
        search.0.insert(0, String::new());
        if all_modules.contains_key(&search) {
            debug!("  import aabs {} => {}", import.name, search);
            return search;
        }

        // /root/current/search
        let mut search = imported_from.clone();
        search.0.extend(import.name.0.clone());
        if all_modules.contains_key(&search) {
            debug!("  import aabs/lib {} => {}", import.name, search);
            return search;
        }

        // self
        let mut search = import.name.clone();
        search.0.insert(0, String::new());
        if &search == imported_from  {
            debug!("  import self abs {} => {}", import.name, search);
            return search;
        }

        // self literal
        if let Some("self") = import.name.0.get(0).map(|s|s.as_ref()) {
            let mut search2 = import.name.clone();
            search2.0.remove(0);

            let mut search = imported_from.clone();
            search.0.extend(search2.0);

            debug!("  import self {} => {}", import.name, search);
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
    if !fqn.is_absolute() && fqn.len() > 1 {
        ABORT.store(true, Ordering::Relaxed);
        return;
    }


    let mut module_name = fqn.clone();
    let local_name = module_name.pop().unwrap();

    if module_name.len() < 2 {
        return;
    }

    if module_name.0[1] == "libc" {
        //TODO
        return
    }
    if &module_name == selfname {
        return;
    }

    let module = match all_modules.get(&module_name) {
        None => {
            error!("cannot find module '{}' while type checking module '{}' \n{}",
                   module_name, selfname,
                   parser::make_error(&loc, "expected to be in scope here"),
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


fn abs_expr(
    expr: &mut ast::Expression,
    scope: &Scope,
    inbody: bool,
    all_modules: &HashMap<Name, loader::Module>,
    self_md_name: &Name,
    )
{
    match expr {
        ast::Expression::ArrayInit{fields,..} => {
            for expr in fields {
                abs_expr(expr, scope, inbody, all_modules, self_md_name);
            }
        },
        ast::Expression::StructInit{typed, fields,..} => {
            scope.abs(typed, inbody);
            for (_, expr) in fields {
                abs_expr(expr, scope, inbody, all_modules, self_md_name);
            }
        },
        ast::Expression::UnaryPre{expr,..} => {
            abs_expr(expr, scope, inbody, all_modules, self_md_name);
        },
        ast::Expression::UnaryPost{expr,..} => {
            abs_expr(expr, scope, inbody, all_modules, self_md_name);
        },
        ast::Expression::Cast{expr, into,..} => {
            abs_expr(expr, scope, inbody, all_modules, self_md_name);
            scope.abs(into, inbody);
        }
        ast::Expression::MemberAccess{lhs,..}  => {
            abs_expr(lhs, scope, inbody, all_modules, self_md_name);
        }
        ast::Expression::ArrayAccess{lhs,rhs,..}  => {
            abs_expr(lhs, scope, inbody, all_modules, self_md_name);
            abs_expr(rhs, scope, inbody, all_modules, self_md_name);
        }
        ast::Expression::Name(name)  => {
            scope.abs(name, inbody);
        },
        ast::Expression::Literal {..} => {
        }
        ast::Expression::Call { ref mut name, args, ..} => {
            scope.abs(name, inbody);
            check_abs_available(&name.name, &ast::Visibility::Object, all_modules, &name.loc, self_md_name);
            for arg in args {
                abs_expr(arg, scope, inbody, all_modules, self_md_name);
            }
        },
        ast::Expression::InfixOperation {lhs, rhs,.. } => {
            abs_expr(lhs, scope, inbody, all_modules, self_md_name);
            for (_, rhs) in rhs {
                abs_expr(rhs, scope, inbody, all_modules, self_md_name);
            }
        }
    }
}

fn abs_statement(
    stm: &mut ast::Statement,
    scope: &Scope,
    inbody: bool,
    all_modules: &HashMap<Name, loader::Module>,
    self_md_name: &Name,
    )
{
    match stm {
        ast::Statement::Mark{lhs,..} => {
            abs_expr(lhs, &scope, inbody, all_modules, self_md_name);
        },
        ast::Statement::Goto{..} |  ast::Statement::Label{..} => {
        }
        ast::Statement::Block(b2) => {
            abs_block(b2, &scope, all_modules, self_md_name);
        }
        ast::Statement::For{e1,e2,e3, body} => {
            abs_block(body, &scope, all_modules, self_md_name);
            if let Some(s) = e1 {
                abs_statement(s, scope, inbody, all_modules, self_md_name);
            }
            if let Some(s) = e2 {
                abs_statement(s, scope, inbody, all_modules, self_md_name);
            }
            if let Some(s) = e3 {
                abs_statement(s, scope, inbody, all_modules, self_md_name);
            }
        },
        ast::Statement::Cond{expr, body, ..} => {
            for expr in expr {
                abs_expr(expr, &scope, inbody, all_modules, self_md_name);
            }
            abs_block(body, &scope, all_modules, self_md_name);
        },
        ast::Statement::Assign{lhs, rhs, ..}  => {
            abs_expr(lhs, &scope, inbody, all_modules, self_md_name);
            abs_expr(rhs, &scope, inbody, all_modules, self_md_name);
        },
        ast::Statement::Var{assign, typed, array, ..}  => {
            if let Some(assign) = assign {
                abs_expr(assign, &scope, inbody, all_modules, self_md_name);
            }
            if let Some(array) = array {
                abs_expr(array, &scope, inbody, all_modules, self_md_name);
            }
            scope.abs(typed, false);
            //check_abs_available(&typed.name, &ast.vis, all_modules, &typed.loc, &md.name);
        },
        ast::Statement::Expr{expr, ..} => {
            abs_expr(expr, &scope, inbody, all_modules, self_md_name);
        }
        ast::Statement::Return {expr, ..} => {
            if let Some(expr) = expr {
                abs_expr(expr, &scope, inbody, all_modules, self_md_name);
            }
        }
    }
}

fn abs_block(
    block:   &mut ast::Block,
    scope: &Scope,
    all_modules: &HashMap<Name, loader::Module>,
    self_md_name: &Name,
    )
{
    for stm in &mut block.statements {
        abs_statement(stm, scope, true, all_modules, self_md_name);
    }
}

pub fn abs(md: &mut ast::Module, all_modules: &HashMap<Name, loader::Module>) {
    debug!("abs {}", md.name);

    let mut scope = Scope::default();

    for import in &mut md.imports {
        let fqn  = abs_import(&md.name, &import, all_modules);

        if import.local.len() == 0 {
            scope.insert(import.name.0.last().unwrap().clone(), fqn.clone(), &import.loc, true);
        } else {
            for (local, import_as) in &import.local {
                let mut nn = fqn.clone();
                nn.push(local.clone());
                check_abs_available(&nn, &import.vis, all_modules, &import.loc, &md.name);

                let localname = if let Some(n) = import_as {
                    n.clone()
                } else {
                    local.clone()
                };

                // if not self
                if md.name.0[..] != nn.0[..md.name.len()] {
                    // add to scope
                    scope.insert(localname, nn, &import.loc, false);
                }
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
            ast::Def::Static{typed,expr,..} => {
                abs_expr(expr, &scope, false, all_modules, &md.name);
                scope.abs(typed, false);
                check_abs_available(&typed.name, &ast.vis, all_modules, &typed.loc, &md.name);
            }
            ast::Def::Const{typed, expr,..} => {
                abs_expr(expr, &scope, false,all_modules, &md.name);
                scope.abs(typed, false);
                check_abs_available(&typed.name, &ast.vis, all_modules, &typed.loc, &md.name);
            }
            ast::Def::Function{ret, args, ref mut body, ..} => {
                if let Some(ret) = ret {
                    scope.abs(&mut ret.typed, false);
                    check_abs_available(&ret.typed.name, &ast.vis, all_modules, &ret.typed.loc, &md.name);
                }
                for arg in args {
                    scope.abs(&mut arg.typed, false);
                    check_abs_available(&arg.typed.name, &ast.vis, all_modules, &arg.typed.loc, &md.name);
                }
                abs_block(body, &scope,all_modules, &md.name);
            }
            ast::Def::Struct{fields,..} => {
                for field in fields {
                    scope.abs(&mut field.typed, false);
                    check_abs_available(&field.typed.name, &ast.vis, all_modules, &field.typed.loc, &md.name);
                    if let Some(ref mut array) = &mut field.array {
                        abs_expr(array, &scope, false, all_modules, &md.name);
                    }
                }
            }
            ast::Def::Macro{body, ..} => {
                abs_block(body, &scope,all_modules, &md.name);
            }
        }
    }

    if ABORT.load(Ordering::Relaxed) {
        error!("exit abs due to previous errors");
        std::process::exit(9);
    }

}
