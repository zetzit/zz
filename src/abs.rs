/// make all names in a module absolute

use super::ast;
use super::parser::emit_error;
use std::collections::HashMap;
use super::name::Name;
use super::loader;
use std::sync::atomic::{AtomicBool, Ordering};

static ABORT: AtomicBool = AtomicBool::new(false);


struct InScope {
    name:       Name,
    loc:        ast::Location,
    is_module:  bool,
    subtypes:   bool,
}

#[derive(Default)]
struct Scope {
    v: HashMap<String, InScope>,
}

impl Scope{
    pub fn insert(&mut self, local: String, fqn: Name, loc: &ast::Location, is_module: bool, subtypes: bool) {
        if let Some(previous) = self.v.get(&local) {
            if !is_module || !previous.is_module || fqn != previous.name {

                emit_error(format!("conflicting local name '{}'", local), &[
                       (loc.clone(), "declared here"),
                       (previous.loc.clone(), "also declared here"),
                ]);
                std::process::exit(9);
            }
        }
        trace!("  insert {:?}", fqn);
        self.v.insert(local, InScope{
            name:       fqn,
            loc:        loc.clone(),
            is_module,
            subtypes,
        });
    }

    pub fn abs(&self, t: &mut ast::Typed , inbody: bool) {
        if t.name.is_absolute() {
            return;
        }

        match t.name.to_string().as_str() {
            "u8" => {
                t.name = Name(vec![String::new(), "ext".to_string(), "<stdint.h>".to_string(), "uint8_t".to_string()]);
                return;
            },
            "i8" => {
                t.name = Name(vec![String::new(), "ext".to_string(), "<stdint.h>".to_string(), "int8_t".to_string()]);
                return;
            },
            "u16" => {
                t.name = Name(vec![String::new(), "ext".to_string(), "<stdint.h>".to_string(), "uint16_t".to_string()]);
                return;
            },
            "i16" => {
                t.name = Name(vec![String::new(), "ext".to_string(), "<stdint.h>".to_string(), "int16_t".to_string()]);
                return;
            },
            "u32" => {
                t.name = Name(vec![String::new(), "ext".to_string(), "<stdint.h>".to_string(), "uint32_t".to_string()]);
                return;
            },
            "i32" => {
                t.name = Name(vec![String::new(), "ext".to_string(), "<stdint.h>".to_string(), "int32_t".to_string()]);
                return;
            },
            "u64" => {
                t.name = Name(vec![String::new(), "ext".to_string(), "<stdint.h>".to_string(), "uint64_t".to_string()]);
                return;
            },
            "i64" => {
                t.name = Name(vec![String::new(), "ext".to_string(), "<stddef.h>".to_string(), "int64_t".to_string()]);
                return;
            },
            "usize" => {
                t.name = Name(vec![String::new(), "ext".to_string(), "<stddef.h>".to_string(), "size_t".to_string()]);
                return;
            },
            "bool" => {
                t.name = Name(vec![String::new(), "ext".to_string(), "<stdbool.h>".to_string(), "bool".to_string()]);
                return;
            },
            "true" => {
                t.name = Name(vec![String::new(), "ext".to_string(), "<stdbool.h>".to_string(), "true".to_string()]);
                return;
            },
            "false" => {
                t.name = Name(vec![String::new(), "ext".to_string(), "<stdbool.h>".to_string(), "false".to_string()]);
                return;
            },
            "char"
            | "void"
            | "int"
            | "float"
            | "double"
            | "sizeof"
            | "unsigned"
            | "size_t"
            => {
                let nuname = Name(vec![
                    String::new(),
                    "ext".to_string(),
                    "<stddef.h>".to_string(),
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
                        emit_error(format!("possibly undefined name '{}'", lhs), &[
                              (t.loc.clone(), "cannot use :: notation to reference names not tracked by zz"),
                        ]);
                        ABORT.store(true, Ordering::Relaxed);
                    }
                } else {
                    emit_error(format!("undefined name '{}'", lhs), &[
                               (t.loc.clone(), "used in this scope"),
                    ]);
                    ABORT.store(true, Ordering::Relaxed);
                }
            },
            Some(v) => {
                if rhs.len() != 0  && !v.subtypes {
                    emit_error(format!("resolving '{}' as member is not possible", t.name), &[
                        (t.loc.clone(), format!("'{}' is not a module", lhs))
                    ]);
                    ABORT.store(true, Ordering::Relaxed);
                }

                /*
                if rhs.len() != 0 && v.name.0[1] == "ext" {
                    emit_error("'{}' cannot be used as qualified name\n{}\n{}",
                           v.name,
                           (t.loc, format!("'{}' is a c header", lhs)),
                           (v.loc, format!("suggestion: add '{}' to this import", rhs.join("::")))
                           );
                    ABORT.store(true, Ordering::Relaxed);
                }
                */

                if rhs.len() == 0 && v.is_module {
                    emit_error(format!("cannot use module '{}' as a type", v.name), &[
                           (t.loc.clone(), format!("cannot use module '{}' as a type", t.name)),
                           (v.loc.clone(), format!("if you wanted to import '{}' as a type, use ::{{{}}} here", t.name, t.name)),
                    ]);
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

        if let Some("ext") = import.name.0.get(1).map(|s|s.as_str()) {
            debug!("  import ext {} ", import.name);
            return import.name.clone();
        }

    } else {

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

    emit_error(format!("cannot find module '{}'", import.name), &[
        (import.loc.clone(), "imported here"),
    ]);
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

    if module_name.0[1] == "ext" {
        //TODO
        return
    }
    if &module_name == selfname {
        return;
    }

    let module = match all_modules.get(&module_name) {
        None => {
            emit_error(format!("cannot find module '{}' while type checking module '{}'", module_name, selfname), &[
                   (loc.clone(), "expected to be in scope here"),
            ]);
            std::process::exit(9);
        },
        Some(loader::Module::C(_)) => return,
        Some(loader::Module::ZZ(v)) => v,
    };

    for local2 in &module.locals {
        if local2.name == local_name {
            if local2.vis == ast::Visibility::Object {
                emit_error(format!("the type '{}' in '{}' is private", local_name, module_name), &[
                       (loc.clone(), "cannot use private type"),
                       (local2.loc.clone(), "add 'pub' to share this type"),
                ]);
                ABORT.store(true, Ordering::Relaxed);
            }
            if this_vis == &ast::Visibility::Export && local2.vis != ast::Visibility::Export {
                emit_error(format!("the type '{}' in '{}' is not exported", local_name, module_name), &[
                       (loc.clone(), "cannot use an unexported type here"),
                       (local2.loc.clone(), "suggestion: export this type"),
                ]);
                ABORT.store(true, Ordering::Relaxed);
            }
            return;
        }
    };

    emit_error(format!("module '{}' does not contain '{}'", module_name, local_name), &[
        (loc.clone(), "imported here"),
    ]);
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
        ast::Statement::Goto{..} |  ast::Statement::Label{..} | ast::Statement::Break{..} | ast::Statement::Continue{..}=> {
        }
        ast::Statement::Block(b2) => {
            abs_block(b2, &scope, all_modules, self_md_name);
        }
        ast::Statement::Unsafe(b2) => {
            abs_block(b2, &scope, all_modules, self_md_name);
        }
        ast::Statement::For{e1,e2,e3, body} => {
            abs_block(body, &scope, all_modules, self_md_name);
            for s in e1 {
                abs_statement(s, scope, inbody, all_modules, self_md_name);
            }
            for s in e2 {
                abs_statement(s, scope, inbody, all_modules, self_md_name);
            }
            for s in e3 {
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
                if let Some(array) = array {
                    abs_expr(array, &scope, inbody, all_modules, self_md_name);
                }
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
        ast::Statement::Switch {expr, cases, default, ..} => {
            abs_expr(expr, &scope, inbody, all_modules, self_md_name);
            for (expr, block) in cases {
                abs_expr(expr, &scope, inbody, all_modules, self_md_name);
                abs_block(block, &scope, all_modules, self_md_name);
            }
            if let Some(block) = default {
                abs_block(block, &scope, all_modules, self_md_name);
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

        let local_module_name = import.alias.clone().unwrap_or(import.name.0.last().unwrap().clone());

        if import.local.len() == 0 {
            scope.insert(local_module_name, fqn.clone(), &import.loc, true, true);
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
                if md.name.len() > nn.len() || md.name.0[..] != nn.0[..md.name.len()] {
                    // add to scope
                    scope.insert(localname, nn, &import.loc, false, false);
                }
            }
        }
        import.name = fqn;
    }


    let mut new_locals = Vec::new();
    // round one, just get all local defs
    for ast in &mut md.locals {
        let mut ns = md.name.clone();
        ns.0.push(ast.name.clone());
        match &mut ast.def {
            ast::Def::Enum{names,..} => {
                let mut value = 0;
                for (_, val) in names.iter_mut() {
                    if let Some(val) = val {
                        value = *val;
                    } else {
                        *val = Some(value);
                    }
                    value += 1;
                }
                for (name, value) in names {
                    let subname = format!("{}::{}", ast.name, name);

                    new_locals.push(ast::Local{
                        name: subname.clone(),
                        loc:  ast.loc.clone(),
                        vis:  ast.vis.clone(),
                        def: ast::Def::Const {
                            typed: ast::Typed {
                                name:   Name::from(&ast.name),
                                loc:    ast.loc.clone(),
                                ptr:    Vec::new(),
                            },
                            expr: ast::Expression::Literal{
                                loc:    ast.loc.clone(),
                                v:      format!("{}", value.unwrap()),
                            },
                        }
                    });
                    let mut ns = md.name.clone();
                    ns.push(subname.clone());
                    scope.insert(subname, ns, &ast.loc, false, false);
                }
                scope.insert(ast.name.clone(), ns, &ast.loc, false, true);
            }
            _ => {
                scope.insert(ast.name.clone(), ns, &ast.loc, false, false);
            }
        };
    }

    //md.locals.extend(new_locals);

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
            ast::Def::Enum{names,..} => {
            }
            ast::Def::Macro{body, ..} => {
                abs_block(body, &scope,all_modules, &md.name);
            }
        }
    }

    if ABORT.load(Ordering::Relaxed) {
        warn!("exit abs due to previous errors");
        std::process::exit(9);
    }

}
