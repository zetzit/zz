use super::ast;
use super::name::Name;
use super::loader;
use super::parser::emit_error;
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
    pub c_names:        HashMap<Name, ast::Location>,
    pub d:              Vec<D>,
    pub aliases:        HashMap<Name, String>,
    pub deps:           HashSet<Name>,
}

struct Local {
    deps:               Vec<(Name, ast::Location)>,
    ast:                Option<ast::Local>,
    in_scope_here:      ast::Location,
}

#[derive(Default)]
struct Locals (HashMap<Name, Local>);


fn stm_deps(stm: &ast::Statement) -> Vec<(Name, ast::Location)> {
    match stm {
        ast::Statement::Mark{lhs, .. } => {
            expr_deps(lhs)
        },
        ast::Statement::Goto{..} |  ast::Statement::Label{..} => {
            Vec::new()
        },
        ast::Statement::Block(b2) => {
            block_deps(b2)
        },
        ast::Statement::Unsafe(b2) => {
            block_deps(b2)
        },
        ast::Statement::Switch{expr, cases, default, ..} => {
            let mut deps = expr_deps(expr);
            for (_,body) in cases {
                deps.extend(block_deps(body));
            }
            if let Some(default) =  default {
                deps.extend(block_deps(default));
            }
            deps
        },
        ast::Statement::For{e1,e2,e3, body} => {
            let mut deps = Vec::new();
            for s in e1 {
                deps.extend(stm_deps(s));
            }
            for s in e2 {
                deps.extend(stm_deps(s));
            }
            for s in e3 {
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
        ast::Statement::Var{assign, typed, array, ..}  => {
            let mut deps = Vec::new();
            if let Some(array) = &array {
                if let Some(array) = &array {
                    deps.extend(expr_deps(array));
                }
            }
            if let Some(assign) = &assign {
                deps.extend(expr_deps(assign));
            }
            deps.push((typed.name.clone(), typed.loc.clone()));
            deps
        },
        ast::Statement::Expr{expr, ..} => {
            expr_deps(expr)
        }
        ast::Statement::Return {expr, ..} => {
            if let Some(expr) = expr {
                expr_deps(expr)
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
    }
}

fn block_deps(block: &ast::Block) -> Vec<(Name, ast::Location)> {
    let mut deps = Vec::new();
    for stm in &block.statements {
        deps.extend(stm_deps(stm));
    }
    deps
}


fn expr_deps(expr: &ast::Expression) -> Vec<(Name, ast::Location)> {
    match expr {
        ast::Expression::ArrayInit{fields,..}  => {
            let mut v = Vec::new();
            for expr in fields {
                v.extend(expr_deps(expr));
            }
            v
        },
        ast::Expression::StructInit{typed, fields,..}  => {
            let mut v = Vec::new();
            v.push((typed.name.clone(), typed.loc.clone()));
            for (_, expr) in fields {
                v.extend(expr_deps(expr));
            }
            v
        },
        ast::Expression::Name(name)  => {
            if name.name.len() > 2 {
                vec![(name.name.clone(), name.loc.clone())]
            } else {
                Vec::new()
            }
        },
        ast::Expression::Cast{expr, into,..} => {
            let mut v = Vec::new();
            v.push((into.name.clone(), into.loc.clone()));
            v.extend(expr_deps(expr));
            v
        }
        ast::Expression::Literal {..} => {
            Vec::new()
        }
        ast::Expression::Call { name, args, ..} => {
            let mut v = Vec::new();
            if name.name.len() > 2 {
                v.push((name.name.clone(), name.loc.clone()));
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
                    deps: Vec::new(),
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
            let local = local.expect(&format!("ICE: module {} does not contain {}", module_name, local_name));


            let mut deps : Vec<(Name, ast::Location)> = Vec::new();
            match &local.def {
                ast::Def::Enum{names, ..} => {
                    expecting_sub_type = false;
                    //flat.aliases.insert(name.clone(), format!("enum {}", name.0[1..].join("_")));
                    for (subname, _) in names {
                        let mut name = name.clone();
                        name.push(subname.clone());
                        collected.0.insert(name, Local{
                            deps:           Vec::new(),
                            ast:            None,
                            in_scope_here:  loc.clone(),
                        });
                    }
                }
                ast::Def::Static{typed,expr,..} => {
                    deps.push((typed.name.clone(), typed.loc.clone()));
                    deps.extend(expr_deps(expr));
                }
                ast::Def::Const{typed,expr, ..} => {
                    deps.push((typed.name.clone(), typed.loc.clone()));
                    deps.extend(expr_deps(expr));
                }
                ast::Def::Function{ret, args,body, ..} => {
                    if let Some(ret) = ret {
                        deps.push((ret.typed.name.clone(), ret.typed.loc.clone()));
                    }
                    for arg in args {
                        deps.push((arg.typed.name.clone(), arg.typed.loc.clone()));
                    }
                    deps.extend(block_deps(body));
                }
                ast::Def::Struct{fields,..} => {
                    for field in fields {
                        deps.push((field.typed.name.clone(), field.typed.loc.clone()));
                        if let Some(ref expr) = &field.array {
                            deps.extend(expr_deps(expr));
                        }
                    }
                }
                ast::Def::Macro{body, ..} => {
                    deps.extend(block_deps(body));
                }
            }

            if expecting_sub_type {
                panic!("ice: incorrectly resolved '{}' as local '{}' in module '{}' ", name, local_name, module_name)
            }

            for (dep,loc) in &deps  {
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


        for import in std::mem::replace(&mut incomming_imports, Vec::new()) {
            assert!(import.name.is_absolute(), "ice: not abs: {}", import.name);
            match all_modules.get(&import.name) {
                None => {
                    debug!("    < none {}", import.name);
                    if import.name.0[1] == "ext" {
                        //md.includes.push(ast::Include{
                        //    loc: import.loc.clone(),
                        //    expr: format!("<{}.h>", import.name.0[2..].join("/")),
                        //});
                        for (local,_) in &import.local {
                            let mut nn = import.name.clone();
                            nn.push(local.clone());
                            flat.c_names.insert(nn, import.loc.clone());
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



    let mut included = HashSet::new();
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
        for (dep, _) in &l.deps {
            debug!("    < {}", dep);
        }
        if let Some(ast) = &l.ast {
            flat.d.push(D::Local(ast.clone()));
        } else if name.0[1] == "ext" {
            let mut fqn = name.0.clone();
            fqn.pop();
            let inc = ast::Include{
                expr: name.0[2].clone(),
                loc:  l.in_scope_here.clone(),
                fqn:  Name(fqn),
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
    let n = match unsorted.remove(name) {
        Some(v) => v,
        None => {
            eprintln!("recursive type {} will never complete.\nsorted:", name);
            for (name, _) in sorted {
                eprintln!("  {}", name);
            }
            eprintln!("unsorted:");
            for (name, _) in unsorted {
                eprintln!("  {}", name);
            }
            std::process::exit(10);
        },
    };

    for (dep,_) in &n.deps {
        sort_visit(sorted, sorted_mark, unsorted, dep);
    }
    sorted_mark.insert(name.clone());
    sorted.push((name.clone(), n));
}

