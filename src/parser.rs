use pest::Parser;
use super::ast::*;
use std::path::Path;
use std::io::{Read};

#[derive(Parser)]
#[grammar = "zz.pest"]
pub struct ZZParser;

pub fn parse<'a>(ns: Vec<String>, n: &Path) -> Module<'a>
{
    match p(ns, &n){
        Err(e) => {
            error!("{:?} : {}", n, e);
            std::process::exit(9);
        }
        Ok(md) => {
            md
        }
    }
}

fn p<'a>(nsi: Vec<String>, n: &Path) -> Result<Module<'a>, pest::error::Error<Rule>> {

    let mut module = Module::default();
    module.source = n.to_path_buf();
    module.sources.insert(n.canonicalize().unwrap());
    module.namespace = nsi;
    module.namespace.push(n.file_stem().expect(&format!("stem {:?}", n)).to_string_lossy().into());

    let mut f = std::fs::File::open(n).expect(&format!("cannot open file {:?}", n));
    let mut file = String::new();
    f.read_to_string(&mut file).expect(&format!("read {:?}", n));
    let mut file = ZZParser::parse(Rule::file, Box::leak(Box::new(file)))?;


    for decl in file.next().unwrap().into_inner() {
        match decl.as_rule() {
            Rule::imacro => {
                let decl = decl.into_inner();
                let mut loc  = None;
                let mut name = None;
                let mut args = Vec::new();
                let mut imports = Vec::new();
                let mut body = None;
                let mut vis = Visibility::Shared;
                for part in decl {
                    match part.as_rule() {
                        Rule::key_private => {
                            vis = Visibility::Object;
                        }
                        Rule::key_pub => {
                            vis = Visibility::Export;
                        }
                        Rule::ident if name.is_none() => {
                            name = part.as_str().into();
                        }
                        Rule::macroimports => {
                            for arg in part.into_inner() {
                                let loc = Location{
                                    line: arg.as_span().start_pos().line_col().0,
                                    file: n.to_string_lossy().into(),
                                    span: arg.as_span(),
                                };
                                let namespace : Vec<String> = arg.as_str().split("::").map(|s|s.to_string()).collect();
                                let import = Import{
                                    loc,
                                    namespace,
                                    vis: Visibility::Object,
                                };
                                module.imports.push(import.clone());
                                imports.push(import);
                            }
                        }
                        Rule::call_args => {
                            for arg in part.into_inner() {
                                args.push(arg.as_str().into());
                            }
                        }
                        Rule::block if body.is_none() => {
                            loc = Some(Location{
                                line: part.as_span().start_pos().line_col().0,
                                file: n.to_string_lossy().into(),
                                span: part.as_span(),
                            });
                            body = Some(part.as_str().to_string());
                        },
                        e => panic!("unexpected rule {:?} in macro ", e),
                    }
                }

                module.macros.insert(name.unwrap().to_string(), Macro{
                    name: name.unwrap().to_string(),
                    args,
                    body: body.unwrap(),
                    imports,
                    vis,
                    loc: loc.unwrap(),
                });

            }
            Rule::function => {
                let mut loc  = None;
                let decl = decl.into_inner();
                let mut name = String::new();
                let mut args = Vec::new();
                let mut ret  = None;
                let mut body = String::new();
                let mut vis = Visibility::Shared;

                for part in decl {
                    match part.as_rule() {
                        Rule::key_private => {
                            vis = Visibility::Object;
                        }
                        Rule::key_pub => {
                            vis = Visibility::Export;
                        }
                        Rule::ident => {
                            name = part.as_str().into();
                        }
                        Rule::ret_arg => {
                            ret = Some(AnonArg{
                                typ: part.into_inner().as_str().to_string()
                            });
                        },
                        Rule::fn_args => {
                            for arg in part.into_inner() {
                                let mut arg       = arg.into_inner();
                                let types         = arg.next().unwrap();
                                let name          = arg.next().unwrap().as_str().to_string();
                                let mut muta      = false;
                                let mut ptr       = false;
                                let mut typ       = String::new();
                                let mut namespace = None;

                                for part in types.into_inner() {
                                    match part.as_rule() {
                                        Rule::namespace => {
                                            namespace = Some(part.as_str().to_string());
                                        },
                                        Rule::key_ptr => {
                                            ptr = true;
                                        },
                                        Rule::ident => {
                                            typ = part.as_str().to_string();
                                        },
                                        Rule::key_const => {
                                            muta = false;
                                        },
                                        Rule::key_mut => {
                                            muta = true;
                                        },
                                        e => panic!("unexpected rule {:?} in function argument", e),
                                    }
                                }

                                args.push(NamedArg{
                                    name,
                                    typ,
                                    muta,
                                    ptr,
                                    namespace,
                                });
                            }
                        },
                        Rule::block => {
                            loc = Some(Location{
                                line: part.as_span().start_pos().line_col().0,
                                file: n.to_string_lossy().into(),
                                span: part.as_span(),
                            });
                            body = part.as_str().to_string();
                        },
                        e => panic!("unexpected rule {:?} in function", e),
                    }
                }

                module.functions.insert(name.clone(), Function{
                    name,
                    ret,
                    args,
                    body,
                    vis,
                    loc: loc.unwrap(),
                });
            },
            Rule::EOI => {},
            Rule::struct_d => {
                let decl = decl.into_inner();

                let mut vis   = Visibility::Shared;
                let mut name  = None;
                let mut body  = None;
                let mut loc   = None;
                let mut packed = false;

                for part in decl {
                    match part.as_rule() {
                        Rule::key_packed => {
                            packed = true;
                        }
                        Rule::key_private => {
                            vis = Visibility::Object;
                        }
                        Rule::key_pub => {
                            vis = Visibility::Export;
                        }
                        Rule::ident => {
                            name = Some(part.as_str().into());
                        }
                        Rule::struct_c => {
                            loc  = Some(Location{
                                line: part.as_span().start_pos().line_col().0,
                                file: n.to_string_lossy().into(),
                                span: part.as_span(),
                            });
                            body = Some(part.as_str().into());
                        }
                        e => panic!("unexpected rule {:?} in struct ", e),
                    }
                };



                module.structs.push(Struct {
                    name: name.unwrap(),
                    body: body.unwrap(),
                    vis,
                    loc: loc.unwrap(),
                    packed,
                });
            }
            Rule::import => {
                let loc  = Location{
                    line: decl.as_span().start_pos().line_col().0,
                    file: n.to_string_lossy().into(),
                    span: decl.as_span(),
                };
                let mut vis = Visibility::Object;
                let mut decli = None;
                for part in decl.into_inner() {
                    match part.as_rule() {
                        Rule::name => {
                            decli = Some(part);
                            break;
                        },
                        Rule::key_pub => {
                            vis = Visibility::Export;
                        }
                        e => panic!("unexpected rule {:?} in import ", e),
                    }
                };
                let decl = decli.unwrap();

                let namespace  = collect_ns(decl);

                module.imports.push(Import{
                    namespace,
                    vis,
                    loc
                });


            },
            Rule::include => {
                let loc = Location{
                    line: decl.as_span().start_pos().line_col().0,
                    file: n.to_string_lossy().into(),
                    span: decl.as_span(),
                };
                let im = decl.into_inner().as_str();
                module.includes.push(Include{
                    expr: im.to_string(),
                    loc,
                    vis: Visibility::Object,
                });
            },
            Rule::comment => {},
            Rule::istatic => {
                let mut loc     = None;
                let mut typ     = None;
                let mut name    = None;
                let mut expr    = None;
                let mut muta    = false;
                let mut storage = Storage::Static;

                for part in decl.into_inner() {
                    match part.as_rule() {
                        Rule::key_thread_local => {
                            storage = Storage::ThreadLocal;
                        }
                        Rule::key_static => {
                            storage = Storage::Static;
                        }
                        Rule::key_atomic => {
                            storage = Storage::Atomic;
                        }
                        Rule::key_mut => {
                            muta = true;
                        }
                        Rule::key_private | Rule::key_pub => {
                            let e = pest::error::Error::<Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                                message: format!("cannot change visibility static variable"),
                            }, part.as_span());
                            error!("{} : {}", n.to_string_lossy(), e);
                            std::process::exit(9);
                        }
                        Rule::typ if typ.is_none() => {
                            typ = Some(part.as_str().into());
                        },
                        Rule::ident if name.is_none() => {
                            name  = Some(part.as_str().into());
                        }
                        Rule::expression if expr.is_none() => {
                            expr = Some(part.as_str().into());
                            loc = Some(Location{
                                line: part.as_span().start_pos().line_col().0,
                                file: n.to_string_lossy().into(),
                                span: part.as_span(),
                            });
                        }
                        e => panic!("unexpected rule {:?} in static", e),
                    }
                }
                module.statics.insert(name.clone().unwrap(), Static {
                    storage,
                    muta,
                    typ: typ.unwrap(),
                    name: name.unwrap(),
                    expr: expr.unwrap(),
                    loc: loc.unwrap(),
                });
            },
            Rule::constant => {
                let mut loc     = None;
                let mut typ     = None;
                let mut name    = None;
                let mut expr    = None;
                let mut vis     = Visibility::Shared;

                for part in decl.into_inner() {
                    match part.as_rule() {
                        Rule::key_private => {
                            vis = Visibility::Object;
                        }
                        Rule::key_pub => {
                            vis = Visibility::Export;
                        }
                        Rule::typ if typ.is_none() => {
                            typ = Some(part.as_str().into());
                        },
                        Rule::ident if name.is_none() => {
                            name  = Some(part.as_str().into());
                        }
                        Rule::expression if expr.is_none() => {
                            expr = Some(part.as_str().into());
                            loc = Some(Location{
                                line: part.as_span().start_pos().line_col().0,
                                file: n.to_string_lossy().into(),
                                span: part.as_span(),
                            });
                        }
                        e => panic!("unexpected rule {:?} in const", e),
                    }
                }
                module.constants.insert(name.clone().unwrap(), Const {
                    typ: typ.unwrap(),
                    name: name.unwrap(),
                    expr: expr.unwrap(),
                    vis,
                    loc: loc.unwrap(),
                });
            },
            e => panic!("unexpected rule {:?} in file", e),

        }

    }

    Ok(module)
}



fn collect_ns(decl: pest::iterators::Pair<Rule>) -> Vec<String> {
    let mut v = Vec::new();
    for part in decl.into_inner() {
        match part.as_rule() {
            Rule::ident_or_star | Rule::ident => {
                v.push(part.as_str().into());
            }
            Rule::namespace => {
                v.extend(collect_ns(part));
            }
            e => panic!("unexpected rule {:?} in import ", e),
        }
    }
    v
}
