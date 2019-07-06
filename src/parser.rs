use pest::Parser;
use super::ast::*;
use std::path::Path;
use std::io::{Read};

#[derive(Parser)]
#[grammar = "zz.pest"]
pub struct ZZParser;


pub fn make_error<S: Into<String>>(loc: &Location, message: S) -> pest::error::Error<Rule> {
    pest::error::Error::<Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
        message: message.into(),
    }, loc.span.clone()).with_path(&loc.file)
}

pub fn parse(ns: Vec<String>, n: &Path) -> Module
{
    match p(ns, &n){
        Err(e) => {
            let e = e.with_path(&n.to_string_lossy());
            error!("syntax error\n{}", e);
            std::process::exit(9);
        }
        Ok(md) => {
            md
        }
    }
}

fn p(nsi: Vec<String>, n: &Path) -> Result<Module, pest::error::Error<Rule>> {

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
                let loc = Location{
                    file: n.to_string_lossy().into(),
                    span: decl.as_span(),
                };
                let decl = decl.into_inner();
                let mut bodyloc  = None;
                let mut name = None;
                let mut args = Vec::new();
                let mut imports = Vec::new();
                let mut body = None;
                let mut vis = Visibility::Object;
                for part in decl {
                    match part.as_rule() {
                        Rule::key_shared => {
                            vis = Visibility::Shared;
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
                            bodyloc = Some(Location{
                                file: n.to_string_lossy().into(),
                                span: part.as_span(),
                            });
                            body = Some(part.as_str().to_string());
                        },
                        e => panic!("unexpected rule {:?} in macro ", e),
                    }
                }

                module.locals.push(Local{
                    name: name.unwrap().to_string(),
                    vis,
                    loc,
                    def:  Def::Macro{
                        args,
                        body: CExpr{
                            expr: body.unwrap(),
                            loc:  bodyloc.unwrap(),
                        },
                        imports,
                    }
                });

            }
            Rule::function => {
                let loc = Location{
                    file: n.to_string_lossy().into(),
                    span: decl.as_span(),
                };
                let mut bodyloc  = None;
                let decl = decl.into_inner();
                let mut name = String::new();
                let mut args = Vec::new();
                let mut ret  = None;
                let mut body = None;
                let mut vis = Visibility::Object;

                for part in decl {
                    match part.as_rule() {
                        Rule::key_shared => {
                            vis = Visibility::Shared;
                        }
                        Rule::key_pub => {
                            vis = Visibility::Export;
                        }
                        Rule::ident => {
                            name = part.as_str().into();
                        }
                        Rule::ret_arg => {
                            let mut namespace = None;
                            let mut ptr       = false;
                            let mut typeref   = String::new();
                            let mut typeloc   = None;

                            for part in part.into_inner().next().unwrap().into_inner().next().unwrap().into_inner() {
                                match part.as_rule() {
                                    Rule::namespace => {
                                        namespace = Some(part.as_str().to_string());
                                    },
                                    Rule::key_ptr => {
                                        ptr = true;
                                    },
                                    Rule::ident => {
                                        typeref = part.as_str().to_string();
                                        typeloc = Some(Location{
                                            file: n.to_string_lossy().into(),
                                            span: part.as_span(),
                                        })
                                    },
                                    e => panic!("unexpected rule {:?} in return argument", e),
                                }
                            }

                            ret = Some(AnonArg{
                                typeref: TypeUse {
                                    name : typeref.clone(),
                                    loc : typeloc.unwrap(),
                                },
                                ptr,
                                namespace,
                            });
                        },
                        Rule::fn_args => {
                            for arg in part.into_inner() {
                                let mut arg       = arg.into_inner();
                                let types         = arg.next().unwrap();
                                let name          = arg.next().unwrap().as_str().to_string();
                                let mut muta      = false;
                                let mut ptr       = false;
                                let mut typeref   = String::new();
                                let mut typeloc   = None;
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
                                            typeref = part.as_str().to_string();
                                            typeloc = Some(Location{
                                                file: n.to_string_lossy().into(),
                                                span: part.as_span(),
                                            })
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

                                let typeref = TypeUse{
                                    name : typeref.clone(),
                                    loc: typeloc.unwrap(),
                                };

                                args.push(NamedArg{
                                    name,
                                    typeref,
                                    muta,
                                    ptr,
                                    namespace,
                                });
                            }
                        },
                        Rule::block => {
                            bodyloc = Some(Location{
                                file: n.to_string_lossy().into(),
                                span: part.as_span(),
                            });
                            body = Some(part.as_str().to_string());
                        },
                        e => panic!("unexpected rule {:?} in function", e),
                    }
                }

                module.locals.push(Local{
                    name,
                    vis,
                    loc,
                    def:Def::Function{
                        ret,
                        args,
                        body: CExpr{
                            expr: body.unwrap(),
                            loc:  bodyloc.unwrap(),
                        },
                    }
                });
            },
            Rule::EOI => {},
            Rule::struct_d => {
                let decl = decl.into_inner();

                let mut vis    = Visibility::Object;
                let mut name   = None;
                let mut fields = Vec::new();
                let mut loc    = None;
                let mut packed = false;

                for part in decl {
                    match part.as_rule() {
                        Rule::key_packed => {
                            packed = true;
                        }
                        Rule::key_shared => {
                            vis = Visibility::Shared;
                        }
                        Rule::key_pub => {
                            vis = Visibility::Export;
                        }
                        Rule::ident => {
                            loc  = Some(Location{
                                file: n.to_string_lossy().into(),
                                span: part.as_span(),
                            });
                            name= Some(part.as_str().into());
                        }
                        Rule::field => {
                            let loc  = Location{
                                file: n.to_string_lossy().into(),
                                span: part.as_span(),
                            };

                            let mut field = part.into_inner();
                            let typespan = field.next().unwrap();
                            let typeref  = typespan.as_str().to_string();
                            let expr     = field.next().unwrap();
                            let expr     = CExpr{
                                expr: expr.as_str().into(),
                                loc:  Location{
                                    file: n.to_string_lossy().into(),
                                    span: expr.as_span(),
                                }
                            };

                            let typeref = TypeUse{
                                name : typeref.clone(),
                                loc : Location{
                                    file: n.to_string_lossy().into(),
                                    span: typespan.as_span(),
                                },
                            };

                            fields.push(Field{
                                typeref,
                                expr,
                                loc,
                            });

                            //});
                            //body = Some(part.as_str().into());
                        }
                        e => panic!("unexpected rule {:?} in struct ", e),
                    }
                };



                module.locals.push(Local{
                    name: name.unwrap(),
                    vis,
                    loc: loc.unwrap(),
                    def: Def::Struct {
                        fields,
                        packed,
                    }
                });
            }
            Rule::import => {
                let loc  = Location{
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
                    file: n.to_string_lossy().into(),
                    span: decl.as_span(),
                };
                let im = decl.into_inner().as_str();
                module.includes.push(Include{
                    expr: im.to_string(),
                    loc,
                });
            },
            Rule::comment => {},
            Rule::istatic => {
                let loc     = Location{
                    file: n.to_string_lossy().into(),
                    span: decl.as_span(),
                };
                let mut typeref = None;
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
                        Rule::key_shared | Rule::key_pub => {
                            let e = pest::error::Error::<Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                                message: format!("cannot change visibility static variable"),
                            }, part.as_span());
                            error!("{} : {}", n.to_string_lossy(), e);
                            std::process::exit(9);
                        }
                        Rule::typ if typeref.is_none() => {
                            typeref =  Some(TypeUse {
                                name : part.as_str().into(),
                                loc : Location{
                                    file: n.to_string_lossy().into(),
                                    span: part.as_span().clone(),
                                },
                            });
                        },
                        Rule::ident if name.is_none() => {
                            name  = Some(part.as_str().to_string());
                        }
                        Rule::expression if expr.is_none() => {
                            expr = Some(CExpr{
                                expr: part.as_str().into(),
                                loc: Location{
                                    file: n.to_string_lossy().into(),
                                    span: part.as_span(),
                                }
                            });
                        }
                        e => panic!("unexpected rule {:?} in static", e),
                    }
                }
                module.locals.push(Local{
                    name: name.unwrap(),
                    loc,
                    vis: Visibility::Object,
                    def: Def::Static {
                        storage,
                        muta,
                        typeref: typeref.unwrap(),
                        expr: expr.unwrap(),
                    }
                });
            },
            Rule::constant => {
                let loc     = Location{
                    file: n.to_string_lossy().into(),
                    span: decl.as_span(),
                };
                let mut typeref = None;
                let mut name    = None;
                let mut expr    = None;
                let mut vis     = Visibility::Object;

                for part in decl.into_inner() {
                    match part.as_rule() {
                        Rule::key_shared => {
                            vis = Visibility::Shared;
                        }
                        Rule::key_pub => {
                            vis = Visibility::Export;
                        }
                        Rule::typ if typeref.is_none() => {
                            typeref =  Some(TypeUse {
                                name : part.as_str().into(),
                                loc : Location{
                                    file: n.to_string_lossy().into(),
                                    span: part.as_span().clone(),
                                },
                            });
                        },
                        Rule::ident if name.is_none() => {
                            name  = Some(part.as_str().into());
                        }
                        Rule::expression if expr.is_none() => {
                            expr = Some(CExpr{
                                expr: part.as_str().into(),
                                loc: Location{
                                    file: n.to_string_lossy().into(),
                                    span: part.as_span(),
                                }
                            });
                        }
                        e => panic!("unexpected rule {:?} in const", e),
                    }
                }
                module.locals.push(Local{
                    name: name.unwrap(),
                    vis,
                    loc,
                    def: Def::Const {
                        typeref: typeref.unwrap(),
                        expr: expr.unwrap(),
                    }
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
