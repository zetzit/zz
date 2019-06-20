#[macro_use] extern crate pest_derive;

use pest::RuleType;
use pest::Parser;
use std::fmt::Debug;
use std::io::{self, Read};
use std::io::Write;
use std::path::Path;
use std::collections::HashMap;
use std::fs;

#[derive(Parser)]
#[grammar = "zz.pest"]
pub struct ZZParser;
pub struct Emitter{
    f: fs::File
}


#[derive(Default)]
pub struct Module {
    name:       String,
    functions:  HashMap<String, Function>,
    imports:    Vec<String>
}

pub struct AnonArg {
    typ:    String
}

pub struct NamedArg {
    typ:    String,
    name:   String,
    muta:   bool,
    ptr:    bool,
    namespace: Option<String>,
}

pub struct Function {
    ret:    Option<AnonArg>,
    args:   Vec<NamedArg>,
    name:   String,
}

impl Emitter {

    pub fn new(mn: &str) -> Self {
        let p = format!("target/c/{}", mn);
        let f = fs::File::create(&p).expect(&format!("cannot create {}", p));
        Emitter{f}
    }

    pub fn define(&mut self, md: &str, f: &Function) {
        write!(self.f, "{} {}_{} (",
                 match &f.ret {
                     None       => "void",
                     Some(a)    => &a.typ,
                 },
                 md,
                 f.name).unwrap();

        let mut first = true ;
        for arg in &f.args {
            if first {
                first = false;
            } else {
                write!(self.f, ", ").unwrap();
            }

            if !arg.muta {
                write!(self.f, "const ").unwrap();
            }

            if let Some(ns) = &arg.namespace {
                write!(self.f, "{}", ns.replace("::", "_")).unwrap();
            }

            write!(self.f, "{}", arg.typ).unwrap();

            if arg.ptr {
                write!(self.f, " *").unwrap();
            }

            write!(self.f, " {}", arg.name).unwrap();
        }
        write!(self.f, ");\n").unwrap();

    }
}


fn main() {
    std::fs::create_dir_all("./target/c/").expect("create target dir");

    let mut modules = HashMap::new();
    pp(&mut modules, &Path::new("main.zz"));

    for (name, md) in &modules {
        let mut em = Emitter::new(&(name.clone() + ".c"));
        for (_,fun) in &md.functions {
            em.define(&name, &fun);
        }
        for mp in &md.imports {
            match modules.get(mp) {
                None => panic!("{}: imports unknown module {}", name, mp),
                Some(m2) => {
                    for (_,fun) in &m2.functions {
                        em.define(&m2.name, &fun);
                    }
                }
            }

        }
    }
}

fn pp(modules: &mut HashMap<String, Module>, n: &Path)
{
    match p(modules, &Path::new(n)){
        Err(e) => {
            eprintln!("parse error in {:?} : {}", n, e);
            std::process::exit(9);
        }
        Ok(md) => {
            modules.insert(md.name.clone(), md);
        }
    }
}

fn p(modules: &mut HashMap<String, Module>, n: &Path) -> Result<Module, pest::error::Error<Rule>> {
    let mut module = Module::default();
    module.name = n.file_stem().unwrap().to_string_lossy().into();

    let mut f = std::fs::File::open(n).expect(&format!("cannot open file {:?}", n));
    let mut file = String::new();
    f.read_to_string(&mut file).unwrap();
    let mut file = ZZParser::parse(Rule::file, &file)?;

    for decl in file.next().unwrap().into_inner() {
        match decl.as_rule() {
            Rule::function => {
                let mut decl = decl.into_inner();
                let name : String = decl.next().unwrap().as_str().into();
                let mut args = Vec::new();
                let mut ret  = None;

                for part in decl {
                    match part.as_rule() {
                        Rule::ret_arg => {
                            ret = Some(AnonArg{
                                typ: part.into_inner().as_str().to_string()
                            });
                        },
                        Rule::fn_args => {
                            for arg in part.into_inner() {
                                let mut arg = arg.into_inner();
                                let types  = arg.next().unwrap();
                                let name    = arg.next().unwrap().as_str().to_string();
                                let mut muta        = false;
                                let mut ptr         = false;
                                let mut typ         = String::new();
                                let mut namespace   = None;

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
                        Rule::block=> {
                        },
                        e => panic!("unexpected rule {:?} in function", e),
                    }
                }

                module.functions.insert(name.clone(), Function{
                    name,
                    ret,
                    args,
                });
            },
            Rule::EOI=> {},
            Rule::import => {
                let im = decl.into_inner().as_str();
                if !modules.contains_key(im) {
                    pp(modules, &Path::new(im).with_extension("zz"));
                    module.imports.push(im.to_string());
                }
            },
            e => panic!("unexpected rule {:?} in file", e),

        }

    }

    Ok(module)
}
