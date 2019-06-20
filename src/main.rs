#[macro_use] extern crate pest_derive;

use pest::RuleType;
use pest::Parser;
use std::fmt::Debug;
use std::io::{self, Read};
use std::io::Write;
use std::path::Path;
use std::collections::HashMap;
use std::fs;
use std::process::Command;

#[derive(Parser)]
#[grammar = "zz.pest"]
pub struct ZZParser;
pub struct Emitter{
    f: fs::File
}

pub enum Visibility {
    Shared,
    Object,
    Export,
}

#[derive(Default)]
pub struct Module {
    name:       String,
    functions:  HashMap<String, Function>,
    imports:    Vec<String>,
    structs:    Vec<Struct>,
    includes:   Vec<String>,
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
    body:   String,
    vis:    Visibility,
}

pub struct Struct {
    name:   String,
    body:   String,
    vis:    Visibility,
}

impl Emitter {

    pub fn new(mn: &str) -> Self {
        let p = format!("target/c/{}", mn);
        let f = fs::File::create(&p).expect(&format!("cannot create {}", p));
        Emitter{f}
    }

    pub fn struc(&mut self, ns: Option<&str>, s: &Struct) {
        write!(self.f, "typedef struct \n{}\n",
                 s.body,
                 ).unwrap();

        if let Some(ns) = ns {
            write!(self.f, "{}_",ns).unwrap();
        }

        write!(self.f, "{} ;\n", s.name).unwrap();
    }

    pub fn function(&mut self, ns: Option<&str>, f: &Function, body: Option<&str>) {
        if let Visibility::Object = f.vis {
            write!(self.f, "static ").unwrap();
        }

        match &f.ret {
            None       => write!(self.f, "void "),
            Some(a)    => write!(self.f, "{} ", &a.typ),
        }.unwrap();

        if let Some(ns) = ns {
            write!(self.f, "{}_",ns).unwrap();
        }

        write!(self.f, "{} (", f.name).unwrap();

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
        match body {
            None => write!(self.f, ");\n").unwrap(),
            Some(b) => write!(self.f, ") \n{}\n\n", b).unwrap(),
        }
    }

    pub fn include(&mut self, i: &str) {
        let guardname : String = i.chars().filter(|&c| "abcdefghijklmnopqrstuvwxyz1234567890".contains(c)).collect();
        write!(self.f, "#ifndef ZZ_INCLUDE_{}\n", guardname).unwrap();
        write!(self.f, "#define ZZ_INCLUDE_{}\n", guardname).unwrap();
        write!(self.f, "#include {}\n", i).unwrap();
        write!(self.f, "#endif\n").unwrap();
    }

}


fn main() {
    std::fs::create_dir_all("./target/c/").expect("create target dir");

    let mut modules = HashMap::new();
    pp(&mut modules, &Path::new("main.zz"));

    for (name, md) in &modules {
        let mut em = Emitter::new(&(name.clone() + ".c"));
        for i in &md.includes {
            em.include(i);
        }
        for mp in &md.imports {
            match modules.get(mp) {
                None => panic!("{}: imports unknown module {}", name, mp),
                Some(m2) => {
                    for i in &m2.includes {
                        em.include(i);
                    }
                    for (_,fun) in &m2.functions {
                        if let Visibility::Object  = fun.vis { continue };
                        em.function(None, &fun, None);
                        //em.function(Some(&m2.name), &fun, None);
                    }
                    for s in &m2.structs {
                        if let Visibility::Object  = s.vis { continue };
                        em.struc(None, &s);
                        //em.struc(Some(&m2.name), &s);
                    }
                }
            }
        }
        for s in &md.structs {
            em.struc(None, &s);
        }
        for (_,fun) in &md.functions {
            em.function(None, &fun, Some(&fun.body));
        }
    }


    let mut linkargs  = Vec::new();
    for (name, md) in &modules {
        let inp  = format!("./target/c/{}.c", name);
        let outp = format!("./target/c/{}.o", name);

        let status = Command::new("clang")
            .args(&["-c", &inp, "-o", &outp])
            .status()
            .expect("failed to execute cc");

        if !status.success() {
            std::process::exit(status.code().unwrap_or(3));
        }

        linkargs.push(outp);
    }

    linkargs.push("-o".into());
    linkargs.push("./target/exe".into());

    let status = Command::new("clang")
        .args(&linkargs)
        .status()
        .expect("failed to execute linker");
    if !status.success() {
        std::process::exit(status.code().unwrap_or(3));
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
                });
            },
            Rule::EOI=> {},
            Rule::struct_d => {
                let mut decl = decl.into_inner();

                let mut vis   = Visibility::Shared;
                let mut name  = None;
                let mut body  = None;

                for part in decl {
                    match part.as_rule() {
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
                            body = Some(part.as_str().into());
                        }
                        e => panic!("unexpected rule {:?} in struct ", e),
                    }
                };


                module.structs.push(Struct {
                    name: name.unwrap(),
                    body: body.unwrap(),
                    vis,
                });
            }
            Rule::import => {
                let im = decl.into_inner().as_str();
                if !modules.contains_key(im) {
                    pp(modules, &Path::new(im).with_extension("zz"));
                }
                module.imports.push(im.to_string());
            },
            Rule::include => {
                let im = decl.into_inner().as_str();
                module.includes.push(im.to_string());
            },
            e => panic!("unexpected rule {:?} in file", e),

        }

    }

    Ok(module)
}
