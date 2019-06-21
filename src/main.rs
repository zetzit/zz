#[macro_use] extern crate pest_derive;

mod emitter;
mod ast;
mod parser;

use emitter::Emitter;
use ast::*;

use std::path::Path;
use std::collections::HashMap;
use std::process::Command;

fn main() {
    std::fs::create_dir_all("./target/c/").expect("create target dir");

    let mut modules = HashMap::new();
    parser::parse(&mut modules, &Path::new("main.zz"));

    for (name, md) in &modules {
        let mut em = Emitter::new(&(name.clone() + ".c"));
        for i in &md.includes {
            em.include(i);
        }
        for mp in &md.imports {
            match modules.get(&mp.namespace) {
                None => panic!("{}: imports unknown module {}", name, &mp.namespace),
                Some(m2) => {
                    for i in &m2.includes {
                        em.include(i);
                    }

                    let mut found = false;
                    for (name,fun) in &m2.functions {
                        if name == &mp.name {
                            if let Visibility::Object  = fun.vis {
                                panic!("{}: imports private function {}::{}", name, &mp.namespace, &mp.name);
                            };
                            found = true;
                            em.function(None, &fun, None);
                            //em.function(Some(&m2.name), &fun, None);
                        }
                    }

                    for s in &m2.structs {
                        if s.name == mp.name {
                            if let Visibility::Object  = s.vis {
                                panic!("{}: imports private struct {}::{}", name, &mp.namespace, &mp.name);
                            };
                            found = true;
                            em.struc(None, &s);
                            //em.struc(Some(&m2.name), &s);
                        }
                    }

                    if !found {
                        panic!("{}: import '{}' not found in module '{}'", name, &mp.name, &mp.namespace);
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
    for (name, _) in &modules {
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

