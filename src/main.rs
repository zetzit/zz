#[macro_use] extern crate pest_derive;
extern crate fasthash;

mod emitter;
mod ast;
mod parser;
mod project;
mod make;

use emitter::Emitter;

use std::path::Path;
use std::collections::HashMap;

fn main() {
    let (root, project) = project::load();
    std::env::set_current_dir(root).unwrap();

    std::fs::create_dir_all("./target/c/").expect("create target dir");
    std::fs::create_dir_all("./target/zz/").expect("create target dir");
    std::fs::create_dir_all("./target/include/").expect("create target dir");


    let namespace = vec![project.name.clone()];
    let mut modules = HashMap::new();
    parser::parse(&mut modules, &namespace, &Path::new("./src/main.zz"));

    for (name, md) in &modules {
        let mut em = Emitter::new(&(name.clone() + ".c"));
        for i in &md.includes {
            em.include(i);
        }

        for mp in &md.imports {
            match modules.get(&mp.namespace.join("::")) {
                None => panic!("{}: imports unknown module {}", name, &mp.namespace.join("::")),
                Some(m2) => {
                    em.import(&modules, m2, mp);
                }
            }
        }

        for (_,v) in &md.macros {
            em.imacro(&v);
        }
        for (_,v) in &md.statics {
            em.istatic(&v);
        }
        for (_,v) in &md.constants {
            em.constant(&v);
        }
        for s in &md.structs {
            em.struc(&s);
        }
        for (_,fun) in &md.functions {
            em.declare(&fun, &md.namespace);
        }
        for (_,fun) in &md.functions {
            em.define(&fun, &md.namespace, &fun.body);
        }
    }



    let mut header = Emitter::new_export_header(&project.name);
    for (name, md) in &modules {
        for i in &md.includes {
            if let ast::Visibility::Export = i.vis {
                if i.expr.contains("<") {
                    header.include(i);
                } else {
                    let e = pest::error::Error::<parser::Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                        message: format!("cannot re-export local c header"),
                    }, i.loc.span.clone());
                    eprintln!("{} : {}", i.loc.file, e);
                    std::process::exit(9);
                }
            }
        }
    }
    for (name, md) in &modules {
        for s in &md.structs {
            if let ast::Visibility::Export = s.vis {
                header.struc(&s);
            }
        }
    }
    for (name, md) in &modules {
        for (_,fun) in &md.functions {
            if let ast::Visibility::Export = fun.vis {
                header.declare(&fun, &md.namespace);
            }
        }
    }
    drop(header);




    let mut make = make::Make::new(project);

    for (_, module) in &modules {
        make.module(&module);
    }

    for entry in std::fs::read_dir("./src").unwrap() {
        let entry = entry.unwrap();
        let path  = entry.path();
        if path.is_file() {
            if let Some("c") = path.extension().map(|v|v.to_str().expect("invalid file name")) {
                make.cobject(&path);
            }
        }
    }

    make.link();
}

