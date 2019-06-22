#[macro_use] extern crate pest_derive;

mod emitter;
mod ast;
mod parser;
mod project;

use emitter::Emitter;

use std::path::Path;
use std::collections::HashMap;
use std::process::Command;

fn main() {
    let project = project::load();
    std::fs::create_dir_all("./target/c/").expect("create target dir");


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


    let mut linkargs  = Vec::new();
    for (name, _) in &modules {
        let inp  = format!("./target/c/{}.c", name);
        let outp = format!("./target/c/{}.o", name);
        let mut args = vec!["-c", &inp, "-o", &outp];
        if let Some(cincs) = &project.cincludes {
            for cinc in cincs {
                args.push("-I");
                args.push(&cinc );
            }
        }

        args.push("-fvisibility=hidden");

        let status = Command::new("clang")
            .args(args)
            .status()
            .expect("failed to execute cc");

        if !status.success() {
            eprintln!("error compiling {}", inp);
            std::process::exit(status.code().unwrap_or(3));
        }

        linkargs.push(outp);
    }

    for entry in std::fs::read_dir("./src").unwrap() {
        let entry = entry.unwrap();
        let path  = entry.path();
        if path.is_file() {
            if let Some("c") = path.extension().map(|v|v.to_str().expect("invalid file name")) {
                linkargs.push(path.to_string_lossy().into());
            }
        }
    }

    linkargs.push("-o".into());
    linkargs.push("./target/exe".into());
    if let Some(cincs) = project.cincludes {
        for cinc in cincs {
            linkargs.push("-I".into());
            linkargs.push(cinc);
        }
    }

    if let Some(c) = project.cobjects {
        for c in c {
            linkargs.push(c);
        }
    }

    if let Some(c) = project.cflags {
        for c in c {
            linkargs.push(c);
        }
    }

    linkargs.push("-fvisibility=hidden".into());

    println!("{:?}", linkargs);

    let status = Command::new("clang")
        .args(&linkargs)
        .status()
        .expect("failed to execute linker");
    if !status.success() {
        std::process::exit(status.code().unwrap_or(3));
    }

}

