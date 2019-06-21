#[macro_use] extern crate pest_derive;

mod emitter;
mod ast;
mod parser;

use emitter::Emitter;

use std::path::Path;
use std::collections::HashMap;
use std::process::Command;

fn main() {
    std::fs::create_dir_all("./target/c/").expect("create target dir");

    let mut modules = HashMap::new();
    parser::parse(&mut modules, &Path::new("./src/main.zz"));

    for (name, md) in &modules {
        let mut em = Emitter::new(&(name.clone() + ".c"));
        for i in &md.includes {
            em.include(i);
        }
        for mp in &md.imports {
            match modules.get(&mp.namespace) {
                None => panic!("{}: imports unknown module {}", name, &mp.namespace),
                Some(m2) => {
                    em.import(&modules, m2, mp);
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
            .args(&["-I", ".", "-c", &inp, "-o", &outp])
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


    println!("{:?}", linkargs);

    let status = Command::new("clang")
        .args(&linkargs)
        .status()
        .expect("failed to execute linker");
    if !status.success() {
        std::process::exit(status.code().unwrap_or(3));
    }

}

