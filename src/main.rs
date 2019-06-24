#[macro_use] extern crate pest_derive;
extern crate fasthash;
extern crate clap;
#[macro_use] extern crate log;
extern crate env_logger;

mod emitter;
mod ast;
mod parser;
mod project;
mod make;
mod resolver;

use emitter::Emitter;

use std::path::Path;
use clap::{App, SubCommand};
use std::process::Command;

fn main() {
    if let Err(_) = std::env::var("RUST_LOG") {
        std::env::set_var("RUST_LOG", "info");
    }
    env_logger::builder()
        .default_format_timestamp(false)
        .init();

    let matches = App::new("Drunk Octopus")
        .version(clap::crate_version!())
        .setting(clap::AppSettings::ArgRequiredElseHelp)
        .setting(clap::AppSettings::UnifiedHelpMessage)
        .subcommand(SubCommand::with_name("build").about("build the current project"))
        .subcommand(SubCommand::with_name("clean").about("remove the target directory"))
        .subcommand(SubCommand::with_name("run").about("build and run"))
        .get_matches();

    match matches.subcommand() {
        ("clean", Some(_submatches)) => {
            let (root, _) = project::load();
            std::env::set_current_dir(root).unwrap();
            if std::path::Path::new("./target").exists() {
                std::fs::remove_dir_all("target").unwrap();
            }
        },
        ("test", Some(_submatches)) => {
        }
        ("run", Some(_submatches)) => {
            build();
            let (root, mut project) = project::load();
            std::env::set_current_dir(root).unwrap();

            let mut exes = Vec::new();
            for artifact in std::mem::replace(&mut project.artifacts, None).expect("no artifacts") {
                if let project::ArtifactType::Exe = artifact.typ {
                    exes.push(artifact);
                }
            }
            if exes.len() < 1 {
                error!("no exe artifact to run");
                std::process::exit(9);
            }
            if exes.len() > 1 {
                error!("multiple exe artifacts");
                std::process::exit(9);
            }

            info!("running \"./target/{}\"\n", exes[0].name);
            let status = Command::new(format!("./target/{}", exes[0].name))
                .status()
                .expect("failed to execute process");
            std::process::exit(status.code().expect("failed to execute process"));
        },
        ("build", Some(_submatches)) => {
            build()
        },
        _ => unreachable!(),
    }
}



fn build() {

    let (root, mut project) = project::load();
    std::env::set_current_dir(root).unwrap();

    std::fs::create_dir_all("./target/c/").expect("create target dir");
    std::fs::create_dir_all("./target/zz/").expect("create target dir");
    std::fs::create_dir_all("./target/include/").expect("create target dir");

    let namespace = vec![project.project.name.clone()];

    for artifact in std::mem::replace(&mut project.artifacts, None).expect("no artifacts") {
        let modules   = resolver::resolve(&namespace, &Path::new(&artifact.file));
        for (name, md) in &modules {
            let mut em = Emitter::new(md.namespace.clone());

            debug!("emitting {}", name);

            em.import(&modules, md.imports.clone());

            for i in &md.includes {
                em.include(i);
            }
            for (_,v) in &md.macros {
                em.imacro(&v);
            }
            for s in &md.structs {
                em.struc(&s);
            }
            for (_,v) in &md.statics {
                em.istatic(&v);
            }
            for (_,v) in &md.constants {
                em.constant(&v);
            }
            for (_,fun) in &md.functions {
                em.declare(&fun, &md.namespace);
            }
            for (_,fun) in &md.functions {
                em.define(&fun, &md.namespace, &fun.body);
            }
        }

        let mut header = Emitter::new_export_header(vec![project.project.name.clone()]);
        for (_name, md) in &modules {
            for i in &md.includes {
                if let ast::Visibility::Export = i.vis {
                    if i.expr.contains("<") {
                        header.include(i);
                    } else {
                        let e = pest::error::Error::<parser::Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                            message: format!("cannot re-export local c header"),
                        }, i.loc.span.clone());
                        error!("{} : {}", i.loc.file, e);
                        std::process::exit(9);
                    }
                }
            }
        }
        for (_name, md) in &modules {
            for i in &md.imports {
                if let ast::Visibility::Export = i.vis {
                    header.import(&modules, vec![i.clone()]);
                }
            }
        }
        for (_name, md) in &modules {
            for s in &md.structs {
                if let ast::Visibility::Export = s.vis {
                    header.struc(&s);
                }
            }
        }
        for (_name, md) in &modules {
            for (_,fun) in &md.functions {
                if let ast::Visibility::Export = fun.vis {
                    header.declare(&fun, &md.namespace);
                }
            }
        }
        drop(header);

        let mut make = make::Make::new(project.project.clone(), artifact);

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
}

