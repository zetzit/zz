#[macro_use] extern crate pest_derive;
extern crate fasthash;
extern crate clap;
#[macro_use] extern crate log;
extern crate env_logger;

mod ast;
mod parser;
mod project;
//mod make;
mod loader;
mod flatten;
mod emitter;
mod abs;
mod name;

use std::path::Path;
use clap::{App, SubCommand};
use std::process::Command;
use name::Name;
use std::collections::HashSet;
use std::collections::HashMap;

fn main() {
    if let Err(_) = std::env::var("RUST_LOG") {
        std::env::set_var("RUST_LOG", "info");
    }
    env_logger::builder()
        //.default_format_module_path(false)
        .default_format_timestamp(false)
        .init();

    let matches = App::new("Drunk Octopus")
        .version(clap::crate_version!())
        .setting(clap::AppSettings::UnifiedHelpMessage)
        .subcommand(SubCommand::with_name("build").about("build the current project"))
        .subcommand(SubCommand::with_name("clean").about("remove the target directory"))
        .subcommand(SubCommand::with_name("test").about("execute all in tests/*.zz"))
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
            build(true);
            let (root, mut project) = project::load();
            std::env::set_current_dir(root).unwrap();

            for artifact in std::mem::replace(&mut project.artifacts, None).expect("no artifacts") {
                if let project::ArtifactType::Test = artifact.typ {
                    info!("running \"./target/{}\"\n", artifact.name);
                    let status = Command::new(format!("./target/{}", artifact.name))
                        .status()
                        .expect("failed to execute process");
                    if let Some(0) = status.code()  {
                        info!("PASS {}", artifact.name);
                    } else {
                        error!("FAIL {} {:?}", artifact.name, status);
                        std::process::exit(10);
                    }
                }
            }

        }
        ("run", Some(_submatches)) => {
            build(false);
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
        ("build", _) | ("", None) => {
            build(false)
        },
        _ => unreachable!(),
    }
}



fn build(tests: bool) {

    let (root, project) = project::load();
    std::env::set_current_dir(root).unwrap();

    std::fs::create_dir_all("./target/c/").expect("create target dir");
    std::fs::create_dir_all("./target/zz/").expect("create target dir");
    std::fs::create_dir_all("./target/include/").expect("create target dir");

    let project_name        = Name(vec![String::new(), project.project.name.clone()]);
    let project_tests_name  = Name(vec![String::new(), project.project.name.clone(), "tests".to_string()]);

    let mut modules = HashMap::new();
    loader::load(&mut modules, &project_name, &Path::new("./src"));
    loader::load(&mut modules, &project_tests_name, &Path::new("./tests"));

    let names : HashSet<Name> = modules.keys().cloned().collect();
    for name in &names {
        let mut md = modules.remove(name).unwrap();
        match &mut md {
            loader::Module::C(_) => (),
            loader::Module::ZZ(ast) => abs::abs(ast, &modules),
        }
        modules.insert(name.clone(), md);
    }

    let mut flat = Vec::new();
    for name in &names {
        let mut md = modules.remove(name).unwrap();
        match &mut md {
            loader::Module::C(_) => (),
            loader::Module::ZZ(ast) => flat.push(flatten::flatten(ast, &modules)),
        }
        modules.insert(name.clone(), md);
    }

    for module in flat {
        let em = emitter::Emitter::new(module, false);
        em.emit();
    }

    //for artifact in std::mem::replace(&mut project.artifacts, None).expect("no artifacts") {
    //    let mut artifact_name = Name(artifact.name.split("::").map(|s|s.to_string()).collect());
    //    artifact_name.0.insert(0, String::new());
    //};


    /*
    for artifact in std::mem::replace(&mut project.artifacts, None).expect("no artifacts") {

        let project_name = Name(vec![String::new(), project.project.name.clone()]);
        project_name.0.insert(0, String::new());
        let artifact_name = project_name.join(Name(artifact.name.split("::").map(|s|s.to_string()).collect()));


        match (tests, &artifact.typ) {
            (false, project::ArtifactType::Test) => continue,
            (false, _) => (),
            (true,  project::ArtifactType::Test) => (),
            (true,  project::ArtifactType::Lib) => (),
            (true,  _) => continue,
        }

        let mut modules = resolver::resolve(&project_name, &artifact_name, &Path::new(&artifact.file));
        for (name, md) in &mut modules {
            abs::abs(name, md);
        }
        let modules = flatten::Flatten::new(modules).run();

        let cfiles : Vec<emitter::CFile> = modules.into_iter().map(|(name, module)|{
            let em = emitter::Emitter::new(name.join("::"), module, artifact.typ.clone());
            em.emit()
        }).collect();

        let mut make = make::Make::new(project.project.clone(), artifact);
        for module in cfiles {
            make.build(module);
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
    */
}

