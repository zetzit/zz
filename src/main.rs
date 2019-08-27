#[macro_use] extern crate pest_derive;
extern crate fasthash;
extern crate clap;
#[macro_use] extern crate log;
extern crate env_logger;
extern crate pbr;
extern crate rayon;

mod ast;
mod parser;
mod project;
mod make;
mod loader;
mod flatten;
mod emitter;
mod abs;
mod name;
mod pp;
mod lifetimes;

use std::path::Path;
use clap::{App, Arg, SubCommand};
use std::process::Command;
use name::Name;
use std::collections::HashSet;
use std::collections::HashMap;
use std::sync::atomic::{Ordering};

fn main() {
    if let Err(_) = std::env::var("RUST_LOG") {
        std::env::set_var("RUST_LOG", "info");
    }
    env_logger::builder()
        //.default_format_module_path(false)
        .default_format_timestamp(false)
        .default_format_module_path(false)
        .init();

    let matches = App::new("Drunk Octopus")
        .version(clap::crate_version!())
        .setting(clap::AppSettings::UnifiedHelpMessage)
        .subcommand(SubCommand::with_name("check").about("check the current project"))
        .subcommand(SubCommand::with_name("build").about("build the current project"))
        .subcommand(SubCommand::with_name("clean").about("remove the target directory"))
        .subcommand(SubCommand::with_name("test").about("execute tests/*.zz")
                    .arg(Arg::with_name("testname").takes_value(true).required(false).index(1)),
                    )
        .subcommand(SubCommand::with_name("init").about("init zz project in current directory"))
        .subcommand(
            SubCommand::with_name("run").about("build and run")
            .arg(Arg::with_name("args").takes_value(true).multiple(true).required(false).index(1)),
        )
        .get_matches();

    match matches.subcommand() {
        ("init", Some(_submatches)) => {
            project::init();
        },
        ("clean", Some(_submatches)) => {
            let (root, _) = project::load_cwd();
            std::env::set_current_dir(root).unwrap();
            if std::path::Path::new("./target").exists() {
                std::fs::remove_dir_all("target").unwrap();
            }
        },
        ("test", Some(submatches)) => {
            build(true, false);
            let (root, mut project) = project::load_cwd();
            std::env::set_current_dir(root).unwrap();

            for artifact in std::mem::replace(&mut project.artifacts, None).expect("no artifacts") {
                if let project::ArtifactType::Test = artifact.typ {
                    if let Some(testname) = submatches.value_of("testname") {
                        if testname != artifact.name {
                            if format!("tests::{}", testname) != artifact.name {
                                continue;
                            }
                        }
                    }
                    println!("running \"./target/{}\"\n", artifact.name);
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
        ("run", Some(submatches)) => {
            build(false, false);
            let (root, mut project) = project::load_cwd();
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

            println!("running \"./target/{}\"\n", exes[0].name);
            let status = Command::new(format!("./target/{}", exes[0].name))
                .args(submatches.values_of("args").unwrap_or_default())
                .status()
                .expect("failed to execute process");
            std::process::exit(status.code().expect("failed to execute process"));
        },
        ("check", _) => {
            parser::ERRORS_AS_JSON.store(true, Ordering::SeqCst);
            build(false, true)
        },
        ("build", _) | ("", None) => {
            build(false, false)
        },
        _ => unreachable!(),
    }
}



fn build(tests: bool, check: bool) {
    use rayon::prelude::*;
    use std::sync::{Arc, Mutex};

    let (root, mut project) = project::load_cwd();
    std::env::set_current_dir(root).unwrap();

    std::fs::create_dir_all("./target/c/").expect("create target dir");
    std::fs::create_dir_all("./target/zz/").expect("create target dir");
    std::fs::create_dir_all("./target/include/").expect("create target dir");

    let project_name        = Name(vec![String::new(), project.project.name.clone()]);
    let project_tests_name  = Name(vec![String::new(), project.project.name.clone(), "tests".to_string()]);



    let mut modules = HashMap::new();
    if std::path::Path::new("./src").exists() {
        loader::load(&mut modules, &project_name, &Path::new("./src"));
    }
    if std::path::Path::new("./tests").exists() {
        loader::load(&mut modules, &project_tests_name, &Path::new("./tests"));
    }


    if let Some(deps) = &project.dependencies {
        for (name, dep) in deps {
            match dep {
                toml::Value::String(v) => {
                    getdep(name, &mut modules);
                },
                _ => (),
            }
        }
    }


    let mut names : Vec<Name> = modules.keys().cloned().collect();
    names.sort_unstable();
    for name in &names {
        let mut md = modules.remove(name).unwrap();
        match &mut md {
            loader::Module::C(_) => (),
            loader::Module::ZZ(ast) => {
                abs::abs(ast, &modules);
            }
        }
        modules.insert(name.clone(), md);
    }

    let mut flat = Vec::new();
    for name in &names {
        let mut md = modules.remove(name).unwrap();
        match &mut md {
            loader::Module::C(_) => (),
            loader::Module::ZZ(ast) => {
                flat.push(flatten::flatten(ast, &modules));
            }
        }
        modules.insert(name.clone(), md);
    }

    let pb = Arc::new(Mutex::new(pbr::ProgressBar::new(flat.len() as u64)));
    pb.lock().unwrap().show_speed = false;
    let cfiles : HashMap<Name, emitter::CFile> = flat.into_par_iter().map(|mut module|{
        lifetimes::check(&mut module);
        pb.lock().unwrap().message(&format!("emitting {} ", module.name));
        let header  = emitter::Emitter::new(&project.project, module.clone(), true);
        let header  = header.emit();

        let em = emitter::Emitter::new(&project.project, module, false);
        let cf = em.emit();

        pb.lock().unwrap().inc();
        (cf.name.clone(), cf)
    }).collect();

    pb.lock().unwrap().finish_print("done emitting");

    for artifact in std::mem::replace(&mut project.artifacts, None).expect("no artifacts") {
        if let project::ArtifactType::Test = artifact.typ {
            if !tests {
                continue;
            }
        }
        let mut make = make::Make::new(project.project.clone(), artifact.clone());

        let mut main = Name::from(&artifact.main);
        if !main.is_absolute() {
            main.0.insert(0,String::new());
        }
        let main = cfiles.get(&main).expect(&format!(
                "cannot build artifact '{}', main module '{}' does not exist", artifact.name, main));

        let mut need = Vec::new();
        need.push(main.name.clone());
        let mut used = HashSet::new();

        while need.len() > 0 {
            for n in std::mem::replace(&mut need, Vec::new()) {
                if !used.insert(n.clone()) {
                    continue
                }
                let n = cfiles.get(&n).unwrap();
                for d in &n.deps {
                    need.push(d.clone());
                }
                make.build(n);
            }
        }

        if let project::ArtifactType::Lib = artifact.typ {
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

        if !check {
            make.link();
        }

    };
}

fn getdep(name: &str, modules: &mut HashMap<Name, loader::Module>) {

    let mut searchpaths = Vec::new();
    searchpaths.push(std::env::current_exe().expect("self path")
        .canonicalize().expect("self path")
        .parent().expect("self path")
        .parent().expect("self path")
        .parent().expect("self path")
        .join("modules"));


    let mut found = None;
    for searchpath in &searchpaths {
        let modpath = searchpath.join(name).join("zz.toml");;
        if modpath.exists() {
            found = Some(searchpath.join(name));
        }
    }

    let found = match found {
        Some(v) => v,
        None => {
            eprintln!("dependency \"{}\" not found in any of {:#?}", name, searchpaths);
            std::process::exit(9);
        }
    };

    let pp = std::env::current_dir().unwrap();
    //std::env::set_current_dir(&found).unwrap();
    let (_root, project)  = project::load(&found);
    let project_name     = Name(vec![String::new(), project.project.name.clone()]);
    if found.join("./src").exists() {
        loader::load(modules, &project_name, &found.join("./src"));
    }
    //std::env::set_current_dir(pp).unwrap();

}




