#![allow(dead_code)] // dead code checker is broken

#[macro_use] extern crate pest_derive;
extern crate fasthash;
#[macro_use] extern crate log;
extern crate pbr;
extern crate rayon;

pub mod ast;
pub mod parser;
pub mod project;
pub mod make;
pub mod loader;
pub mod flatten;
pub mod emitter;
pub mod emitter_rs;
pub mod abs;
pub mod name;
pub mod pp;
pub mod symbolic;
pub mod smt;
pub mod z3;

use std::path::Path;
use name::Name;
use std::collections::HashSet;
use std::collections::HashMap;
use std::sync::atomic::{Ordering};



pub fn build_rs(variant: &str) {
    if let Err(_) = std::env::var("RUST_LOG") {
        std::env::set_var("RUST_LOG", "info");
    }
    env_logger::builder()
        //.default_format_module_path(false)
        .default_format_timestamp(false)
        .default_format_module_path(false)
        .init();

    make::BUILD_RS.store(true, Ordering::SeqCst);
    build(false, false, variant, make::Stage::release(), false);
}

pub fn build(tests: bool, check: bool, variant: &str, stage: make::Stage, slow: bool) {
    use rayon::prelude::*;
    use std::sync::{Arc, Mutex};

    let (root, mut project) = project::load_cwd();
    std::env::set_current_dir(root).unwrap();

    std::fs::create_dir_all(format!("./target/{}/c/", stage)).expect("create target dir");
    std::fs::create_dir_all(format!("./target/{}/zz/", stage)).expect("create target dir");
    std::fs::create_dir_all(format!("./target/{}/include/", stage)).expect("create target dir");

    let project_name        = Name(vec![String::new(), project.project.name.clone()]);
    let project_tests_name  = Name(vec![String::new(), project.project.name.clone(), "tests".to_string()]);



    let mut modules = HashMap::new();
    if std::path::Path::new("./src").exists() {
        loader::load(&mut modules, &project_name, &Path::new("./src"),
            project.features(variant).into_iter().map(|(n,(e,_))|(n,e)).collect());
    }
    if std::path::Path::new("./tests").exists() {
        loader::load(&mut modules, &project_tests_name, &Path::new("./tests"),
            project.features(variant).into_iter().map(|(n,(e,_))|(n,e)).collect());
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
    let silent = parser::ERRORS_AS_JSON.load(Ordering::SeqCst);


    let iterf =  |mut module|{

        //only do symbolic execution if any source file is newer than the c file


        let (_, outname) = emitter::outname(&project.project, &stage, &module, false);
        if module.is_newer_than(&outname) {
            if !silent {
                pb.lock().unwrap().message(&format!("emitting {} ", module.name));
            }
            let symbolic = symbolic::execute(&mut module);
            let header  = emitter::Emitter::new(&project.project, stage.clone(), module.clone(), true);
            let header  = header.emit();

            let rsbridge = emitter_rs::Emitter::new(&project.project, stage.clone(), module.clone());
            rsbridge.emit();

            let em = emitter::Emitter::new(&project.project, stage.clone(), module, false);
            let cf = em.emit();

            if !silent {
                pb.lock().unwrap().inc();
            }
            (cf.name.clone(), cf)
        } else {
            if !silent {
                pb.lock().unwrap().message(&format!("cached {} ", module.name));
                pb.lock().unwrap().inc();
            }
            let cf = emitter::CFile{
                name:       module.name,
                filepath:   outname,
                sources:    module.sources,
                deps:       module.deps
            };
            (cf.name.clone(), cf)
        }

    };
    let cfiles : HashMap<Name, emitter::CFile> = if slow {
        flat.into_iter().map(iterf).collect()
    } else {
        flat.into_par_iter().map(iterf).collect()
    };

    if !silent {
        pb.lock().unwrap().finish_print("done emitting");
    }

    for artifact in std::mem::replace(&mut project.artifacts, None).expect("no artifacts") {
        if let project::ArtifactType::Test = artifact.typ {
            if !tests {
                continue;
            }
        }
        let mut make = make::Make::new(project.clone(), variant, stage.clone(), artifact.clone());

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

    searchpaths.push(
        std::env::current_dir().unwrap().join("modules")
    );

    searchpaths.push(std::env::current_exe().expect("self path")
        .canonicalize().expect("self path")
        .parent().expect("self path")
        .parent().expect("self path")
        .parent().expect("self path")
        .join("modules"));


    searchpaths.push(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("modules")
    );




    let mut found = None;
    for searchpath in &searchpaths {
        let modpath = searchpath.join(name).join("zz.toml");
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
        loader::load(modules, &project_name, &found.join("./src"),
            project.features("default").into_iter().map(|(n,(e,_))|(n,e)).collect());
    }
    //std::env::set_current_dir(pp).unwrap();

    if let Some(deps) = &project.dependencies {
        for (name, dep) in deps {
            match dep {
                toml::Value::String(v) => {
                    getdep(name, modules);
                },
                _ => (),
            }
        }
    }
}




