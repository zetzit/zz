#![allow(dead_code)] // dead code checker is broken

#[macro_use] extern crate pest_derive;
extern crate metrohash;
#[macro_use] extern crate log;
extern crate pbr;
extern crate rayon;
extern crate askama;

pub mod ast;
pub mod parser;
pub mod project;
pub mod make;
pub mod loader;
pub mod flatten;
pub mod emitter;
pub mod emitter_rs;
pub mod emitter_js;
pub mod abs;
pub mod name;
pub mod pp;
pub mod symbolic;
pub mod expand;
pub mod smt;
pub mod emitter_docs;

use std::path::Path;
use name::Name;
use std::collections::HashSet;
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};


pub struct Error {
    message:    String,
    details:    Vec<(ast::Location, String)>,
}

impl Error {
    pub fn new(message: String, details:    Vec<(ast::Location, String)>) -> Self {
        Self{
            message,
            details,
        }
    }
}

static ABORT: AtomicBool = AtomicBool::new(false);



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
    //std::env::set_current_dir(root).unwrap();

    std::fs::create_dir_all(root.join("target").join(stage.to_string()).join("c")).expect("create target dir");
    std::fs::create_dir_all(root.join("target").join(stage.to_string()).join("zz")).expect("create target dir");
    std::fs::create_dir_all(root.join("target").join(stage.to_string()).join("include")
                            .join("zz").join(&project.project.name)).expect("create target dir");

    let project_name        = Name(vec![String::new(), project.project.name.clone()]);
    let project_tests_name  = Name(vec![String::new(), project.project.name.clone(), "tests".to_string()]);



    let mut modules = HashMap::new();
    let features = project.features(variant).into_iter().map(|(n,(e,_))|(n,e)).collect();
    if root.join("src").exists() {
        loader::load(&mut modules, &project_name, &root.join("src"), &features, &stage);
    }
    if root.join("tests").exists() {
        loader::load(&mut modules, &project_tests_name, &root.join("tests").canonicalize().unwrap(), &features, &stage);
    }



    let mut searchpaths = HashSet::new();
    searchpaths.insert(std::env::current_exe().expect("self path")
        .canonicalize().expect("self path")
        .parent().expect("self path")
        .parent().expect("self path")
        .parent().expect("self path")
        .join("modules"));
    searchpaths.insert(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("modules")
    );


    if let Some(deps) = &project.dependencies {
        for (name, dep) in deps {
            match dep {
                toml::Value::String(_) => {
                    getdep(name, &mut modules, &mut project.project, &mut searchpaths, &stage);
                },
                _ => (),
            }
        }
    }



    let mut ext = abs::Ext::new();

    let mut names : Vec<Name> = modules.keys().cloned().collect();
    names.sort_unstable();
    for name in &names {
        let mut md = modules.remove(name).unwrap();
        match &mut md {
            loader::Module::C(_) => (),
            loader::Module::ZZ(ast) => {
                abs::abs(ast, &modules, &mut ext);
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
                flat.push(flatten::flatten(ast, &modules, &ext));
            }
        }
        modules.insert(name.clone(), md);
    }

    let pb = Arc::new(Mutex::new(pbr::ProgressBar::new(flat.len() as u64)));
    pb.lock().unwrap().show_speed = false;
    let silent = parser::ERRORS_AS_JSON.load(Ordering::SeqCst);



    let working_on_these = Arc::new(Mutex::new(HashSet::new()));


    let iterf =  |mut module| {

        //only emit if any source file is newer than the c file
        let (_, outname) = emitter::outname(&project.project, &stage, &module, false);
        if module.is_newer_than(&outname) {
            let module_human_name = module.name.human_name();
            if !silent {
                working_on_these.lock().unwrap().insert(module_human_name.clone());
                let mut indic = String::new();
                for working_on in  working_on_these.lock().unwrap().iter() {
                    if !indic.is_empty() {
                        indic.push_str(", ");
                    }
                    if indic.len() > 30 {
                        indic = format!("{}.. ", indic);
                        break;
                    }
                    indic = format!("{}{} ", indic, working_on);
                }
                indic = format!("prove [ {}]  ", indic);
                pb.lock().unwrap().message(&indic);
                pb.lock().unwrap().tick();
            }

            expand::expand(&mut module)?;
            if !symbolic::execute(&mut module) {
                ABORT.store(true, Ordering::Relaxed);
                return Ok(None);
            }

            let header  = emitter::Emitter::new(&project.project, stage.clone(), module.clone(), true);
            header.emit();

            let rsbridge = emitter_rs::Emitter::new(&project.project, stage.clone(), module.clone());
            rsbridge.emit();

            let jsbridge = emitter_js::Emitter::new(&project.project, stage.clone(), module.clone());
            jsbridge.emit();

            let docs = emitter_docs::Emitter::new(&project.project, stage.clone(), module.clone());
            docs.emit();

            let em = emitter::Emitter::new(&project.project, stage.clone(), module, false);
            let cf = em.emit();


            if !silent {
                working_on_these.lock().unwrap().remove(&module_human_name);
                let mut indic = String::new();
                for working_on in  working_on_these.lock().unwrap().iter() {
                    if !indic.is_empty() {
                        indic.push_str(", ");
                    }
                    if indic.len() > 30 {
                        indic = format!("{}.. ", indic);
                        break;
                    }
                    indic = format!("{}{} ", indic, working_on);
                }
                indic = format!("prove [ {}]  ", indic);
                pb.lock().unwrap().message(&indic);
                pb.lock().unwrap().inc();
            }
            Ok(Some((cf.name.clone(), cf)))
        } else {
            if !silent {
                //pb.lock().unwrap().message(&format!("cached {} ", module.name));
                pb.lock().unwrap().inc();
            }
            let cf = emitter::CFile{
                name:       module.name,
                filepath:   outname,
                sources:    module.sources,
                deps:       module.deps
            };
            Ok(Some((cf.name.clone(), cf)))
        }

    };
    let cfiles_r : Vec<Result<Option<(Name, emitter::CFile)>, Error>> = if slow {
        flat.into_iter().map(iterf).collect()
    } else {
        flat.into_par_iter().map(iterf).collect()
    };

    let mut cfiles = HashMap::new();
    for r in cfiles_r {
        match r {
            Ok(None) => {},
            Ok(Some(v)) => {
                cfiles.insert(v.0, v.1);
            }
            Err(e) => {
                parser::emit_error(e.message.clone(), &e.details);
                ABORT.store(true, Ordering::Relaxed);
            }
        }
    };



    if ABORT.load(Ordering::Relaxed) {
        std::process::exit(9);
    }

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
                let n = cfiles.get(&n).expect(&format!("ICE: dependency {} module doesnt exist", n));
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

fn getdep(
        name: &str,
        modules: &mut HashMap<Name, loader::Module>,
        rootproj: &mut project::Project,
        searchpaths: &mut HashSet<std::path::PathBuf>,
        stage:  &make::Stage,
) {

    searchpaths.insert(
        std::env::current_dir().unwrap().join("modules")
    );

    let mut found = None;
    for searchpath in searchpaths.iter() {
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

    //let pp = std::env::current_dir().unwrap();
    //std::env::set_current_dir(&found).unwrap();
    let (root, project)  = project::load(&found);
    let project_name     = Name(vec![String::new(), project.project.name.clone()]);        
    if found.join("src").exists() {
        let features = project.features("default").into_iter().map(|(n,(e,_))|(n,e)).collect();
        loader::load(modules, &project_name, &found.join("src"), &features, &stage);
    }
    //std::env::set_current_dir(pp).unwrap();

    searchpaths.insert(
        root.join("modules")
    );


    for i in project.project.cincludes {
        let ii = root.join(&i);
        let i = std::fs::canonicalize(&ii).expect(&format!("{}: cannot resolve cinclude {:?}", name, ii));
        rootproj.cincludes.push(i.to_string_lossy().into());
    }
    for i in project.project.cobjects {
        let ii = root.join(&i);
        let i = std::fs::canonicalize(&ii).expect(&format!("{}: cannot resolve cobject {:?}", name, ii));
        rootproj.cobjects.push(i.to_string_lossy().into());
    }
    rootproj.pkgconfig.extend(project.project.pkgconfig);
    rootproj.cflags.extend(project.project.cflags);
    rootproj.lflags.extend(project.project.lflags);


    if let Some(deps) = &project.dependencies {
        for (name, dep) in deps {
            match dep {
                toml::Value::String(_) => {
                    getdep(name, modules, rootproj, searchpaths, stage);
                },
                _ => (),
            }
        }
    }
}




