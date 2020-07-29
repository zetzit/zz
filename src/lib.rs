#[macro_use]
extern crate pest_derive;
extern crate metrohash;
#[macro_use]
extern crate log;
extern crate askama;
extern crate pbr;
extern crate rayon;

pub mod abs;
pub mod ast;
pub mod emitter;
pub mod emitter_docs;
pub mod emitter_js;
pub mod emitter_py;
pub mod emitter_rs;
pub mod emitter_go;
pub mod emitter_common;
pub mod expand;
pub mod export_make;
pub mod export_cmake;
pub mod export_esp;
pub mod flatten;
pub mod loader;
pub mod make;
pub mod makro;
pub mod name;
pub mod parser;
pub mod pipeline;
pub mod project;
pub mod repos;
pub mod smt;
pub mod symbolic;
pub mod mergecc;

use name::Name;
use std::collections::HashMap;
use std::collections::HashSet;

pub struct Error {
    message: String,
    details: Vec<(ast::Location, String)>,
}

impl Error {
    pub fn new(message: String, details: Vec<(ast::Location, String)>) -> Self {
        Self { message, details }
    }
}

#[derive(PartialEq, Debug)]
pub enum BuildSet {
    Tests,
    Run,
    Check(Option<std::path::PathBuf>),
    All,
    Export,
    Named(String),
}

pub fn build(buildset: BuildSet, variant: &str, stage: make::Stage, _slow: bool) {
    let (root, mut project) = project::load_cwd();
    std::env::set_current_dir(&root).unwrap();
    //
    //
    let mut searchpaths = repos::index(&project);

    let td = project::target_dir();
    std::fs::create_dir_all(td.join(stage.to_string()).join("c"))
        .expect("create target dir");
    std::fs::create_dir_all(td.join(stage.to_string()).join("zz"))
        .expect("create target dir");
    std::fs::create_dir_all(td.join("gen"))
        .expect("create target dir");
    std::fs::create_dir_all(td.join("c"))
        .expect("create target dir");
    std::fs::create_dir_all(td.join("include").join("zz"))
        .expect("create target dir");

    let project_name = Name(vec![String::new(), project.project.name.clone()]);
    let project_tests_name = Name(vec![
        String::new(),
        project.project.name.clone(),
        "tests".to_string(),
    ]);

    let mut modules = HashMap::new();
    if root.join("src").exists() {
        loader::load(
            &mut modules,
            &project.project,
            &project_name,
            &root.join("src"),
            &stage,
        );
    }
    if root.join("tests").exists() {
        loader::load(
            &mut modules,
            &project.project,
            &project_tests_name,
            &root.join("tests").canonicalize().unwrap(),
            &stage,
        );
    }

    searchpaths.insert(
        std::env::current_exe()
            .expect("self path")
            .canonicalize()
            .expect("self path")
            .parent()
            .expect("self path")
            .parent()
            .expect("self path")
            .parent()
            .expect("self path")
            .join("modules"),
    );
    searchpaths.insert(std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("modules"));

    if let Ok(zz_path) = std::env::var("ZZ_MODULE_PATHS") {
        let module_paths = if cfg!(windows) {
            zz_path.split(";")
        } else {
            zz_path.split(":")
        };

        for path in module_paths {
            searchpaths.insert(std::path::Path::new(&path).to_path_buf());
        }
    }

    let mut visited = HashSet::new();
    if let Some(deps) = &project.dependencies {
        for (name, dep) in deps {
            match dep {
                toml::Value::String(_) => {
                    getdep(
                        &mut visited,
                        name,
                        &mut modules,
                        &mut project.project,
                        &mut searchpaths,
                        &stage,
                    );
                }
                _ => (),
            }

            std::env::set_current_dir(&root).unwrap();
        }
    }

    let pipeline = pipeline::Pipeline::new(project, stage, variant.to_string(), modules);
    pipeline.build(buildset);
}

fn getdep(
    visited: &mut HashSet<String>,
    name: &str,
    modules: &mut HashMap<Name, loader::Module>,
    rootproj: &mut project::Project,
    searchpaths: &mut HashSet<std::path::PathBuf>,
    stage: &make::Stage,
) {
    if !visited.insert(name.to_string()) {
        return;
    }

    searchpaths.insert(std::env::current_dir().unwrap().join("modules"));

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
            eprintln!(
                "dependency \"{}\" not found in any of {:#?}",
                name, searchpaths
            );
            std::process::exit(9);
        }
    };

    let pp = std::env::current_dir().unwrap();
    //std::env::set_current_dir(&found).unwrap();
    let (root, project) = project::load(&found);
    let project_name = Name(vec![String::new(), project.project.name.clone()]);
    if found.join("src").exists() {
        loader::load(
            modules,
            &project.project,
            &project_name,
            &found.join("src"),
            &stage,
        );
    }

    std::env::set_current_dir(&root).unwrap();
    let depsearchpaths = repos::index(&project);
    std::env::set_current_dir(pp).unwrap();

    searchpaths.insert(root.join("modules"));
    searchpaths.extend(depsearchpaths);

    for i in project.project.cincludes {
        let ii = root.join(&i);
        let i = std::fs::canonicalize(&ii)
            .expect(&format!("{}: cannot resolve cinclude {:?}", name, ii));
        rootproj.cincludes.push(i.to_string_lossy().into());
    }
    for i in project.project.cobjects {
        let ii = root.join(&i);
        let i = std::fs::canonicalize(&ii)
            .expect(&format!("{}: cannot resolve cobject {:?}", name, ii));
        rootproj.cobjects.push(i.to_string_lossy().into());
    }
    for i in project.project.pkgconfig {
        let ii = root.join(&i);
        let i = std::fs::canonicalize(&ii)
            .expect(&format!("{}: cannot resolve pkgconfig {:?}", name, ii));
        rootproj.pkgconfig.push(i.to_string_lossy().into());
    }
    rootproj.cflags.extend(project.project.cflags);

    if let Some(deps) = &project.dependencies {
        for (name, dep) in deps {
            match dep {
                toml::Value::String(_) => {
                    getdep(visited, name, modules, rootproj, searchpaths, stage);
                }
                _ => (),
            }
        }
    }
}


