use super::ast;
use super::make::Stage;
use super::name::Name;
use super::parser;
use pbr;
use rayon::prelude::*;
use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;
use std::sync::atomic::Ordering;
use std::sync::{Arc, Mutex};

#[derive(Clone)]
pub enum Module {
    C(PathBuf),
    ZZ(ast::Module),
}

pub fn load(
    modules: &mut HashMap<Name, Module>,
    project: &super::project::Project,
    artifact_name: &Name,
    src: &Path,
    features: &HashMap<String, bool>,
    stage: &Stage,
) {
    let mut files = Vec::new();
    for entry in std::fs::read_dir(src).expect(&format!("cannot open src directory {:?} ", src)) {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_file() {
            let ext = match path.extension().map(|v| v.to_str()) {
                Some(ext) => ext,
                None => continue,
            };
            match ext {
                Some("h") => {
                    let mut name = artifact_name.clone();
                    name.push(path.file_stem().unwrap().to_string_lossy().to_string());
                    modules.insert(name, Module::C(path.into()));
                }
                Some("zz") => {
                    files.push(path.clone());
                }
                _ => {}
            }
        }
    }

    let pb = Arc::new(Mutex::new(pbr::ProgressBar::new(files.len() as u64)));
    pb.lock().unwrap().show_speed = false;
    let silent = parser::ERRORS_AS_JSON.load(Ordering::SeqCst);

    if !silent {
        pb.lock()
            .unwrap()
            .finish_print(&format!("parsing {}", artifact_name));
    }
    let om: HashMap<Name, Module> = files
        .into_par_iter()
        .map(|path| {
            let mut module_name = artifact_name.clone();
            let stem = path.file_stem().unwrap().to_string_lossy().to_string();
            if stem != "lib" {
                module_name.push(stem);
            }

            let (_, outname) = super::emitter::outname(&project, &stage, &module_name, false);
            let cachepath = format!("{}.parsecache", outname);

            if !is_cache_outdated(&path, &cachepath) {
                match std::fs::File::open(&cachepath) {
                    Ok(f) => {
                        match serde_cbor::from_reader(&f) {
                            Ok(cf) => {
                                if !silent {
                                    //pb.lock().unwrap().message(&format!("cached {} ", module.name));
                                    pb.lock().unwrap().inc();
                                }
                                return (module_name, Module::ZZ(cf));
                            }
                            Err(_) => {
                                std::fs::remove_file(&cachepath)
                                    .expect(&format!("cannot remove {}", cachepath));
                            }
                        }
                    }
                    Err(_) => (),
                };
            }

            if !silent {
                pb.lock().unwrap().message(&format!("parsing {:?} ", path));
            }
            let mut m = parser::parse(&path, features, stage);
            m.name = module_name;

            debug!("loaded {:?} as {}", path, m.name);
            if !silent {
                pb.lock().unwrap().inc();
            }

            let cachefile =
                std::fs::File::create(&cachepath).expect(&format!("cannot create {}", cachepath));
            serde_cbor::ser::to_writer(cachefile, &m)
                .expect(&format!("cannot write {}", cachepath));

            (m.name.clone(), Module::ZZ(m))
        })
        .collect();
    modules.extend(om);
    if !silent {
        pb.lock()
            .unwrap()
            .finish_print(&format!("finished parsing {}", artifact_name));
    }
}

pub fn is_cache_outdated(source_file: &PathBuf, cache_file: &str) -> bool {
    let m1 = match std::fs::metadata(&source_file) {
        Ok(v) => v,
        Err(_) => return true,
    };
    let m1 = m1
        .modified()
        .expect(&format!("cannot stat {:?}", source_file));

    let m2 = match std::fs::metadata(&cache_file) {
        Ok(v) => v,
        Err(_) => return true,
    };
    let m2 = m2.modified().expect(&format!("cannot stat {}", cache_file));

    return m1 > m2;
}
