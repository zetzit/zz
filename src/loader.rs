use super::ast;
use super::parser;
use std::collections::HashMap;
use std::collections::HashSet;
use std::path::Path;
use std::path::PathBuf;
use super::name::Name;
use super::project::Config;


pub enum Module {
    C(PathBuf),
    ZZ(ast::Module),
}

pub fn load(
    modules:       &mut HashMap<Name, Module>,
    artifact_name: &Name,
    src:          &Path
) {
    for entry in std::fs::read_dir(src).unwrap() {
        let entry = entry.unwrap();
        let path  = entry.path();
        if path.is_file() {
            let ext = path.extension().map(|v|v.to_str()).expect(&format!("invalid file name {:?}", path));
            match ext {
                Some("h") => {
                    let mut name = artifact_name.clone();
                    name.push(path.file_stem().unwrap().to_string_lossy().to_string());
                    modules.insert(name, Module::C(path.into()));
                },
                Some("zz") => {
                    let mut m = parser::parse(&path);
                    m.name = artifact_name.clone();
                    m.name.push(path.file_stem().unwrap().to_string_lossy().to_string());
                    modules.insert(m.name.clone(), Module::ZZ(m));
                },
                _ => {},
            }
        }
    }
}

/*



    loop {
        let mut is_dirty = false;
        for name in r.modules.keys().cloned().collect::<Vec<String>>().into_iter() {
            let mut module = r.modules.remove(&name).unwrap();
            let imports = std::mem::replace(&mut module.imports, Vec::new());
            let imports = imports.into_iter().filter_map(|mut imp|{

                // is a c system include
                if let Some("libc") = imp.name.0.first().map(|s|s.as_str()) {
                    debug!("resolved import {} as c system include", imp.name);
                    return Some(imp);
                }

                let (search, path) = match to_absolute_namespace(
                    &project.project.name,
                    &module.source,
                    &module.name,
                    artifact_namespace,
                    search.clone(),
                ) {
                    Some(v) => v,
                    None => {

                        // is a c header
                        let mut path = imp.namespace.clone();
                        path.pop();
                        let path = path.join("/");
                        let path = module.source.parent().unwrap().join(&path).with_extension("h");
                        if path.exists() {
                            debug!("resolved import {} as c file {:?}", imp.namespace.join("::"), path);
                            module.includes.push(ast::Include{
                                expr: format!("{:?}", path.canonicalize().unwrap()),
                                loc: imp.loc.clone(),
                            });
                            module.sources.extend(vec![path.canonicalize().unwrap()]);
                            return None;
                        }

                        // really not found
                        let e = pest::error::Error::<parser::Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                            message: format!("when imported here"),
                        }, imp.loc.span.clone()).with_path(&imp.loc.file);
                        error!("cannot find module '{}'\n{}\n", search.join("::"), e);
                        std::process::exit(9);
                    }
                };


                // change import to absolute
                imp.namespace = search.clone();
                if let Some(name) = import_one_name {
                    imp.namespace.push(name);
                }

                // already cached
                if let Some(m3) = &r.modules.get(&search.join("::")) {
                    debug!("resolved import {} as module {}", imp.namespace.join("::"), m3.namespace.join("::"));

                    for source in &m3.sources {
                        module.sources.insert(source.clone());
                    }
                    return Some(imp);
                }

                // no self imports
                if search.join("::") == name {
                    let e = pest::error::Error::<parser::Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                        message: format!("cannot import self"),
                    }, imp.loc.span.clone());
                    error!("{} : {}", imp.loc.file, e);
                    std::process::exit(9);
                }


                let mut parent = search.clone();
                parent.pop();
                let m = parser::parse(parent, &path);
                assert!(m.namespace == search , "{:?} != {:?}", m.namespace, search);

                for source in &m.sources {
                    module.sources.insert(source.clone());
                }

                debug!("resolved import {} as new module {}", imp.namespace.join("::"), m.namespace.join("::"));
                is_dirty = true;
                let ns = m.namespace.join("::");
                if r.modules.insert(ns.clone(), m).is_some() {
                    error!("bug : loaded module {} was already inserted",ns);
                    std::process::exit(9);
                }
                Some(imp)

            });
            module.imports = imports.collect();
            r.modules.insert(name, module);
        }
        if !is_dirty {
            break;
        }
    }
    r.modules
}

#[derive(Default)]
struct Resolver {
    modules: HashMap<String, ast::Module>,
}


pub fn to_absolute_namespace(
    project_name:               &Name,
    search:                     &Name,
    importer_module_source:     &Path,
    importer_module_name:       &Name,
    artifact_name:              &Name,
)-> Option<(Name, PathBuf)> {
    debug!("to absolute {:?} ", search);

    let path = search.0.join("/");
    let p = importer_module_source.parent().unwrap().join(&path).with_extension("zz");
    if p.exists() {
        let mut ns = importer_module_name.clone();
        ns.pop();
        ns.0.extend(search.0);
        debug!("   relative to importer => {:?}", ns);
        return Some((ns, p));
    }

    if &search[0] == project_name {
        let mut search = search.clone();
        search.remove(0);
        let path = search.join("/");

        let p = Path::new("./src").join(&path).with_extension("zz");
        if p.exists() {
            let mut ns = vec![project_name.clone()];
            ns.extend(search);
            debug!("   relative to project root => {:?}", ns);
            return Some((ns, p));
        }
    }

    None
}
*/
