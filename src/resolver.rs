use super::ast;
use super::parser;
use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;
use super::project::Config;

#[derive(Default)]
struct Resolver<'a> {
    modules: HashMap<String, ast::Module<'a>>,
}


pub fn to_absolute_namespace(
    project_name:               &String,
    importer_module_source:     &Path,
    importer_module_namespace:  &Vec<String>,
    artifact_namespace:         &Vec<String>,
    search:                     Vec<String>,
)-> Option<(Vec<String>, PathBuf)> {
    debug!("to absolute {:?} ", search);

    let path = search.join("/");
    let p = importer_module_source.parent().unwrap().join(&path).with_extension("zz");
    if p.exists() {
        let mut ns = importer_module_namespace.clone();
        ns.pop();
        ns.extend(search);
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

pub fn resolve<'a>(
    project: &Config,
    artifact_namespace: &Vec<String>,
    main: &Path
) -> HashMap<String, ast::Module<'a>> {

    let mut r = Resolver::default();
    let md = parser::parse(artifact_namespace.clone(), &main);
    r.modules.insert(md.namespace.join("::"), md);

    loop {
        let mut is_dirty = false;
        for name in r.modules.keys().cloned().collect::<Vec<String>>().into_iter() {
            let mut module = r.modules.remove(&name).unwrap();
            let imports = std::mem::replace(&mut module.imports, Vec::new());
            let imports = imports.into_iter().filter_map(|mut imp|{

                // is a c system include
                if let Some("c") = imp.namespace.first().map(|s|s.as_str()) {
                    debug!("resolved import {} as c system include", imp.namespace.join("::"));
                    return Some(imp);
                }

                // make search path absolute
                let mut search = imp.namespace.clone();
                search.pop();
                let (search, path) = match to_absolute_namespace(
                    &project.project.name,
                    &module.source,
                    &module.namespace,
                    artifact_namespace,
                    search,
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
                                vis: imp.vis.clone(),
                                loc: imp.loc.clone(),
                            });
                            module.sources.extend(vec![path.canonicalize().unwrap()]);
                            return None;
                        }

                        // really not found
                        let e = pest::error::Error::<parser::Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                            message: format!("cannot find module"),
                        }, imp.loc.span.clone());
                        error!("{} : {}", imp.loc.file, e);
                        std::process::exit(9);
                    }
                };


                // change import to absolute
                let importname = imp.namespace.pop().unwrap();
                imp.namespace = search.clone();
                imp.namespace.push(importname);

                // already cached
                if let Some(m3) = &r.modules.get(&search.join("::")) {
                    debug!("resolved import {} as module {}", imp.namespace.join("::"), m3.namespace.join("::"));
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

                debug!("resolved import {} as new module {}", imp.namespace.join("::"), m.namespace.join("::"));
                is_dirty = true;
                let ns = m.namespace.join("::");
                if r.modules.insert(ns.clone(), m).is_some() {
                    error!("bug : loaded module {} was already inserted",ns);
                    std::process::exit(9);
                }
                Some(imp)


                /*





                let mut path = imp.namespace.clone();
                path.pop();
                let path = path.join("/");

                let mut parent = search.clone();
                parent.pop();

                let mut n2 = module.source.parent().unwrap().join(&path).with_extension("zz");
                if !n2.exists() && imp.namespace[0] == project.project.name  {

                    let mut path = imp.namespace.clone();
                    path.remove(0);
                    path.pop();
                    let path = path.join("/");

                    let nn2 = Path::new("./src").join(&path).with_extension("zz");
                    if nn2.exists() {
                        search = imp.namespace.clone();
                        search.pop();
                        parent = vec![project.project.name.clone()];
                        n2 = nn2;
                    }
                }
                if n2.exists() {
                    let m = parser::parse(parent.clone(), &n2);
                    assert!(m.namespace == search , "{:?} != {:?}", m.namespace, search);
                    debug!("resolved import {} as new module {}", imp.namespace.join("::"), m.namespace.join("::"));
                    is_dirty = true;
                    let ns = m.namespace.join("::");
                    if r.modules.insert(ns.clone(), m).is_some() {
                        error!("bug : loaded module {} was already inserted",ns);
                        std::process::exit(9);
                    }
                    Some(imp)
                } else {
                    n2 = Path::new("./src").join(&path).with_extension("h");
                    if n2.exists() {
                        debug!("resolved import {} as c file {:?}", imp.namespace.join("::"), n2);
                        module.includes.push(ast::Include{
                            expr: format!("{:?}", n2.canonicalize().unwrap()),
                            vis: imp.vis.clone(),
                            loc: imp.loc.clone(),
                        });
                        module.sources.extend(vec![n2.clone()]);
                    } else {
                        let e = pest::error::Error::<parser::Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                            message: format!("cannot find module"),
                        }, imp.loc.span.clone());
                        error!("{} : {}", imp.loc.file, e);
                        std::process::exit(9);
                    }
                    None
                }

                */
            });
            module.imports = imports.collect();
            r.modules.insert(name, module);
        }
        if !is_dirty {
            break;
        }
    }


    /*
    for name in r.modules.keys().cloned().collect::<Vec<String>>().into_iter() {
        let mut module = r.modules.remove(&name).unwrap();
        for mp in &module.imports {
            if let Some("c") = mp.namespace.first().map(|s|s.as_str()) {
                continue;
            }
            let mut search = namespace.clone();
            search.extend(mp.namespace.iter().cloned());
            search.pop();

            let m2 = &r.modules[&search.join("::")];
            module.sources.extend(m2.sources.clone());
        }
        r.modules.insert(name, module);
    }
    */

    r.modules
}
