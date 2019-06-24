use super::ast;
use super::parser;
use std::collections::HashMap;
use std::path::Path;


#[derive(Default)]
struct Resolver<'a> {
    modules: HashMap<String, ast::Module<'a>>,
}

pub fn resolve<'a>(namespace: &Vec<String>, main: &Path) -> HashMap<String, ast::Module<'a>> {
    let mut r = Resolver::default();
    let md = parser::parse(namespace.clone(), &main);
    r.modules.insert(md.namespace.join("::"), md);

    loop {
        let mut is_dirty = false;
        for name in r.modules.keys().cloned().collect::<Vec<String>>().into_iter() {
            let mut module = r.modules.remove(&name).unwrap();
            let imports = std::mem::replace(&mut module.imports, Vec::new());
            let imports = imports.into_iter().filter_map(|mp|{
                let mut search = namespace.clone();
                search.extend(mp.namespace.iter().cloned());
                search.pop();

                if search.join("::") == name {
                    let e = pest::error::Error::<parser::Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                        message: format!("cannot import self"),
                    }, mp.loc.span.clone());
                    error!("{} : {}", mp.loc.file, e);
                    std::process::exit(9);
                }

                if let Some(m3) = &r.modules.get(&search.join("::")) {
                    debug!("resolved import {} as module {}", mp.namespace.join("::"), m3.namespace.join("::"));
                    return Some(mp);
                }
                if let Some("c") = mp.namespace.first().map(|s|s.as_str()) {
                    debug!("resolved import {} as c system include", mp.namespace.join("::"));
                    return Some(mp);
                }

                let mut path = mp.namespace.clone();
                path.pop();
                let path     = path.join("/");

                let mut n2 = Path::new("./src").join(&path).with_extension("zz");
                if n2.exists() {
                    let mut parent = search.clone();
                    parent.pop();
                    let m = parser::parse(parent.clone(), &n2);
                    assert!(m.namespace == search , "{:?} != {:?}", m.namespace, search);
                    debug!("resolved import {} as new module {}", mp.namespace.join("::"), m.namespace.join("::"));
                    is_dirty = true;
                    let ns = m.namespace.join("::");
                    if r.modules.insert(ns.clone(), m).is_some() {
                        error!("bug : loaded module {} was already inserted",ns);
                        std::process::exit(9);
                    }
                    Some(mp)
                } else {
                    n2 = Path::new("./src").join(&path).with_extension("h");
                    if n2.exists() {
                        debug!("resolved import {} as c file {:?}", mp.namespace.join("::"), n2);
                        module.includes.push(ast::Include{
                            expr: format!("{:?}", n2.canonicalize().unwrap()),
                            vis: mp.vis.clone(),
                            loc: mp.loc.clone(),
                        });
                        module.sources.extend(vec![n2.clone()]);
                    } else {
                        let e = pest::error::Error::<parser::Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                            message: format!("cannot find module"),
                        }, mp.loc.span.clone());
                        error!("{} : {}", mp.loc.file, e);
                        std::process::exit(9);
                    }
                    None
                }
            });
            module.imports = imports.collect();
            r.modules.insert(name, module);
        }
        if !is_dirty {
            break;
        }
    }


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

    r.modules
}
