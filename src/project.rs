use serde::{Deserialize};
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

#[derive(Deserialize)]
pub struct Project {
    pub name:       String,
    pub cincludes:  Option<Vec<String>>,
    pub cobjects:   Option<Vec<String>>,
    pub cflags:     Option<Vec<String>>,
    pub lflags:     Option<Vec<String>>,
}
#[derive(Deserialize)]
pub struct Config {
    pub project:   Project,
}

pub fn load() -> (PathBuf, Project) {
    let mut search = std::env::current_dir().unwrap();
    loop {
        if !search.join("zz.toml").exists() {
            search = match search.parent() {
                Some(v) => v.into(),
                None => {
                    eprintln!("error: could not find \"zz.toml\" in {:?} or any parent directory",
                              std::env::current_dir().unwrap());
                    std::process::exit(9);
                }
            }
        } else {
            break
        }
    }

    let mut f = File::open(&search.join("zz.toml")).expect(&format!("cannot open {:?}", search));
    let mut s = String::new();
    f.read_to_string(&mut s).expect(&format!("cannot read {:?}", search));
    let c : Config = toml::from_str(&mut s).expect(&format!("cannot read {:?}", search));

    (search.clone(), c.project)
}
