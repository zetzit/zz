use serde::{Serialize, Deserialize};
use std::fs::File;
use std::io::{Read, Write};
use std::path::PathBuf;
use std::collections::HashMap;
use toml::Value;


#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ArtifactType {
    Lib,
    Exe,
    Test,
    Header,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct Artifact {
    #[serde(rename = "type")]
    pub name:   String,
    pub main:   String,
    pub typ:    ArtifactType,
}

#[derive(Clone, Default, Serialize, Deserialize)]
pub struct Project {
    pub version:    String,
    pub name:       String,
    pub std:        Option<String>,
    pub cincludes:  Option<Vec<String>>,
    pub cobjects:   Option<Vec<String>>,
    pub pkgconfig:  Option<Vec<String>>,
    pub cflags:     Option<Vec<String>>,
    pub lflags:     Option<Vec<String>>,
}

#[derive(Serialize, Deserialize)]
pub enum Dependency {
    V(String)
}


#[derive(Serialize, Deserialize)]
pub struct Config {
    pub project:        Project,
    pub artifacts:      Option<Vec<Artifact>>,
    pub dependencies:   Option<HashMap<String, Value>>,
}

pub fn init() {
    let c  = Config {
        artifacts: None,
        project: Project {
            name: std::env::current_dir().unwrap().file_name().unwrap().to_string_lossy().into(),
            version: "0.1.0".to_string(),
            ..Default::default()
        },
        dependencies: Some(HashMap::new()),
    };

    if !std::env::current_dir().unwrap().join("zz.toml").exists() {
        let s = toml::to_string(&c).unwrap();
        let mut f = File::create("zz.toml").unwrap();
        f.write_all(s.as_bytes()).unwrap();
    }

    std::fs::create_dir_all("./src/").expect("create src dir");
    if !std::env::current_dir().unwrap().join("src/main.zz").exists() {
        let mut f = File::create("./src/main.zz").unwrap();
        write!(f, "\
using <stdio.h>::{{printf}};

export fn main() -> int {{
    printf(\"hello {}\\n\");
    return 0;
}}
", c.project.name).unwrap();
    }

    println!("project '{}' created", c.project.name);
}

pub fn load_cwd() -> (PathBuf, Config) {
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
    load(&search)
}

pub fn load(search: &std::path::Path) -> (PathBuf, Config) {

    let mut f = File::open(&search.join("zz.toml")).expect(&format!("cannot open {:?}", search));
    let mut s = String::new();
    f.read_to_string(&mut s).expect(&format!("cannot read {:?}", search));
    let mut c : Config = toml::from_str(&mut s).expect(&format!("cannot read {:?}", search));


    if c.artifacts.is_none() {
        let mut a = Vec::new();
        if search.join("./src/main.zz").exists() {
            a.push(Artifact{
                name: c.project.name.clone(),
                main: format!("{}::main", c.project.name),
                typ:  ArtifactType::Exe,
            });
        }

        if search.join("./src/lib.zz").exists() {
            a.push(Artifact{
                name: c.project.name.clone(),
                main: format!("{}", c.project.name),
                typ:  ArtifactType::Lib,
            });
        }
        c.artifacts = Some(a);
    }

    if let Ok(dd) = std::fs::read_dir("./tests/") {
        for entry in dd {
            let entry = entry.unwrap();
            let path  = entry.path();
            if path.is_file() {
                if let Some("zz") = path.extension().map(|v|v.to_str().expect("invalid file name")) {
                    c.artifacts.as_mut().unwrap().push(Artifact{
                        name: format!("tests::{}", path.file_stem().unwrap().to_string_lossy()),
                        typ:  ArtifactType::Test,
                        main: format!("{}::tests::{}",
                                      c.project.name.clone(),
                                      path.file_stem().unwrap().to_string_lossy()),
                    });
                }
            }
        }
    }

    (search.into(), c)
}
