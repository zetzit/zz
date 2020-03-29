use serde::{Serialize, Deserialize};
use std::fs::File;
use std::io::{Read, Write};
use std::path::PathBuf;
use std::collections::HashMap;
use toml::Value;


#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ArtifactType {
    #[serde(rename = "lib")]
    Lib,
    #[serde(rename = "staticlib")]
    Staticlib,
    #[serde(rename = "exe")]
    Exe,
    #[serde(rename = "test")]
    Test,
    #[serde(rename = "header")]
    Header,
    #[serde(rename = "npm")]
    NodeModule,
    #[serde(rename = "macro")]
    Macro,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct Artifact {
    pub name:       String,
    pub main:       String,
    #[serde(rename = "type")]
    pub typ:        ArtifactType,
    pub indexjs:    Option<String>,
}


impl Default for Artifact {
    fn default() -> Self {
        Self {
            name:       String::new(),
            main:       String::new(),
            typ:        ArtifactType::Lib,
            indexjs:    None,
        }
    }
}


#[derive(Clone, Default, Serialize, Deserialize)]
pub struct Feature {
    #[serde(default)]
    pub cincludes:  Vec<String>,
    #[serde(default)]
    pub cobjects:   Vec<String>,
    #[serde(default)]
    pub pkgconfig:  Vec<String>,
    #[serde(default)]
    pub cflags:     Vec<String>,
    #[serde(default)]
    pub lflags:     Vec<String>,
}

#[derive(Clone, Default, Serialize, Deserialize)]
pub struct Project {
    pub version:    String,
    pub name:       String,
    pub std:        Option<String>,
    
    #[serde(default)]
    pub cincludes:  Vec<String>,
    #[serde(default)]
    pub cobjects:   Vec<String>,
    #[serde(default)]
    pub pkgconfig:  Vec<String>,
    #[serde(default)]
    pub cflags:     Vec<String>,
    #[serde(default)]
    pub lflags:     Vec<String>,
}

#[derive(Serialize, Deserialize)]
pub enum Dependency {
    V(String)
}


#[derive(Clone, Serialize, Deserialize)]
pub struct Config {
    pub project:        Project,
    pub artifacts:      Option<Vec<Artifact>>,

    pub features:       Option<HashMap<String, Feature>>,
    #[serde(default)]
    pub variants:       HashMap<String, Vec<String>>,

    pub dependencies:   Option<HashMap<String, Value>>,
}

pub fn init() {
    let mut c  = Config {
        artifacts: None,
        project: Project {
            name: std::env::current_dir().unwrap().file_name().unwrap().to_string_lossy().into(),
            version: "0.1.0".to_string(),
            ..Default::default()
        },
        dependencies:   Some(HashMap::new()),
        features:       None,
        variants:       HashMap::new(),
    };
    c.variants.insert("default".to_string(), Vec::new());

    if !std::env::current_dir().unwrap().join("zz.toml").exists() {
        let s = toml::to_string(&c).unwrap();
        let mut f = File::create("zz.toml").unwrap();
        f.write_all(s.as_bytes()).unwrap();
    }

    std::fs::create_dir_all("./src/").expect("create src dir");
    std::fs::create_dir_all("./tests/").expect("create tests dir");
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

    if !std::env::current_dir().unwrap().join(".gitignore").exists() {
        let mut f = File::create(".gitignore").unwrap();
        write!(f, "/target\n").unwrap();
        write!(f, ".gdb_history\n").unwrap();
        write!(f, "vgcore.*\n").unwrap();
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
                    error!("error: could not find \"zz.toml\" in {:?} or any parent directory",
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


    // implicit features
    if !c.variants.contains_key("default") {
        c.variants.insert("default".to_string(), Default::default());
    }
    for (_,features) in &c.variants {
        for f in features {
            if c.features.is_none() {
                c.features = Some(HashMap::new());
            }
            if !c.features.as_mut().unwrap().contains_key(f) {
                c.features.as_mut().unwrap().insert(f.to_string(), Default::default());
            }
        }
    }

    if c.artifacts.is_none() {
        let mut a = Vec::new();
        if search.join("./src/main.zz").exists() {
            a.push(Artifact{
                name: c.project.name.clone(),
                main: format!("{}::main", c.project.name),
                typ:  ArtifactType::Exe,
                ..Default::default()
            });
        }

        if search.join("./src/lib.zz").exists() {
            a.push(Artifact{
                name: c.project.name.clone(),
                main: format!("{}", c.project.name),
                typ:  ArtifactType::Lib,
                ..Default::default()
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
                        name: format!("tests_{}", path.file_stem().unwrap().to_string_lossy()),
                        typ:  ArtifactType::Test,
                        main: format!("{}::tests::{}",
                                      c.project.name.clone(),
                                      path.file_stem().unwrap().to_string_lossy()),
                        ..Default::default()
                    });
                }
            }
        }
    }

    (search.into(), c)
}


impl Config {
    pub fn features(&self, variant: &str) -> HashMap<String, (bool, Feature)> {

        match self.variants.get(variant) {
            None => {
                error!("variant {} not defined", variant);
                std::process::exit(9);
            },
            Some(v) => {
                let mut r = HashMap::new();
                if let Some(features) = &self.features {
                    for (n,feature) in features {
                        r.insert(n.to_string(), (false, feature.clone()));
                    }
                }
                for v in v {
                    r.get_mut(v).unwrap().0 = true;
                }
                r
            }
        }

    }
}
