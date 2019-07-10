use serde::{Deserialize};
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;


#[derive(Debug, Clone, Deserialize)]
pub enum ArtifactType {
    Lib,
    Exe,
    Test,
    Header,
}

#[derive(Clone, Deserialize)]
pub struct Artifact {
    #[serde(rename = "type")]
    pub name:   String,
    pub main:   String,
    pub typ:    ArtifactType,
}

#[derive(Clone, Deserialize)]
pub struct Project {
    pub name:       String,
    pub cincludes:  Option<Vec<String>>,
    pub cobjects:   Option<Vec<String>>,
    pub cflags:     Option<Vec<String>>,
    pub lflags:     Option<Vec<String>>,
}
#[derive(Deserialize)]
pub struct Config {
    pub project:    Project,
    pub artifacts:  Option<Vec<Artifact>>,
}

pub fn load() -> (PathBuf, Config) {
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
                main: format!("{}::lib", c.project.name),
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

    (search.clone(), c)
}
