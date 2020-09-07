use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Write};
use std::path::PathBuf;
use toml::Value;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ArtifactType {
    Macro,

    #[serde(rename = "lib")]
    Lib,
    #[serde(rename = "staticlib")]
    Staticlib,
    #[serde(rename = "exe")]
    Exe,
    #[serde(rename = "test")]
    Test,

    #[serde(rename = "rust")]
    Rust,
    #[serde(rename = "npm")]
    NodeModule,
    #[serde(rename = "cmake")]
    CMake,
    #[serde(rename = "esp32")]
    Esp32,
    #[serde(rename = "python")]
    Python,
    #[serde(rename = "go")]
    Go,
    #[serde(rename = "make")]
    Make,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct Artifact {
    pub name: String,
    pub main: String,
    #[serde(rename = "type")]
    pub typ: ArtifactType,
    pub indexjs: Option<String>,
    #[serde(default)]
    pub requires: Vec<String>,
}

impl Default for Artifact {
    fn default() -> Self {
        Self {
            name: String::new(),
            main: String::new(),
            typ: ArtifactType::Lib,
            indexjs: None,
            requires: Vec::new(),
        }
    }
}

#[derive(Clone, Default, Serialize, Deserialize)]
pub struct Feature {
    #[serde(default)]
    pub cincludes: Vec<String>,
    #[serde(default)]
    pub cobjects: Vec<String>,
    #[serde(default)]
    pub pkgconfig: Vec<String>,
    #[serde(default)]
    pub cflags: Vec<String>,
}

#[derive(Clone, Default, Serialize, Deserialize)]
pub struct Project {
    pub version: String,
    pub name: String,
    pub std: Option<String>,

    #[serde(default)]
    pub cincludes: Vec<String>,
    #[serde(default)]
    pub cobjects: Vec<String>,
    #[serde(default)]
    pub pkgconfig: Vec<String>,
    #[serde(default)]
    pub cflags: Vec<String>,
}

#[derive(Serialize, Deserialize)]
pub enum Dependency {
    V(String),
}

#[derive(Clone, Serialize, Deserialize)]
pub struct Config {
    pub project: Project,
    pub artifacts: Option<Vec<Artifact>>,

    pub dependencies: Option<HashMap<String, Value>>,

    #[serde(default)]
    pub repos: HashMap<String, String>,
}

pub fn init() {
    let mut dependencies = HashMap::new();
    dependencies.insert("log".into(), "1".into());
    dependencies.insert("mem".into(), "1".into());

    let c = Config {
        artifacts: None,
        project: Project {
            name: std::env::current_dir()
                .unwrap()
                .file_name()
                .unwrap()
                .to_string_lossy()
                .into(),
            version: "0.1.0".to_string(),
            ..Default::default()
        },
        dependencies: Some(dependencies),
        repos: HashMap::new(),
    };

    if !std::env::current_dir().unwrap().join("zz.toml").exists() {
        let s = toml::to_string(&c).unwrap();
        let mut f = File::create("zz.toml").unwrap();
        f.write_all(s.as_bytes()).unwrap();
    }

    std::fs::create_dir_all("./src/").expect("create src dir");
    std::fs::create_dir_all("./tests/").expect("create tests dir");
    if !std::env::current_dir()
        .unwrap()
        .join("src/main.zz")
        .exists()
    {
        let mut f = File::create("./src/main.zz").unwrap();
        write!(
            f,
            "\
using log;

export fn main() int {{
    log::info(\"hello %s\", \"{}\");
    return 0;
}}
",
            c.project.name
        )
        .unwrap();
    }

    if !std::env::current_dir().unwrap().join(".gitignore").exists() {
        let mut f = File::create(".gitignore").unwrap();
        write!(f, "/target\n").unwrap();
        write!(f, ".gdb_history\n").unwrap();
        write!(f, "vgcore.*\n").unwrap();
        write!(f, "**/*.o\n").unwrap();
        write!(f, "**/*.parsecache\n").unwrap();
        write!(f, "**/*.buildcache\n").unwrap();


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
                    error!(
                        "error: could not find \"zz.toml\" in {:?} or any parent directory",
                        std::env::current_dir().unwrap()
                    );
                    std::process::exit(9);
                }
            }
        } else {
            break;
        }
    }
    load(&search)
}

pub fn target_dir() -> PathBuf {
    match std::env::var("ZZ_TARGET_DIR") {
        Ok(val) => {
            PathBuf::from(val).join("target")
        }
        Err(_) => {
            let mut search = std::env::current_dir().unwrap();
            loop {
                if !search.join("zz.toml").exists() {
                    search = match search.parent() {
                        Some(v) => v.into(),
                        None => {
                            error!(
                                "error: could not find \"zz.toml\" in {:?} or any parent directory",
                                std::env::current_dir().unwrap()
                            );
                            std::process::exit(9);
                        }
                    }
                } else {
                    break;
                }
            }
            search.join("target")
        }
    }
}

fn sanitize(s: &mut String) {
    *s = s
        .chars()
        .map(|c| match c {
            'A'..='Z' => c,
            'a'..='z' => c,
            '0'..='9' => c,
            _ => '_',
        })
        .collect()
}

pub fn load(search: &std::path::Path) -> (PathBuf, Config) {
    let mut f = File::open(&search.join("zz.toml")).expect(&format!("cannot open {:?}", search));
    let mut s = String::new();
    f.read_to_string(&mut s)
        .expect(&format!("cannot read {:?}", search));
    let mut c: Config = toml::from_str(&mut s).expect(&format!("cannot read {:?}", search));

    sanitize(&mut c.project.name);

    if let Some(artifacts) = &mut c.artifacts {
        for artifact in artifacts {
            sanitize(&mut artifact.name);
        }
    }

    if c.artifacts.is_none() {
        let mut a = Vec::new();
        if search.join("./src/main.zz").exists() {
            a.push(Artifact {
                name: c.project.name.clone(),
                main: format!("{}::main", c.project.name),
                typ: ArtifactType::Exe,
                ..Default::default()
            });
        }

        if search.join("./src/lib.zz").exists() {
            a.push(Artifact {
                name: c.project.name.clone(),
                main: format!("{}", c.project.name),
                typ: ArtifactType::Lib,
                ..Default::default()
            });
        }
        c.artifacts = Some(a);
    }

    if let Ok(dd) = std::fs::read_dir("./tests/") {
        for entry in dd {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.is_file() {
                if let Some("zz") = path
                    .extension()
                    .map(|v| v.to_str().expect("invalid file name"))
                {
                    c.artifacts.as_mut().unwrap().push(Artifact {
                        name: format!("tests_{}", path.file_stem().unwrap().to_string_lossy()),
                        typ: ArtifactType::Test,
                        main: format!(
                            "{}::tests::{}",
                            c.project.name.clone(),
                            path.file_stem().unwrap().to_string_lossy()
                        ),
                        ..Default::default()
                    });
                }
            }
        }
    }

    (search.into(), c)
}
