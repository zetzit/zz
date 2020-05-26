use super::project;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Clone, Serialize, Deserialize)]
pub struct Repo {
    origin: String,
}

#[derive(Default, Clone, Serialize, Deserialize)]
pub struct Index {
    pub repos: HashMap<String, Repo>,
}

pub fn index(project: &project::Config) -> HashSet<PathBuf> {
    let cachepath = Path::new("target").join("repos").join("index");
    let index = if let Some(index) = cache("zz.toml", &cachepath) {
        index
    } else {
        let mut index = Index::default();
        std::fs::create_dir_all(Path::new("target").join("repos"))
            .expect("cannot create target/repos");
        for (name, surl) in &project.repos {
            let url = match url::Url::parse(surl) {
                Ok(v) => v,
                Err(url::ParseError::RelativeUrlWithoutBase) => {
                    url::Url::parse(&format!("file://{}", surl))
                        .expect(&format!("unable to parse repo url: {}", surl))
                }
                Err(e) => {
                    panic!(format!("unable to parse repo url: {}: {}", surl, e));
                }
            };

            match url.scheme() {
                "file" => {}
                "https" => {
                    panic!(format!(
                        "unsupported scheme in repo url: {}, did you mean git:// ?",
                        surl
                    ));
                }
                "git" | "git+ssh" => {
                    let np = Path::new("target").join("repos").join(name);
                    if np.exists() {
                        std::fs::remove_dir_all(&np).expect(&format!("cannot remove {:?}", np));
                    }
                    std::fs::create_dir_all(&np).expect(&format!("cannot create {:?}", np));

                    println!("downloading {}", surl);
                    Command::new("git")
                        .args(&["clone", "-q", surl, np.to_string_lossy().as_ref()])
                        .status()
                        .expect("failed to execute git");

                    let np_sub = np.join("modules");
                    if !np_sub.exists() {
                        if !np.join("zz.toml").exists() {
                            panic!(
                                "unsupported repo in url: {} : no zz.toml or modules subdir",
                                surl
                            );
                        }

                        let npx = Path::new("target").join("repos").join("___");
                        if npx.exists() {
                            std::fs::remove_dir_all(&npx)
                                .expect(&format!("cannot remove {:?}", npx));
                        }
                        std::fs::create_dir_all(&npx).expect(&format!("cannot create {:?}", npx));
                        std::fs::rename(&np, npx.join(name.clone())).expect(&format!(
                            "cannot move {:?} to {:?}",
                            np,
                            npx.join(name.clone())
                        ));
                    }
                }
                _ => {
                    panic!(format!("unsupported scheme in repo url: {}", surl));
                }
            }
            index.repos.insert(
                name.clone(),
                Repo {
                    origin: surl.clone(),
                },
            );
        }
        let cachefile =
            std::fs::File::create(&cachepath).expect(&format!("cannot create {:?}", cachepath));
        serde_cbor::ser::to_writer(cachefile, &index)
            .expect(&format!("cannot write {:?}", cachepath));
        index
    };

    let mut searchpaths = HashSet::new();
    for (name, repo) in &index.repos {
        let url = match url::Url::parse(&repo.origin) {
            Ok(v) => v,
            Err(url::ParseError::RelativeUrlWithoutBase) => {
                url::Url::parse(&format!("file://{}", repo.origin))
                    .expect(&format!("unable to parse repo url: {}", repo.origin))
            }
            Err(e) => {
                panic!(format!("unable to parse repo url: {}: {}", repo.origin, e));
            }
        };
        match url.scheme() {
            "file" => {
                searchpaths.insert(url.path().into());
                searchpaths.insert(Path::new(url.path()).join("modules"));
            }
            _ => {
                searchpaths.insert(Path::new("target").join("repos").join("___"));
                searchpaths.insert(Path::new("target").join("repos").join(name).join("modules"));
            }
        }
    }
    return searchpaths;
}

pub fn cache(source_file: &str, cache_file: &Path) -> Option<Index> {
    let m1 = match std::fs::metadata(&source_file) {
        Ok(v) => v,
        Err(_) => return None,
    };
    let m1 = m1
        .modified()
        .expect(&format!("cannot stat {:?}", source_file));

    let m2 = match std::fs::metadata(&cache_file) {
        Ok(v) => v,
        Err(_) => return None,
    };
    let m2 = m2
        .modified()
        .expect(&format!("cannot stat {:?}", cache_file));

    if m1 > m2 {
        return None;
    }

    match std::fs::File::open(&cache_file) {
        Ok(f) => match serde_cbor::from_reader(&f) {
            Ok(cf) => {
                return Some(cf);
            }
            Err(_) => {
                std::fs::remove_file(&cache_file)
                    .expect(&format!("cannot remove {:?}", cache_file));
            }
        },
        Err(_) => (),
    };
    return None;
}
