use super::project;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::io::Write;

#[derive(Clone, Serialize, Deserialize)]
pub struct Repo {
    origin: String,
}

#[derive(Default, Clone, Serialize, Deserialize)]
pub struct Index {
    pub repos: HashMap<String, Repo>,
}

pub fn index(project: &project::Config) -> HashSet<PathBuf> {

    // shortcut so we dont try to write to read only modules such when installed as distro pkg
    if project.repos.len() == 0 {
        return HashSet::new();
    }

    let td = super::project::target_dir();
    let cachepath = td.join("repos").join("index");
    let index = if let Some(index) = cache("zz.toml", &cachepath) {
        index
    } else {
        let mut index = Index::default();
        std::fs::create_dir_all(td.join("repos"))
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
                    let np = td.join("repos").join(name);
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

                        let npx = td.join("repos").join("___").join(name.clone());
                        if npx.exists() {
                            std::fs::remove_dir_all(&npx)
                                .expect(&format!("cannot remove {:?}", npx));
                        }
                        std::fs::create_dir_all(&npx).expect(&format!("cannot create {:?}", npx));
                        std::fs::rename(&np, &npx).expect(&format!(
                            "cannot move {:?} to {:?}",
                            np,
                            npx
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

        let mut cachefile =
            std::fs::File::create(&cachepath).expect(&format!("cannot create {:?}", cachepath));

        cachefile.write(
            &rmp_serde::to_vec(&index).expect(&format!("cannot encode {:?}", cachepath))[..]
        ).expect(&format!("cannot write {:?}", cachepath));

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
                let path = Path::new(url.path()).to_path_buf();
                searchpaths.insert(path.canonicalize().unwrap_or(path));

                let path = Path::new(url.path()).join("modules");
                searchpaths.insert(path.canonicalize().unwrap_or(path));
            }
            _ => {
                let path = td.join("repos").join("___");
                searchpaths.insert(path.canonicalize().unwrap_or(path));

                let path = td.join("repos").join(name).join("modules");
                searchpaths.insert(path.canonicalize().unwrap_or(path));
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
        Ok(f) => match rmp_serde::from_read(&f) {
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
