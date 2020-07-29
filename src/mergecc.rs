use std::path::Path;
use std::path::PathBuf;
use std::fs;
use std::io::{self, BufRead, Write};
use std::collections::HashMap;


#[derive(Default)]
struct M {
    outbase:        PathBuf,
    includes:       Vec<String>,
    visited:        HashMap<PathBuf, PathBuf>,
}

pub fn mergecc(includes: &Vec<String>, outbase: &Path, inp: &Path) -> PathBuf {

    let mut m = M {
        includes:   includes.clone(),
        outbase:    outbase.into(),
        ..Default::default()
    };

    m.copy(inp.into())
}


impl M {

fn copy(&mut self, inp: PathBuf) -> PathBuf
{
    if let Some(x) = self.visited.get(&inp) {
        return x.clone();
    }

    let oext = inp.extension().map(|v| v.to_str().unwrap()).unwrap_or("");

    let merged = inp.to_string_lossy()
        .replace(|c: char| !c.is_alphanumeric(), "_");
    let merged = self.outbase.join(merged + "." + oext);

    self.visited.insert(inp.clone(), merged.clone());

    if let Ok(target) = std::fs::metadata(&merged) {
        if let Ok(source) = std::fs::metadata(&inp) {
            let itarget = target.modified().expect(&format!("cannot stat {:?}", target));
            let isource = source.modified().expect(&format!("cannot stat {:?}", source));
            if itarget > isource {
                return merged;
            }
        }
    }

    let mut w = fs::File::create(&merged).expect(&format!("cannot create {:?}", merged));

    let iff = fs::File::open(&inp).expect(&format!("cannot open {:?}", inp));

    'outer: for line in io::BufReader::new(iff).lines() {
        let line = line.unwrap();

        if line.starts_with("#include ") {
            let l : Vec<&str> = line.split(" ").collect();
            if l.len() > 1 {
                let mut l = l[1].trim().to_string();
                if l.starts_with("\"") && l.ends_with("\"") {
                    l.remove(0);
                    l.remove(l.len() - 1);

                    if let Some(p) = inp.parent() {
                        let l = p.join(&l);
                        if l.exists() {
                            let nu = self.copy(l);
                            let nu = nu.file_name().unwrap();
                            w.write(format!("#include {:?}\n", nu).as_bytes()).unwrap();
                            continue 'outer;
                        }
                    }
                    for include in &self.includes {
                        let l = Path::new(include).join(&l);
                        if l.exists() {
                            let nu = self.copy(l);
                            let nu = nu.file_name().unwrap();
                            w.write(format!("#include {:?}\n", nu).as_bytes()).unwrap();
                            continue 'outer;
                        }
                    }
                }
            }
        }

        w.write(line.as_bytes()).unwrap();
        w.write(b"\n").unwrap();
    }

    return merged;
}
}
