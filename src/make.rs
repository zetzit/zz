use super::project::{Artifact, Config};
use crate::emitter_js;
use crate::emitter_py;
use crate::emitter_go;
use crate::emitter_rs;
use crate::export_make;
use crate::export_cmake;
use crate::export_esp;
use crate::mergecc;
use metrohash::MetroHash128;
use pbr;
use std::collections::HashSet;
use std::hash::Hasher;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::sync::atomic::{AtomicBool, Ordering};

static ABORT: AtomicBool = AtomicBool::new(false);
pub static BUILD_RS: AtomicBool = AtomicBool::new(false);

#[cfg(windows)]
pub static EXE_EXT: &'static str = ".exe";
#[cfg(not(windows))]
pub static EXE_EXT: &'static str = "";

#[derive(Clone)]
pub struct Stage {
    pub name: String,
    pub debug: bool,
    pub optimize: Option<String>,
    pub lto: bool,
    pub asan: bool,
    pub fuzz: bool,
    pub pic: bool,
}

impl Stage {
    pub fn release() -> Self {
        Stage {
            name: "release".to_string(),
            debug: false,
            optimize: Some("03".to_string()),
            lto: true,
            asan: false,
            fuzz: false,
            pic: !cfg!(windows),
        }
    }
    pub fn test() -> Self {
        let asan = if let Ok(v) = std::env::var("ZZ_ASAN") {
            v.parse().unwrap()
        } else {
            true
        };

        Stage {
            name: "test".to_string(),
            debug: true,
            optimize: None,
            lto: false,
            asan,
            fuzz: false,
            pic: !cfg!(windows),
        }
    }
    pub fn debug() -> Self {
        Stage {
            name: "debug".to_string(),
            debug: true,
            optimize: Some("03".to_string()),
            lto: false,
            asan: false,
            fuzz: false,
            pic: !cfg!(windows),
        }
    }
    pub fn fuzz() -> Self {
        let asan = if let Ok(v) = std::env::var("ZZ_ASAN") {
            v.parse().unwrap()
        } else {
            true
        };

        Stage {
            name: "fuzz".to_string(),
            debug: true,
            optimize: None,
            lto: false,
            asan,
            fuzz: true,
            pic: !cfg!(windows),
        }
    }
}

impl std::fmt::Display for Stage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

pub struct Step {
    pub source: PathBuf,
    pub cxx: bool,
    pub args: Vec<String>,

    pub deps: HashSet<PathBuf>,
    pub outp: String,

    pub cflags: Vec<String>,
    pub lflags: Vec<String>,
}

pub struct Make {
    pub artifact: Artifact,
    pub steps: Vec<Step>,
    pub host_cc: String,
    pub cc: String,
    pub host_cxx: String,
    pub cxx: String,
    pub ar: String,
    pub cflags: Vec<String>,
    pub cincludes: Vec<String>,
    pub lflags: Vec<String>,
    pub lobjs: Vec<String>,
    pub variant: String,
    pub stage: Stage,
}

impl Make {
    pub fn new(mut config: Config, variant: &str, stage: Stage, artifact: Artifact) -> Self {
        let mut cflags: Vec<String> =
            match std::env::var("TARGET_CFLAGS").or(std::env::var("CFLAGS")) {
                Err(_) => Vec::new(),
                Ok(s) => s.split(" ").map(|s| s.to_string()).collect(),
            };

        let mut lflags: Vec<String> = match std::env::var("TARGET_LDFLAGS")
            .or(std::env::var("TARGET_LFLAGS"))
            .or(std::env::var("LDFLAGS"))
            .or(std::env::var("LFLAGS"))
        {
            Err(_) => Vec::new(),
            Ok(s) => s.split(" ").map(|s| s.to_string()).collect(),
        };

        let mut defaultcc = "gcc".to_string();
        if which::which("clang").is_ok() {
            defaultcc = "clang".to_string();
        }

        let mut defaultcxx = "g++".to_string();
        if which::which("clang++").is_ok() {
            defaultcxx = "clang++".to_string();
        }

        let host_cc = std::env::var("CC").unwrap_or(defaultcc.clone());

        let mut cc = std::env::var("TARGET_CC")
            .or(std::env::var("CC"))
            .unwrap_or(defaultcc.clone());

        let host_cxx = std::env::var("CXX").unwrap_or("clang++".to_string());

        let mut cxx = std::env::var("TARGET_CXX")
            .or(std::env::var("CXX"))
            .unwrap_or(defaultcxx);

        if let Some(std) = config.project.std {
            cflags.push(format!("-std={}", std));
        }
        let ar = std::env::var("TARGET_AR")
            .or(std::env::var("AR"))
            .unwrap_or("ar".to_string());

        if stage.fuzz {
            cc = "afl-clang".to_string();
            cxx = "afl-clang++".to_string();
        }

        let cincludes = config.project.cincludes.clone();
        let pkgconfig = config.project.pkgconfig.clone();
        let cobjects = std::mem::replace(&mut config.project.cobjects, Vec::new());
        let user_cflags = config.project.cflags.clone();

        for cinc in &cincludes {
            cflags.push("-I".into());
            cflags.push(cinc.clone());
        }

        for pkg in &pkgconfig {
            let flags = Command::new("pkg-config")
                .arg("--cflags")
                .arg(pkg)
                .output()
                .expect(&format!("failed to execute pkg-config --cflags {}", pkg));

            let flags = String::from_utf8_lossy(&flags.stdout);
            let flags = flags.split_whitespace();
            for flag in flags {
                cflags.push(flag.to_string());
            }

            let flags = Command::new("pkg-config")
                .arg("--libs")
                .arg(pkg)
                .output()
                .expect(&format!("failed to execute pkg-config --lflags {}", pkg));

            let flags = String::from_utf8_lossy(&flags.stdout);
            let flags = flags.split_whitespace();
            for flag in flags {
                lflags.push(flag.to_string());
            }
        }
        cflags.push("-I".into());
        cflags.push(".".into());
        cflags.push("-I".into());
        cflags.push("-fvisibility=hidden".to_string());

        cflags.extend(user_cflags);

        let mut stage = stage.clone();
        if let Ok(_) = std::env::var("ZZ_BUILD_NO_PIC") {
            stage.pic = false;
        }

        let mut m = Make {
            variant: variant.to_string(),
            stage,
            cc,
            ar,
            artifact,
            //project,
            lflags,
            lobjs: Vec::new(),
            cflags,
            steps: Vec::new(),
            host_cc,
            cxx,
            host_cxx,
            cincludes,
        };

        for c in cobjects {
            m.cobject(Path::new(&c));
        }

        m
    }

    pub fn cobject(&mut self, inp: &Path) {

        let outbase = super::project::target_dir()
            .join("c");
        let merged = mergecc::mergecc(&self.cincludes, &outbase, inp);




        let mut args = self.cflags.clone();

        if self.stage.pic {
            args.push("-fPIC".into());
        }
        if let Some(opt) = &self.stage.optimize {
            args.push(format!("-O{}", opt).into());
        }

        //TODO
        //match &self.artifact.typ {
        //    super::project::ArtifactType::Staticlib |
        //    _ => {
        if self.stage.lto {
            args.push("-flto".into());
        }
        //    }
        //}

        if self.stage.debug {
            args.push("-g".into());
            args.push("-fstack-protector-strong".into());
        }

        if self.stage.asan {
            args.push("-fsanitize=address".into());
            args.push("-fsanitize=undefined".into());
        }

        if self.stage.fuzz {
            args.push("-m32".into());
            args.push("-fno-sanitize=integer".into());
        }

        args.push("-c".to_string());
        args.push(merged.to_string_lossy().to_string());
        args.push("-o".to_string());

        let mut hasher: MetroHash128 = MetroHash128::default();
        hasher.write(args.join(" ").as_bytes());
        let hash = hasher.finish128();

        let outp = merged
            .to_string_lossy()
            .replace(|c: char| !c.is_alphanumeric(), "_");
        let outp = format!("{}_{:x}{:x}", outp, hash.0, hash.1);
        let outp = super::project::target_dir()
            .join(self.stage.to_string())
            .join("c")
            .join(outp + ".o")
            .to_string_lossy().to_string();


        args.push(outp.clone());

        let mut sources = HashSet::new();
        sources.insert(inp.into());
        sources.insert(merged.clone());

        let cxx = if let Some("cpp") = merged
            .extension()
            .map(|v| v.to_str().expect("invalid file name"))
        {
            true
        } else {
            false
        };

        self.steps.push(Step {
            cxx,
            source: merged.into(),
            args,
            deps: sources,
            outp: outp.clone(),
            lflags : Vec::new(),
            cflags : Vec::new(),
        });

        self.lobjs.push(outp);
    }


    pub fn getflags(&self, cf: &mut super::emitter::CFile) {
        cf.lflags.clear();

        let prk = Command::new(&self.cc)
            .arg("-E")
            .arg(&cf.filepath)
            .output()
            .expect("failed to execute cc -E");
        for line in String::from_utf8_lossy(&prk.stdout).lines() {
            if line.starts_with("#pragma comment") {
                let mut line = line.split(")");
                let line = match line.next() {
                    None => continue,
                    Some(l) => l,
                };
                let mut line = line.split("(");
                match line.next() {
                    None => continue,
                    Some(l) => l,
                };
                let     line = match line.next() {
                    None => continue,
                    Some(l) => l,
                };
                println!(">>{}<<", line);
                let mut line = line.split(",");
                let     key = match line.next() {
                    None => continue,
                    Some(l) => l,
                };
                let mut val = match line.next() {
                    None => continue,
                    Some(l) => l.trim().to_string(),
                };
                if val.starts_with("\"") {
                    val.remove(0);
                }
                if val.ends_with("\"") {
                    val.remove(val.len() - 1);
                }
                if key == "linker" {
                    println!(">>{}<<", val);
                    cf.lflags.push(val.to_string());
                }
            }
        }
    }

    pub fn build(&mut self, cf: &super::emitter::CFile) {
        let mut args = self.cflags.clone();

        args.push("-fomit-frame-pointer".into());
        args.push("-fno-exceptions".into());
        args.push("-fno-asynchronous-unwind-tables".into());
        args.push("-fno-unwind-tables".into());

        args.push("-Werror=implicit-function-declaration".to_string());
        args.push("-Werror=incompatible-pointer-types".to_string());
        args.push("-Werror=return-type".to_string());
        args.push("-Wpedantic".to_string());
        args.push("-Wall".to_string());
        args.push("-Wno-unused-function".to_string());
        args.push("-Wno-parentheses-equality".to_string());

        // should translate tails to something not else. nested flexible arrays are not standard
        args.push("-Wno-flexible-array-extensions".to_string());
        args.push("-Wno-gnu-variable-sized-type-not-at-end".to_string());

        args.push("-Werror=pointer-sign".to_string());
        args.push("-Werror=int-to-pointer-cast".to_string());

        if self.stage.pic {
            args.push("-fPIC".into());
        }
        if let Some(opt) = &self.stage.optimize {
            args.push(format!("-O{}", opt).into());
        }
        if self.stage.lto {
            args.push("-flto".into());
        }
        if self.stage.debug {
            args.push("-g".into());
            args.push("-fstack-protector-strong".into());
        }

        if self.stage.asan {
            args.push("-fsanitize=address".into());
            args.push("-fsanitize=undefined".into());
        }

        if self.stage.fuzz {
            args.push("-m32".into());
            args.push("-fno-sanitize=integer".into());
        }

        args.push("-c".to_string());
        args.push(cf.filepath.clone());
        args.push("-o".to_string());

        let mut b = args.join(" ").as_bytes().to_vec();
        b.extend(self.cc.as_bytes());

        let mut hasher: MetroHash128 = MetroHash128::default();
        hasher.write(&b);
        let hash = hasher.finish128();

        let outp = super::project::target_dir()
            .join(self.stage.to_string())
            .join("zz")
            .join(format!("{}_{:x}{:x}.o", cf.name.0.join("_"), hash.0, hash.1))
            .to_string_lossy().to_string()
        ;
        args.push(outp.clone());

        self.lflags.append(&mut cf.lflags.clone());
        self.cflags.append(&mut cf.cflags.clone());

        self.steps.push(Step {
            cxx: false,
            source: Path::new(&cf.filepath).into(),
            args,
            deps:   cf.sources.clone(),
            outp:   outp.clone(),
            lflags: cf.lflags.clone(),
            cflags: cf.cflags.clone(),
        });
        self.lobjs.push(outp);
    }

    pub fn link(mut self) {
        use rayon::prelude::*;
        use std::sync::{Arc, Mutex};

        let td = super::project::target_dir();

        let needs_objects = match self.artifact.typ {
            super::project::ArtifactType::Exe
            | super::project::ArtifactType::Test
            | super::project::ArtifactType::Macro
            | super::project::ArtifactType::Lib => true,
            | super::project::ArtifactType::Staticlib => true,
            _ => false,
        };

        let has_used_cxx = AtomicBool::new(false);
        let pb = Arc::new(Mutex::new(pbr::ProgressBar::new(self.steps.len() as u64)));
        pb.lock().unwrap().show_speed = false;

        if needs_objects {
            let cc  = self.cc.clone();
            let cxx = self.cxx.clone();

            for step in &self.steps {
                self.lflags.append(&mut step.lflags.clone());
            }


            self.steps.par_iter_mut().for_each(|step| {
                if ABORT.load(Ordering::Relaxed) {
                    return;
                };
                let mut cmd = cc.clone();
                if step.cxx {
                    cmd = cxx.clone();
                    has_used_cxx.store(true, Ordering::Relaxed);
                }

                pb.lock()
                    .unwrap()
                    .message(&format!("{} {:?} ", cmd, step.source));


                if step.is_dirty() {
                    debug!("{} {:?}", cmd, step.args);
                    let status = Command::new(&cmd)
                        .env("AFL_USE_ASAN", "1")
                        .args(&step.args)
                        .status()
                        .expect("failed to execute cc");
                    if !status.success() {
                        error!("cc: [{}] args: [{}]", cmd, step.args.join(" "));
                        ABORT.store(true, Ordering::Relaxed);
                    }
                }
                pb.lock().unwrap().inc();
            });

            if ABORT.load(Ordering::Relaxed) {
                pb.lock().unwrap().finish_print(&format!(
                    "failed [{:?}] {}",
                    self.artifact.typ, self.artifact.name
                ));
                std::process::exit(11);
            }
        }

        let mut cmd = if has_used_cxx.load(Ordering::Relaxed) {
            self.cxx.clone()
        } else {
            self.cc.clone()
        };
        let mut args = Vec::new();

        if self.stage.debug {
            args.push("-g".into());
            args.push("-fstack-protector-strong".into());
        }

        if self.stage.asan {
            args.push("-fsanitize=address".into());
            args.push("-fsanitize=undefined".into());
        }
        if self.stage.fuzz {
            args.push("-m32".into());
            args.push("-fno-sanitize=integer".into());
        }


        if self.stage.lto {
            args.push("-flto".into());
        }

        match self.artifact.typ {
            super::project::ArtifactType::Python => {
                emitter_py::make_module(&self);
                return;
            }
            super::project::ArtifactType::Go=> {
                emitter_go::make_module(&self);
                return;
            }
            super::project::ArtifactType::NodeModule => {
                emitter_js::make_npm_module(&self);
                return;
            }
            super::project::ArtifactType::Rust => {
                emitter_rs::make_module(&self);
                return;
            }
            super::project::ArtifactType::CMake => {
                export_cmake::export(self);
                return;
            }
            super::project::ArtifactType::Make => {
                export_make::export(self);
                return;
            }
            super::project::ArtifactType::Esp32 => {
                export_esp::export(self);
                return;
            }
            super::project::ArtifactType::Staticlib => {
                std::fs::create_dir_all(td
                    .join(self.stage.to_string())
                    .join("lib"))
                    .expect("create target dir");

                cmd = self.ar.clone();
                args = vec![
                    "rcs".to_string(),
                    td
                        .join(self.stage.to_string())
                        .join("lib")
                        .join(format!("lib{}.a", self.artifact.name))
                        .to_string_lossy().to_string()
                ];
                args.extend_from_slice(&self.lobjs);

                if BUILD_RS.load(Ordering::Relaxed) {
                    println!("\n\ncargo:rustc-link-lib=static={}\n\n", self.artifact.name);
                    println!(
                        "\n\ncargo:rustc-link-search=native={}/target/{}\n\n",
                        std::env::current_dir().unwrap().display(),
                        self.stage
                    );
                }
            }
            super::project::ArtifactType::Lib => {
                args.push("-fvisibility=hidden".into());
                if !has_used_cxx.load(Ordering::Relaxed) {
                    args.push("-fomit-frame-pointer".into());
                    args.push("-fno-exceptions".into());
                    args.push("-fno-asynchronous-unwind-tables".into());
                    args.push("-fno-unwind-tables".into());
                }
                if self.stage.pic {
                    args.push("-fPIC".into());
                }

                std::fs::create_dir_all(td
                    .join(self.stage.to_string())
                    .join("lib"))
                    .expect("create target dir");

                args.extend_from_slice(&self.lobjs);
                args.extend_from_slice(&self.lflags);
                args.push("-shared".into());
                args.push("-o".into());
                args.push(td
                          .join(self.stage.to_string())
                          .join("lib")
                          .join(format!("lib{}.so", self.artifact.name))
                          .to_string_lossy().to_string()
                );
            }
            super::project::ArtifactType::Macro => {
                let dir = td.join("macro").join(&self.artifact.name);

                std::fs::create_dir_all(&dir).expect("create target dir");
                cmd = self.host_cc.clone();
                args.extend_from_slice(&self.lobjs);
                if self.stage.asan {
                    args.push("-fsanitize=address".into());
                    args.push("-fsanitize=undefined".into());
                }
                args.push("-o".into());
                args.push(dir.join(format!("macro{}", EXE_EXT)).to_string_lossy().to_string());
            }
            super::project::ArtifactType::Exe => {
                args.push("-fvisibility=hidden".into());
                if !has_used_cxx.load(Ordering::Relaxed) {
                    args.push("-fomit-frame-pointer".into());
                    args.push("-fno-exceptions".into());
                    args.push("-fno-asynchronous-unwind-tables".into());
                    args.push("-fno-unwind-tables".into());
                }
                if self.stage.pic {
                    args.push("-fPIC".into());
                }

                std::fs::create_dir_all(td.join(self.stage.to_string()).join("bin"))
                    .expect("create target dir");
                args.extend_from_slice(&self.lobjs);
                args.extend_from_slice(&self.lflags);
                args.push("-o".into());
                args.push(td
                    .join(self.stage.to_string())
                    .join("bin")
                    .join(format!("{}{}", self.artifact.name , EXE_EXT))
                    .to_string_lossy().to_string()
                );
            }
            super::project::ArtifactType::Test => {
                if self.stage.pic {
                    args.push("-fPIC".into());
                }
                std::fs::create_dir_all(td.join(self.stage.to_string()).join("bin"))
                    .expect("create target dir");
                args.extend_from_slice(&self.lobjs);
                args.extend_from_slice(&self.lflags);
                args.push("-o".into());
                args.push(td
                    .join(self.stage.to_string())
                    .join("bin")
                    .join(format!("{}{}", self.artifact.name , EXE_EXT))
                    .to_string_lossy().to_string()
                );
            }
        }

        pb.lock().unwrap().message(&format!(
            "ld [{:?}] {} ",
            self.artifact.typ, self.artifact.name
        ));
        pb.lock().unwrap().tick();

        debug!("{:?}", args);

        let status = Command::new(&cmd)
            .env("AFL_USE_ASAN", "1")
            .args(&args)
            .status()
            .expect("failed to execute linker");
        if !status.success() {
            std::process::exit(status.code().unwrap_or(3));
        }

        pb.lock().unwrap().finish_print(&format!(
            "finished [{:?}] {}",
            self.artifact.typ, self.artifact.name
        ));
        println!("");
    }
}

impl Step {
    fn is_dirty(&self) -> bool {
        let itarget = match std::fs::metadata(&self.outp) {
            Ok(v) => v,
            Err(_) => return true,
        };
        let itarget = itarget
            .modified()
            .expect(&format!("cannot stat {}", self.outp));

        for source in &self.deps {
            let isource = std::fs::metadata(source).expect(&format!("cannot stat {:?}", source));

            let isource = isource
                .modified()
                .expect(&format!("cannot stat {:?}", source));

            if isource > itarget {
                return true;
            }
        }
        return false;
    }
}
