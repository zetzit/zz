use super::project::{Config, Artifact};
use std::hash::{Hasher};
use metrohash::{MetroHash128};
use std::path::Path;
use std::path::PathBuf;
use std::collections::HashSet;
use std::process::Command;
use pbr;
use std::sync::atomic::{AtomicBool, Ordering};
use super::emitter_js;

static ABORT:           AtomicBool = AtomicBool::new(false);
pub static BUILD_RS:    AtomicBool = AtomicBool::new(false);

#[cfg(windows)]
pub static EXE_EXT : &'static str = ".exe";
#[cfg(not(windows))]
pub static EXE_EXT : &'static str = "";



#[derive(Clone)]
pub struct Stage {
    pub name:       String,
    pub debug:      bool,
    pub optimize:   Option<String>,
    pub lto:        bool,
    pub asan:       bool,
    pub fuzz:       bool,
    pub pic:        bool,
}

impl Stage {
    pub fn release() -> Self {
        Stage {
            name:       "release".to_string(),
            debug:      false,
            optimize:   Some("03".to_string()),
            lto:        true,
            asan:       false,
            fuzz:       false,
            pic:        !cfg!(windows),
        }
    }
    pub fn test() -> Self {
        Stage {
            name:       "test".to_string(),
            debug:      true,
            optimize:   None,
            lto:        false,
            asan:       true,
            fuzz:       false,
            pic:        !cfg!(windows),
        }
    }
    pub fn debug() -> Self {
        Stage {
            name:       "debug".to_string(),
            debug:      true,
            optimize:   Some("03".to_string()),
            lto:        false,
            asan:       false,
            fuzz:       false,
            pic:        !cfg!(windows),
        }
    }
    pub fn fuzz() -> Self {
        Stage {
            name:       "fuzz".to_string(),
            debug:      true,
            optimize:   None,
            lto:        false,
            asan:       true,
            fuzz:       true,
            pic:        !cfg!(windows),
        }
    }
}

impl std::fmt::Display for  Stage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}


pub struct Step {
    pub source: PathBuf,
    pub args:   Vec<String>,

    pub deps:   HashSet<PathBuf>,
    pub outp:   String,
}

pub struct Make {
    pub artifact:   Artifact,
    pub steps:      Vec<Step>,
    pub host_cc:    String,
    pub cc:         String,
    pub ar:         String,
    pub cflags:     Vec<String>,
    pub lflags:     Vec<String>,
    pub lobjs:      Vec<String>,
    pub variant:    String,
    pub stage:      Stage,
}

impl Make {
    pub fn new(mut config: Config, variant: &str, stage: Stage, artifact: Artifact) -> Self {


        let features = config.features(variant);

        let mut cflags : Vec<String> =
            match std::env::var("TARGET_CFLAGS").or(std::env::var("CFLAGS")) {
                Err(_) => Vec::new(),
                Ok(s) => s.split(" ").map(|s|s.to_string()).collect()
            };

        let mut lflags : Vec<String> =
            match std::env::var("TARGET_LDFLAGS")
                    .or(std::env::var("TARGET_LFLAGS"))
                    .or(std::env::var("LDFLAGS"))
                    .or(std::env::var("LFLAGS")) {
                Err(_) => Vec::new(),
                Ok(s) => s.split(" ").map(|s|s.to_string()).collect()
            };

        let host_cc = std::env::var("CC")
            .unwrap_or("clang".to_string());

        let mut cc = std::env::var("TARGET_CC")
            .or(std::env::var("CC"))
            .unwrap_or("clang".to_string());

        let mut cxx = false;
        if let Some(std) = config.project.std {
            cflags.push(format!("-std={}", std));
            if std.contains("c++") {
                cxx = true;
                cc = std::env::var("TARGET_CXX")
                    .or(std::env::var("CXX"))
                    .unwrap_or("clang++".to_string());
            }
        }
        let ar = std::env::var("TARGET_AR")
            .or(std::env::var("AR"))
            .unwrap_or("ar".to_string());


        if stage.fuzz {
            cc = "afl-clang".to_string();
        }


        let mut cincludes   = config.project.cincludes.clone();
        let mut pkgconfig   = config.project.pkgconfig.clone();
        let mut cobjects    = std::mem::replace(&mut config.project.cobjects, Vec::new());
        let mut user_cflags = config.project.cflags.clone();
        let mut user_lflags = config.project.lflags.clone();


        for (_,(enabled,feature)) in &features {
            if !enabled {
                continue;
            }
            cincludes.extend(feature.cincludes.clone());
            pkgconfig.extend(feature.pkgconfig.clone());
            cobjects.extend(feature.cobjects.clone());
            user_cflags.extend(feature.cflags.clone());
            user_lflags.extend(feature.lflags.clone());
        }

        for cinc in cincludes{
            cflags.push("-I".into());
            cflags.push(cinc);
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
        if stage.pic {
            cflags.push("-fPIC".into());
        }
        cflags.push("-I".into());
        cflags.push(".".into());
        cflags.push("-I".into());
        cflags.push("-fvisibility=hidden".to_string());


        if let Some(opt) = &stage.optimize {
            cflags.push(format!("-O{}",opt).into());
            lflags.push("-O3".into());
        }


        //TODO
        match artifact.typ {
            super::project::ArtifactType::Staticlib |
            super::project::ArtifactType::NodeModule => (),
            _ => {
                if stage.lto {
                    cflags.push("-flto".into());
                    lflags.push("-flto".into());
                }
            }
        }


        if stage.debug {
            cflags.push("-g".into());
            lflags.push("-g".into());
            cflags.push("-fstack-protector-strong".into());
        }

        if stage.asan {
            cflags.push("-fsanitize=address".into());
            lflags.push("-fsanitize=address".into());
        }

        if stage.fuzz{
            cflags.push("-m32".into());
            lflags.push("-m32".into());
        }


        if !cxx && !stage.debug {
            cflags.push("-fomit-frame-pointer".into());
            cflags.push("-fno-exceptions".into());
            cflags.push("-fno-asynchronous-unwind-tables".into());
            cflags.push("-fno-unwind-tables".into());
            lflags.push("-fomit-frame-pointer".into());
            lflags.push("-fno-exceptions".into());
            lflags.push("-fno-asynchronous-unwind-tables".into());
            lflags.push("-fno-unwind-tables".into());
        }


        cflags.extend(user_cflags);
        lflags.extend(user_lflags);

        let mut m = Make {
            variant: variant.to_string(),
            stage:   stage.clone(),
            cc,
            ar,
            artifact,
            //project,
            lflags,
            lobjs: Vec::new(),
            cflags,
            steps: Vec::new(),
            host_cc,
        };

        for c in cobjects {
            m.cobject(Path::new(&c));
        }

        m
    }

    pub fn cobject(&mut self, inp: &Path) {

        let mut args = self.cflags.clone();
        args.push("-c".to_string());
        args.push(inp.to_string_lossy().to_string());
        args.push("-o".to_string());

        let mut hasher: MetroHash128 = MetroHash128::default();
        hasher.write(args.join(" ").as_bytes());
        let hash = hasher.finish128();    

        let outp = inp.to_string_lossy().replace(|c: char| !c.is_alphanumeric(), "_");
        let outp = format!("{}_{:x}{:x}", outp, hash.0, hash.1);
        let outp = format!("./target/{}/c/", self.stage) + &outp + ".o";

        args.push(outp.clone());

        let mut sources = HashSet::new();
        sources.insert(inp.into());

        self.steps.push(Step{
            source: inp.into(),
            args,
            deps: sources,
            outp: outp.clone(),
        });

        self.lobjs.push(outp);
    }

    pub fn build(&mut self, cf: &super::emitter::CFile) {
        let mut args = self.cflags.clone();
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
        args.push("-c".to_string());
        args.push(cf.filepath.clone());
        args.push("-o".to_string());


        let mut b = args.join(" ").as_bytes().to_vec();
        b.extend(self.cc.as_bytes());

        let mut hasher: MetroHash128 = MetroHash128::default();
        hasher.write(&b);
        let hash = hasher.finish128();

        let outp = format!("./target/{}/zz/{}_{:x}{:x}.o", self.stage, cf.name.0.join("_"), hash.0, hash.1);
        args.push(outp.clone());

        self.steps.push(Step{
            source: Path::new(&cf.filepath).into(),
            args,
            deps: cf.sources.clone(),
            outp: outp.clone(),
        });
        self.lobjs.push(outp);
    }


    pub fn link(mut self) {
        if self.artifact.typ == super::project::ArtifactType::NodeModule {
            emitter_js::make_npm_module(&self);
            return;
        }

        use rayon::prelude::*;
        use std::sync::{Arc, Mutex};

        let pb = Arc::new(Mutex::new(pbr::ProgressBar::new(self.steps.len() as u64)));
        pb.lock().unwrap().show_speed = false;
        self.steps.par_iter().for_each(|step|{
            if ABORT.load(Ordering::Relaxed) {
                return;
            };
            pb.lock().unwrap().message(&format!("{} {:?} ", self.cc, step.source));

            if step.is_dirty() {
                debug!("{} {:?}", self.cc, step.args);
                let status = Command::new(&self.cc)
                    .env("AFL_USE_ASAN", "1")
                    .args(&step.args)
                    .status()
                    .expect("failed to execute cc");
                if !status.success() {
                    error!("cc: [{}] args: [{}]", self.cc, step.args.join(" "));
                    ABORT.store(true, Ordering::Relaxed);
                }
            }
            pb.lock().unwrap().inc();
        });

        if ABORT.load(Ordering::Relaxed) {
            pb.lock().unwrap().finish_print(&format!("failed [{:?}] {}", self.artifact.typ, self.artifact.name));
            std::process::exit(11);
        }

        let mut cmd     = self.cc.clone();
        let mut args    = Vec::new();

        match self.artifact.typ {
            super::project::ArtifactType::Staticlib => {
                std::fs::create_dir_all(format!("./target/{}/lib/", self.stage)).expect("create target dir");
                cmd = self.ar.clone();
                args = vec![
                    "rcs".to_string(),
                    format!("./target/{}/lib/lib{}.a", self.stage, self.artifact.name)
                ];
                args.extend_from_slice(&self.lobjs);

                if BUILD_RS.load(Ordering::Relaxed) {
                    println!("\n\ncargo:rustc-link-lib=static={}\n\n", self.artifact.name);
                    println!("\n\ncargo:rustc-link-search=native={}/target/{}\n\n", std::env::current_dir().unwrap().display(), self.stage);
                }

            },
            super::project::ArtifactType::Lib => {
                std::fs::create_dir_all(format!("./target/{}/lib/", self.stage)).expect("create target dir");
                args.extend_from_slice(&self.lobjs);
                args.extend_from_slice(&self.lflags);
                args.push("-shared".into());
                args.push("-o".into());
                args.push(format!("./target/{}/lib/lib{}.so", self.stage, self.artifact.name));
            },
            super::project::ArtifactType::Macro => {
                std::fs::create_dir_all(format!("./target/macro/")).expect("create target dir");
                cmd = self.host_cc.clone();
                args.extend_from_slice(&self.lobjs);
                if self.stage.asan {
                    args.push("-fsanitize=address".into());
                }
                args.push("-o".into());
                args.push(format!("./target/macro/{}{}", self.artifact.name, EXE_EXT));
            }
            super::project::ArtifactType::Exe => {
                std::fs::create_dir_all(format!("./target/{}/bin/", self.stage)).expect("create target dir");
                args.extend_from_slice(&self.lobjs);
                args.extend_from_slice(&self.lflags);
                args.push("-o".into());
                args.push(format!("./target/{}/bin/{}{}", self.stage, self.artifact.name, EXE_EXT));
            }
            super::project::ArtifactType::Test  => {
                std::fs::create_dir_all(format!("./target/{}/bin/", self.stage)).expect("create target dir");
                args.extend_from_slice(&self.lobjs);
                args.extend_from_slice(&self.lflags);
                args.push("-o".into());
                args.push(format!("./target/{}/bin/{}{}", self.stage, self.artifact.name, EXE_EXT));
            }
            super::project::ArtifactType::Header  => {
                panic!("cannot link header yet");
            }
            super::project::ArtifactType::NodeModule => {
                unreachable!();
            }
        }
        self.lflags.push("-fvisibility=hidden".into());

        pb.lock().unwrap().message(&format!("ld [{:?}] {} ", self.artifact.typ, self.artifact.name));
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

        pb.lock().unwrap().finish_print(&format!("finished [{:?}] {}", self.artifact.typ, self.artifact.name));
        println!("");
    }
}

impl Step {
    fn is_dirty(&self) -> bool {
        let itarget = match std::fs::metadata(&self.outp) {
            Ok(v)  => v,
            Err(_) => return true,
        };
        let itarget = itarget.modified().expect(&format!("cannot stat {}", self.outp));

        for source in &self.deps {
            let isource = std::fs::metadata(source).expect(&format!("cannot stat {:?}", source));

            let isource = isource.modified().expect(&format!("cannot stat {:?}", source));

            if isource > itarget {
                return true;
            }
        }
        return false;
    }
}
