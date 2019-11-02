use super::project::{Config, Artifact};
use fasthash::metro;
use std::path::Path;
use std::path::PathBuf;
use std::collections::HashSet;
use std::process::Command;
use pbr;
use std::sync::atomic::{AtomicBool, Ordering};

static ABORT:           AtomicBool = AtomicBool::new(false);
pub static BUILD_RS:    AtomicBool = AtomicBool::new(false);


#[derive(Clone)]
pub struct Stage {
    pub name:       String,
    pub debug:      bool,
    pub optimize:   Option<String>,
    pub lto:        bool,
    pub asan:       bool,
}

impl Stage {
    pub fn release() -> Self {
        Stage {
            name:       "release".to_string(),
            debug:      false,
            optimize:   Some("03".to_string()),
            lto:        true,
            asan:       false,
        }
    }
    pub fn test() -> Self {
        Stage {
            name:       "test".to_string(),
            debug:      true,
            optimize:   None,
            lto:        false,
            asan:       true,
        }
    }
    pub fn debug() -> Self {
        Stage {
            name:       "debug".to_string(),
            debug:      true,
            optimize:   None,
            lto:        false,
            asan:       false,
        }
    }
}

impl std::fmt::Display for  Stage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}


pub struct Step {
    source: PathBuf,
    args:   Vec<String>,

    deps:   HashSet<PathBuf>,
    outp:   String,
}

pub struct Make {
    artifact:   Artifact,
    steps:      Vec<Step>,
    cc:         String,
    ar:         String,
    cflags:     Vec<String>,
    lflags:     Vec<String>,
    lobjs:      Vec<String>,
    variant:    String,
    stage:      Stage,
}

impl Make {
    pub fn new(mut config: Config, variant: &str, stage: Stage, artifact: Artifact) -> Self {


        let features = config.features(variant);

        let mut lflags = Vec::new();
        let mut cflags = Vec::new();

        let mut cc = std::env::var("TARGET_CC")
            .or(std::env::var("CC"))
            .unwrap_or("clang".to_string());

        if let Some(std) = config.project.std {
            cflags.push(format!("-std={}", std));
            if std.contains("c++") {
                cc = std::env::var("TARGET_CXX")
                    .or(std::env::var("CXX"))
                    .unwrap_or("clang++".to_string());
            }
        }
        let ar = std::env::var("TARGET_AR")
            .or(std::env::var("AR"))
            .unwrap_or("ar".to_string());

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

        cflags.push("-fPIC".into());
        cflags.push("-I".into());
        cflags.push(".".into());
        cflags.push("-I".into());
        cflags.push(format!("./target/{}/include/", stage));
        cflags.push("-fvisibility=hidden".to_string());


        if let Some(opt) = &stage.optimize {
            cflags.push(format!("-O{}",opt).into());
            lflags.push("-O3".into());
        }

        if stage.lto {
            cflags.push("-flto".into());
            lflags.push("-flto".into());
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

        let hash = metro::hash128(args.join(" ").as_bytes());

        let outp = inp.to_string_lossy().replace(|c: char| !c.is_alphanumeric(), "_");
        let outp = format!("{}_{:x}", outp, hash);
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
        args.push("-Werror=pointer-sign".to_string());
        args.push("-Werror=int-to-pointer-cast".to_string());
        args.push("-c".to_string());
        args.push(cf.filepath.clone());
        args.push("-o".to_string());


        let mut b = args.join(" ").as_bytes().to_vec();
        b.extend(self.cc.as_bytes());

        let hash = metro::hash128(b);

        let outp = format!("./target/{}/zz/{}_{:x}.o", self.stage, cf.name, hash);
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
                let status = Command::new(&self.cc)
                    .args(&step.args)
                    .status()
                    .expect("failed to execute cc");
                if !status.success() {
                    error!("{} {}", self.cc, step.args.join(" "));
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
                cmd = self.ar.clone();
                args = vec![
                    "rcs".to_string(),
                    format!("./target/{}/lib{}.a", self.stage, self.artifact.name)
                ];
                args.extend_from_slice(&self.lobjs);

                if BUILD_RS.load(Ordering::Relaxed) {
                    println!("\n\ncargo:rustc-link-lib=static={}\n\n", self.artifact.name);
                    println!("\n\ncargo:rustc-link-search=native={}/target/{}\n\n", std::env::current_dir().unwrap().display(), self.variant);
                }

            },
            super::project::ArtifactType::Lib => {
                args.extend_from_slice(&self.lobjs);
                args.extend_from_slice(&self.lflags);
                args.push("-shared".into());
                args.push("-o".into());
                args.push(format!("./target/{}/lib{}.so", self.stage, self.artifact.name));
            },
            super::project::ArtifactType::Exe => {
                args.extend_from_slice(&self.lobjs);
                args.extend_from_slice(&self.lflags);
                args.push("-o".into());
                args.push(format!("./target/{}/{}", self.stage, self.artifact.name));
            }
            super::project::ArtifactType::Test  => {
                args.extend_from_slice(&self.lobjs);
                args.extend_from_slice(&self.lflags);
                args.push("-o".into());
                args.push(format!("./target/{}/{}", self.stage, self.artifact.name));
            }
            super::project::ArtifactType::Header  => {
                panic!("cannot link header yet");
            }
        }
        self.lflags.push("-fvisibility=hidden".into());

        pb.lock().unwrap().message(&format!("ld [{:?}] {} ", self.artifact.typ, self.artifact.name));
        pb.lock().unwrap().tick();

        debug!("{:?}", args);

        let status = Command::new(&cmd)
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
