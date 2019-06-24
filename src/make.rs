use super::project::{Project, Artifact};
use super::ast;
use std::process::Command;
use fasthash::metro;
use std::path::Path;
use std::path::PathBuf;


pub struct Step {
    source: PathBuf,
    args:   Vec<String>
}

pub struct Make {
    artifact:   Artifact,
    steps:      Vec<Step>,
    cflags:     Vec<String>,
    lflags:     Vec<String>,
}

impl Make {
    pub fn new(mut project: Project, artifact: Artifact) -> Self {
        let mut lflags = Vec::new();
        if let Some(plflags) = &project.lflags{
            lflags.extend(plflags.clone());
        }

        let mut cflags = Vec::new();
        if let Some(pcflags) = &project.cflags{
            cflags.extend(pcflags.clone());
        }

        if let Some(cincs) = &project.cincludes {
            for cinc in cincs {
                cflags.push("-I".into());
                cflags.push(cinc.clone());
            }
        }
        cflags.push("-I".into());
        cflags.push("./target/include/".into());
        cflags.push("-fvisibility=hidden".to_string());


        let cobjects = std::mem::replace(&mut project.cobjects, None);

        let mut m = Make {
            artifact,
            //project,
            lflags,
            cflags,
            steps: Vec::new(),
        };

        if let Some(c) = cobjects {
            for c in c {
                m.cobject(Path::new(&c));
            }
        }

        m
    }


    fn is_dirty(&self, sources: &Vec<PathBuf>, target: &str) -> bool {
        let itarget = match std::fs::metadata(target) {
            Ok(v)  => v,
            Err(_) => return true,
        };
        let itarget = itarget.modified().expect(&format!("cannot stat {}", target));

        for source in sources {
            let isource = std::fs::metadata(source).expect(&format!("cannot stat {:?}", source));

            let isource = isource.modified().expect(&format!("cannot stat {:?}", source));

            if isource > itarget {
                return true;
            }
        }
        return false;
    }

    pub fn cobject(&mut self, inp: &Path) {

        let mut args = self.cflags.clone();
        args.push("-c".to_string());
        args.push(inp.to_string_lossy().to_string());
        args.push("-o".to_string());

        let hash = metro::hash128(args.join(" ").as_bytes());

        let outp = inp.to_string_lossy().replace(|c: char| !c.is_alphanumeric(), "_");
        let outp = format!("{}_{:x}", outp, hash);
        let outp = String::from("./target/c/") + &outp + ".o";

        args.push(outp.clone());

        if self.is_dirty(&vec![inp.into()], &outp) {
            self.steps.push(Step{
                source: inp.into(),
                args,
            });
        }

        self.lflags.push(outp);
    }

    pub fn module(&mut self, md: &ast::Module) {
        let name = md.namespace.join("::");
        let inp  = format!("./target/zz/{}.c", name);

        let mut args = self.cflags.clone();
        args.push("-c".to_string());
        args.push(inp.clone());
        args.push("-o".to_string());

        let hash = metro::hash128(args.join(" ").as_bytes());

        let outp = format!("./target/zz/{}_{:x}.o", name, hash);
        args.push(outp.clone());

        if self.is_dirty(&md.sources, &outp) {
            self.steps.push(Step{
                source: Path::new(&inp).into(),
                args,
            });
        }
        self.lflags.push(outp);
    }


    pub fn link(mut self) {
        for step in self.steps {
            info!("clang {:?}", step.source);
            let status = Command::new("clang")
                .args(step.args)
                .status()
                .expect("failed to execute cc");
            if !status.success() {
                error!("error compiling {:?}", step.source);
                std::process::exit(status.code().unwrap_or(3));
            }
        }

        match self.artifact.typ {
            super::project::ArtifactType::Lib => {
                self.lflags.push("-shared".into());
                self.lflags.push("-o".into());
                self.lflags.push(format!("./target/{}.so", self.artifact.name));
            },
            super::project::ArtifactType::Exe => {
                self.lflags.push("-o".into());
                self.lflags.push(format!("./target/{}", self.artifact.name));
            }
        }
        self.lflags.push("-fvisibility=hidden".into());

        info!("ld {:?}", self.lflags);

        let status = Command::new("clang")
            .args(&self.lflags)
            .status()
            .expect("failed to execute linker");
        if !status.success() {
            std::process::exit(status.code().unwrap_or(3));
        }
    }
}
