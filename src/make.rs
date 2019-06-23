use super::project::Project;
use super::ast;
use std::process::Command;


pub struct Step {
    source: String,
    args: Vec<String>
}

pub struct Make {
    steps:      Vec<Step>,
    cflags:     Vec<String>,
    lflags:     Vec<String>,
}

impl Make {
    pub fn new(project: Project) -> Self {
        let mut lflags = Vec::new();
        if let Some(plflags) = project.lflags{
            lflags.extend(plflags);
        }

        let mut cflags = Vec::new();
        if let Some(pcflags) = project.cflags{
            cflags.extend(pcflags);
        }
        if let Some(cincs) = project.cincludes {
            for cinc in cincs {
                cflags.push("-I".into());
                cflags.push(cinc);
            }
        }
        cflags.push("-fvisibility=hidden".to_string());


        let mut m = Make {
            lflags,
            cflags,
            steps: Vec::new(),
        };

        if let Some(c) = project.cobjects {
            for c in c {
                m.cobject(c);
            }
        }

        m
    }

    pub fn cobject(&mut self, inp: String) {

        let outp = String::from("./target/c/") + &inp.replace("/", "__") + ".o";

        let mut args = self.cflags.clone();
        args.push("-c".to_string());
        args.push(inp.clone());
        args.push("-o".to_string());
        args.push(outp.clone());

        self.steps.push(Step{
            source: inp,
            args,
        });
        self.lflags.push(outp);
    }

    pub fn module(&mut self, md: &ast::Module) {
        let name = md.namespace.join("::");
        let inp  = format!("./target/zz/{}.c", name);
        let outp = format!("./target/zz/{}.o", name);

        let mut args = self.cflags.clone();
        args.push("-c".to_string());
        args.push(inp.clone());
        args.push("-o".to_string());
        args.push(outp.clone());

        self.steps.push(Step{
            source: inp,
            args,
        });
        self.lflags.push(outp);
    }


    pub fn link(mut self) {
        for step in self.steps {
            println!("clang {}", step.source);
            let status = Command::new("clang")
                .args(step.args)
                .status()
                .expect("failed to execute cc");
            if !status.success() {
                eprintln!("error compiling {}", step.source);
                std::process::exit(status.code().unwrap_or(3));
            }
        }

        self.lflags.push("-o".into());
        self.lflags.push("./target/exe".into());
        self.lflags.push("-fvisibility=hidden".into());

        println!("ld {:?}", self.lflags);

        let status = Command::new("clang")
            .args(&self.lflags)
            .status()
            .expect("failed to execute linker");
        if !status.success() {
            std::process::exit(status.code().unwrap_or(3));
        }
    }
}
