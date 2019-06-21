use std::fs;
use std::collections::HashSet;
use super::ast::*;
use std::io::Write;

pub struct Emitter{
    f: fs::File,
    included: HashSet<String>,
}
impl Emitter {

    pub fn new(mn: &str) -> Self {
        let p = format!("target/c/{}", mn);
        let f = fs::File::create(&p).expect(&format!("cannot create {}", p));
        Emitter{
            f,
            included: HashSet::new()
        }
    }

    pub fn struc(&mut self, ns: Option<&str>, s: &Struct) {
        write!(self.f, "typedef struct \n").unwrap();
        write!(self.f, "#line {} \"{}\"\n", s.loc.line, s.loc.file).unwrap();
        write!(self.f, "{}\n",
                 s.body,
                 ).unwrap();

        if let Some(ns) = ns {
            write!(self.f, "{}_",ns).unwrap();
        }

        write!(self.f, "{} ;\n", s.name).unwrap();
    }

    pub fn function(&mut self, ns: Option<&str>, f: &Function, body: Option<&str>) {
        write!(self.f, "#line {} \"{}\"\n", f.loc.line, f.loc.file).unwrap();

        if let Visibility::Object = f.vis {
            write!(self.f, "static ").unwrap();
        }

        match &f.ret {
            None       => write!(self.f, "void "),
            Some(a)    => write!(self.f, "{} ", &a.typ),
        }.unwrap();

        if let Some(ns) = ns {
            write!(self.f, "{}_",ns).unwrap();
        }

        write!(self.f, "{} (", f.name).unwrap();

        let mut first = true ;
        for arg in &f.args {
            if first {
                first = false;
            } else {
                write!(self.f, ", ").unwrap();
            }

            if !arg.muta {
                write!(self.f, "const ").unwrap();
            }

            if let Some(ns) = &arg.namespace {
                write!(self.f, "{}", ns.replace("::", "_")).unwrap();
            }

            write!(self.f, "{}", arg.typ).unwrap();

            if arg.ptr {
                write!(self.f, " *").unwrap();
            }

            write!(self.f, " {}", arg.name).unwrap();
        }
        match body {
            None => write!(self.f, ");\n").unwrap(),
            Some(b) => write!(self.f, ") {}\n\n", b).unwrap(),
        }
    }

    pub fn include(&mut self, i: &str) {
        if self.included.insert(i.into()) {
            write!(self.f, "#include {}\n", i).unwrap();
        }
    }

}

