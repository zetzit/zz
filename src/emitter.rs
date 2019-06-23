use std::fs;
use std::collections::HashSet;
use std::collections::HashMap;
use super::ast::*;
use std::io::Write;

pub struct Emitter{
    f: fs::File,
    emitted: HashSet<String>,
}

impl Emitter {
    pub fn new(mn: &str) -> Self {
        let p = format!("target/zz/{}", mn);
        let f = fs::File::create(&p).expect(&format!("cannot create {}", p));
        Emitter{
            f,
            emitted: HashSet::new()
        }
    }

    pub fn import(&mut self, modules: &HashMap<String, Module>, m2: &Module, mp: &Import) {
        for i in &m2.includes {
            self.include(i);
        }

        for mp in &m2.imports {
            match modules.get(&mp.namespace.join("::")) {
                None => panic!("{}: imports unknown module {}", mp.loc, &mp.namespace.join("::")),
                Some(m3) => {
                    self.import(modules, m3, mp);
                }
            }
        }

        let mut found = false;
        for s in &m2.structs {
            if s.name == mp.name || mp.name == "*" {
                if let Visibility::Object  = s.vis {
                    panic!("{}: imports private struct {}::{}", mp.loc, &mp.namespace.join("::"), &mp.name);
                };
                found = true;
                self.struc(&s);
                //em.struc(Some(&m2.name), &s);
            }
        }

        for (name,fun) in &m2.functions {
            if name == &mp.name || mp.name == "*" {
                if let Visibility::Object  = fun.vis {
                    panic!("{}: imports private function {}::{}", mp.loc, &mp.namespace.join("::"), &mp.name);
                };
                found = true;
                self.declare(&fun, &mp.namespace);
                //em.function(Some(&m2.name), &fun, None);
            }
        }

        for (name,v) in &m2.constants {
            if name == &mp.name || mp.name == "*" {
                if let Visibility::Object  = v.vis {
                    panic!("{}: imports private function {}::{}", mp.loc, &mp.namespace.join("::"), &mp.name);
                };
                found = true;
                self.constant(None, &v);
            }
        }


        if !found {
            panic!("{}: import '{}' not found in module '{}'", mp.loc, &mp.name, &mp.namespace.join("::"));
        }
    }

    pub fn constant(&mut self, ns: Option<&str>, v: &Const) {
        if !self.emitted.insert(format!("{:?}::{}", ns, v.name)) {
            return;
        }
        write!(self.f, "#line {} \"{}\"\n", v.loc.line, v.loc.file).unwrap();
        write!(self.f, "#define {} (({}){})\n", v.name, v.typ, v.expr).unwrap();
    }

    pub fn struc(&mut self, s: &Struct) {
        if !self.emitted.insert(format!("struct::{}", s.name)) {
            return;
        }
        write!(self.f, "typedef struct \n").unwrap();
        write!(self.f, "#line {} \"{}\"\n", s.loc.line, s.loc.file).unwrap();
        write!(self.f, "{}\n",
               s.body,
               ).unwrap();

        write!(self.f, "{} ;\n", s.name).unwrap();
    }



    fn function_args(&mut self, f: &Function) {
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
    }

    pub fn declare(&mut self, f: &Function, ns: &Vec<String>) {
        if !self.emitted.insert(format!("{:?}::{}", ns, f.name)) {
            return;
        }

        let mut ns = ns.clone();
        ns.push(f.name.clone());
        let mut fqn = ns.join("_");

        write!(self.f, "#line {} \"{}\"\n", f.loc.line, f.loc.file).unwrap();
        match f.vis {
            Visibility::Object => write!(self.f, "static ").unwrap(),
            _ => {
                if f.name == "main" {
                    fqn = "main".into();
                }
            },
        };
        match &f.ret {
            None       => write!(self.f, "void "),
            Some(a)    => write!(self.f, "{} ", &a.typ),
        }.unwrap();
        write!(self.f, "{} (", fqn).unwrap();
        self.function_args(&f);
        write!(self.f, ");\n").unwrap();



        if fqn == "main" {
            return;
        }


        if let Visibility::Object = f.vis {
            return;
        }

        //aliases are broken in clang, so we need to create an inline redirect

        write!(self.f, "#line {} \"{}\"\n", f.loc.line, f.loc.file).unwrap();

        write!(self.f, "static inline ").unwrap();
        write!(self.f, " __attribute__ ((always_inline)) ").unwrap();
        match &f.ret {
            None       => write!(self.f, "void "),
            Some(a)    => write!(self.f, "{} ", &a.typ),
        }.unwrap();
        write!(self.f, "{} (", f.name).unwrap();

        self.function_args(&f);
        write!(self.f, ")").unwrap();

        match f.vis {
            Visibility::Object => {
                write!(self.f, ";\n").unwrap();
            }
            _ => {
                write!(self.f, "{{").unwrap();
                if f.ret.is_some() {
                    write!(self.f, "return ").unwrap();
                }

                write!(self.f, "{}(", fqn).unwrap();

                let mut first = true;
                for arg in &f.args {
                    if first {
                        first = false;
                    } else {
                        write!(self.f, ", ").unwrap();
                    }
                    write!(self.f, " {}", arg.name).unwrap();
                }

                write!(self.f, ");}}; \n").unwrap();
            }
        }



    }

    pub fn define(&mut self, f: &Function, ns: &Vec<String>, body: &str) {
        write!(self.f, "#line {} \"{}\"\n", f.loc.line, f.loc.file).unwrap();

        let name = match f.vis {
            Visibility::Object => {
                write!(self.f, "static ").unwrap();
                f.name.clone()
            }
            _ => {
                if f.name == "main" {
                    f.name.clone()
                } else {
                    let mut ns = ns.clone();
                    ns.push(f.name.clone());
                    ns.join("_")
                }
            }
        };

        match &f.ret {
            None       => write!(self.f, "void "),
            Some(a)    => write!(self.f, "{} ", &a.typ),
        }.unwrap();

        match f.vis {
            Visibility::Object => (),
            Visibility::Shared => write!(self.f, "__attribute__ ((visibility (\"hidden\"))) ").unwrap(),
            Visibility::Export => write!(self.f, "__attribute__ ((visibility (\"default\"))) ").unwrap(),
        }

        write!(self.f, "{} (", name).unwrap();
        self.function_args(&f);
        write!(self.f, ") {}\n\n", body).unwrap();
    }


    pub fn include(&mut self, i: &str) {
        if !self.emitted.insert(format!("include_{}", i)) {
            return;
        }
        write!(self.f, "#include {}\n", i).unwrap();
    }

}

