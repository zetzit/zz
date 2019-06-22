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
        let p = format!("target/c/{}", mn);
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
            match modules.get(&mp.namespace) {
                None => panic!("{}: imports unknown module {}", mp.loc, &mp.namespace),
                Some(m3) => {
                    self.import(modules, m3, mp);
                }
            }
        }

        let mut found = false;
        for s in &m2.structs {
            if s.name == mp.name || mp.name == "*" {
                if let Visibility::Object  = s.vis {
                    panic!("{}: imports private struct {}::{}", mp.loc, &mp.namespace, &mp.name);
                };
                found = true;
                self.struc(None, &s);
                //em.struc(Some(&m2.name), &s);
            }
        }

        for (name,fun) in &m2.functions {
            if name == &mp.name || mp.name == "*" {
                if let Visibility::Object  = fun.vis {
                    panic!("{}: imports private function {}::{}", mp.loc, &mp.namespace, &mp.name);
                };
                found = true;
                self.function(None, &fun, None);
                //em.function(Some(&m2.name), &fun, None);
            }
        }

        for (name,v) in &m2.constants {
            if name == &mp.name || mp.name == "*" {
                if let Visibility::Object  = v.vis {
                    panic!("{}: imports private function {}::{}", mp.loc, &mp.namespace, &mp.name);
                };
                found = true;
                self.constant(None, &v);
            }
        }


        if !found {
            panic!("{}: import '{}' not found in module '{}'", mp.loc, &mp.name, &mp.namespace);
        }
    }

    pub fn constant(&mut self, ns: Option<&str>, v: &Const) {
        if !self.emitted.insert(format!("{:?}::{}", ns, v.name)) {
            return;
        }
        write!(self.f, "#line {} \"{}\"\n", v.loc.line, v.loc.file).unwrap();
        write!(self.f, "#define {} (({}){})\n", v.name, v.typ, v.expr).unwrap();
    }
    pub fn struc(&mut self, ns: Option<&str>, s: &Struct) {
        if !self.emitted.insert(format!("{:?}::{}", ns, s.name)) {
            return;
        }
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
        if !self.emitted.insert(format!("{:?}::{}", ns, f.name)) {
            return;
        }
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
        if !self.emitted.insert(format!("include_{}", i)) {
            return;
        }
        write!(self.f, "#include {}\n", i).unwrap();
    }

}

