use std::fs;
use std::collections::HashSet;
use std::collections::HashMap;
use super::ast::*;
use std::io::Write;
use super::parser::Rule;

pub struct Emitter{
    myns:       Vec<String>,
    f:          fs::File,
    emitted:    HashSet<String>,
    pub export_header:   bool,
}

impl Drop for Emitter {
    fn drop(&mut self) {
        if self.export_header {
            write!(self.f, "\n#endif\n").unwrap();
        }
    }
}

impl Emitter {
    pub fn new(myns: Vec<String>) -> Self {
        let ns = myns.join("::");
        let p = format!("target/zz/{}.c", ns);
        let f = fs::File::create(&p).expect(&format!("cannot create {}", p));

        let mut emitted = HashSet::new();
        emitted.insert(String::from("module_") + &ns);
        Emitter{
            myns,
            f,
            emitted,
            export_header: false,
        }
    }

    pub fn new_export_header(myns: Vec<String>) -> Self {
        let ns = myns.join("::");
        let p = format!("target/include/{}.h", ns);
        let mut f = fs::File::create(&p).expect(&format!("cannot create {}", p));

        write!(f, "#ifndef ZZ_EXPORT_HEADER_{}\n#define ZZ_EXPORT_HEADER_{}\n", ns, ns).unwrap();

        let mut emitted = HashSet::new();
        emitted.insert(String::from("module_") + &ns);
        Emitter{
            myns,
            f,
            emitted,
            export_header: true,
        }
    }

    pub fn import(&mut self, modules: &HashMap<String, Module>, mp: &Import) {

        if mp.namespace.first().map(|s|s.as_str()) == Some("c") {
            let mut ns = mp.namespace.clone();
            ns.remove(0);

            if ns.last().map(|s|s.as_str()) == Some("*") {
                ns.pop();
            }

            if ns.len() < 1 {
                let e = pest::error::Error::<Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                    message: format!("imports nothing")
                }, mp.loc.span.clone());
                eprintln!("{} : {}", mp.loc.file, e);
                std::process::exit(9);
            }
            self.include(&Include{
                expr: format!("<{}.h>", ns.join("/")),
                vis: mp.vis.clone(),
                loc: mp.loc.clone(),
            });
            return;
        }

        let mut search = self.myns.clone();
        search.pop();
        search.extend(mp.namespace.iter().cloned());
        let mpname = search.pop().unwrap();

        let module = match modules.get(&search.join("::")) {
            None => {
                let e = pest::error::Error::<Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                    message: format!("module not found"),
                }, mp.loc.span.clone());
                eprintln!("{} : {}", mp.loc.file, e);
                std::process::exit(9);
            }
            Some(m3) => m3,
        };

        if self.emitted.insert(format!("module_{}", module.namespace.join("::"))) {
            for i in &module.includes {
                if let Visibility::Export = i.vis {
                    self.include(i);
                }
            }

            for mp in &module.imports {
                self.import(modules, mp);
            }
        }
        let mut found = false;

        for (name,v) in &module.macros {
            if name == &mpname || mpname == "*" {
                if let Visibility::Object  = v.vis {
                    let e = pest::error::Error::<Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                        message: format!("macro {} in {} is private", mpname, mp.namespace.join("::")),
                    }, mp.loc.span.clone());
                    eprintln!("{} : {}", mp.loc.file, e);
                    std::process::exit(9);
                };
                found = true;
                for mp in &v.imports {
                    self.import(modules,mp);
                }
                self.imacro(&v);
            }
        }


        for s in &module.structs {
            if s.name == mpname || mpname == "*" {
                if let Visibility::Object  = s.vis {
                    if mpname != "*" {
                        let e = pest::error::Error::<Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                            message: format!("struct {} in {} is private", mpname, mp.namespace.join("::")),
                        }, mp.loc.span.clone());
                        eprintln!("{} : {}", mp.loc.file, e);
                        std::process::exit(9);
                    }
                } else {
                    found = true;
                    self.struc(&s);
                }
            }
        }

        for (name,fun) in &module.functions {
            if name == &mpname || mpname == "*" {
                if let Visibility::Object  = fun.vis {
                    if mpname != "*" {
                        let e = pest::error::Error::<Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                            message: format!("function {} in {} is private", mpname, mp.namespace.join("::")),
                        }, mp.loc.span.clone());
                        eprintln!("{} : {}", mp.loc.file, e);
                        std::process::exit(9);
                    }
                } else {
                    found = true;

                    self.declare(&fun, &search);
                }
            }
        }

        for (name,v) in &module.statics {
            if name == &mpname {
                let e = pest::error::Error::<Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                    message: format!("static {} in {} is always private", mpname, mp.namespace.join("::")),
                }, mp.loc.span.clone());
                eprintln!("{} : {}", mp.loc.file, e);
                std::process::exit(9);
            }
        }

        for (name,v) in &module.constants {
            if name == &mpname || mpname == "*" {
                if let Visibility::Object  = v.vis {
                    if mpname != "*" {
                        let e = pest::error::Error::<Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                            message: format!("constant {} in {} is private", mpname, mp.namespace.join("::")),
                        }, mp.loc.span.clone());
                        eprintln!("{} : {}", mp.loc.file, e);
                        std::process::exit(9);
                    }
                } else {
                    found = true;
                    self.constant(&v);
                }
            }
        }


        if !found {
            let e = pest::error::Error::<Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                message: format!("cannot find {} in {}", mpname, mp.namespace.join("::")),
            }, mp.loc.span.clone());
            eprintln!("{} : {}", mp.loc.file, e);
            std::process::exit(9);
        }
    }

    pub fn istatic(&mut self, v: &Static) {
        if !self.emitted.insert(format!("{}", v.name)) {
            return;
        }

        if !self.export_header {
            write!(self.f, "#line {} \"{}\"\n", v.loc.line, v.loc.file).unwrap();
        }

        if v.muta {
            write!(self.f, "static ").unwrap();
        } else {
            write!(self.f, "const ").unwrap();
        }

        match v.storage {
            Storage::Atomic => {
                write!(self.f, "_Atomic ").unwrap();
            },
            Storage::ThreadLocal => {
                write!(self.f, "_Thread_local ").unwrap();
            },
            Storage::Static  => (),
        }

        write!(self.f, "{} {} __attribute__ ((visibility (\"hidden\"))) = {};\n", v.typ, v.name, v.expr).unwrap();

    }
    pub fn constant(&mut self, v: &Const) {
        if !self.emitted.insert(format!("{}", v.name)) {
            return;
        }

        if !self.export_header {
            write!(self.f, "#line {} \"{}\"\n", v.loc.line, v.loc.file).unwrap();
        }
        write!(self.f, "#define {} (({}){})\n", v.name, v.typ, v.expr.replace("\n", "\\\n")).unwrap();
    }

    pub fn imacro(&mut self, v: &Macro) {
        if !self.emitted.insert(format!("{}", v.name)) {
            return;
        }
        if !self.export_header {
            write!(self.f, "#line {} \"{}\"\n", v.loc.line, v.loc.file).unwrap();
        }

        write!(self.f, "#define {}", v.name).unwrap();
        if v.args.len() > 0  {
            write!(self.f, "({})", v.args.join(",")).unwrap();
        }
        write!(self.f, " {}\n", v.body[1..v.body.len()-1].replace("\n", "\\\n")).unwrap();
    }

    pub fn struc(&mut self, s: &Struct) {
        if !self.emitted.insert(format!("struct::{}", s.name)) {
            return;
        }
        write!(self.f, "typedef struct \n").unwrap();
        if !self.export_header {
            write!(self.f, "#line {} \"{}\"\n", s.loc.line, s.loc.file).unwrap();
        }
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

        if !self.export_header {
            write!(self.f, "#line {} \"{}\"\n", f.loc.line, f.loc.file).unwrap();
        }
        match f.vis {
            Visibility::Object => {
                fqn = f.name.clone();
                write!(self.f, "static ").unwrap();
            },
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

        if self.export_header {
            return;
        }

        if fqn == "main" {
            return;
        }

        if let Visibility::Object = f.vis {
            return;
        }

        //aliases are broken in clang, so we need to create an inline redirect
        


        write!(self.f, "#line {} \"{}\"\n", f.loc.line, f.loc.file).unwrap();

        write!(self.f, "static inline ").unwrap();
        write!(self.f, " __attribute__ ((always_inline, unused)) ").unwrap();
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


    pub fn include(&mut self, i: &Include) {
        if !self.emitted.insert(format!("include_{}", i.expr)) {
            return;
        }
        if !self.export_header {
            write!(self.f, "#line {} \"{}\"\n", i.loc.line, i.loc.file).unwrap();
        }
        write!(self.f, "#include {}\n", i.expr).unwrap();
    }

}

