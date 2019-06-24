use std::fs;
use std::collections::HashSet;
use std::collections::HashMap;
use super::ast::*;
use std::io::Write;
use super::parser::Rule;


#[derive(Default)]
struct Buffers {
    includes:   Vec<u8>,
    macros:     Vec<u8>,
    constants:  Vec<u8>,
    statics:    Vec<u8>,
    structs:    Vec<u8>,
    decls:      Vec<u8>,
    defs:       Vec<u8>,
}

pub struct Emitter{
    myns:               Vec<String>,
    f:                  fs::File,
    b:                  Buffers,
    emitted:            HashSet<String>,
    pub export_header:  bool,
}

impl Drop for Emitter {
    fn drop(&mut self) {

        self.f.write_all(&self.b.includes).unwrap();
        self.f.write_all(&self.b.macros).unwrap();
        self.f.write_all(&self.b.statics).unwrap();
        self.f.write_all(&self.b.constants).unwrap();
        self.f.write_all(&self.b.structs).unwrap();
        self.f.write_all(&self.b.decls).unwrap();
        self.f.write_all(&self.b.defs).unwrap();

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
            b: Buffers::default(),
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
            b: Buffers::default(),
            emitted,
            export_header: true,
        }
    }

    pub fn import(&mut self, modules: &HashMap<String, Module>, mut mp: Vec<Import>) {

        mp.retain(|mp|{
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
                false
            } else {
                true
            }
        });


        let mp : Vec<(&Module, Vec<String>, String, Import)> = mp.into_iter().map(|mp|{
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

                self.import(modules, module.imports.clone());
            }
            (module, search, mpname, mp)
        }).collect();

        let mut found = vec![false; mp.len()];

        for (n, (module, _search, mpname, mp)) in mp.iter().enumerate() {
            for (name,v) in &module.macros {
                if name == mpname || mpname == "*" {
                    if let Visibility::Object  = v.vis {
                        let e = pest::error::Error::<Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                            message: format!("macro {} in {} is private", mpname, mp.namespace.join("::")),
                        }, mp.loc.span.clone());
                        eprintln!("{} : {}", mp.loc.file, e);
                        std::process::exit(9);
                    };
                    found[n] = true;
                    for mp in &v.imports {
                        self.import(modules, vec![mp.clone()]);
                    }
                    self.imacro(&v);
                }
            }
        }

        for (n, (module, _search, mpname, mp)) in mp.iter().enumerate() {
            for s in &module.structs {
                if &s.name == mpname || mpname == "*" {
                    if let Visibility::Object  = s.vis {
                        if mpname != "*" {
                            let e = pest::error::Error::<Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                                message: format!("struct {} in {} is private", mpname, mp.namespace.join("::")),
                            }, mp.loc.span.clone());
                            eprintln!("{} : {}", mp.loc.file, e);
                            std::process::exit(9);
                        }
                    } else {
                        found[n] = true;
                        self.struc(&s);
                    }
                }
            }
        }

        for (n, (module, search, mpname, mp)) in mp.iter().enumerate() {
            for (name,fun) in &module.functions {
                if name == mpname || mpname == "*" {
                    if let Visibility::Object  = fun.vis {
                        if mpname != "*" {
                            let e = pest::error::Error::<Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                                message: format!("function {} in {} is private", mpname, mp.namespace.join("::")),
                            }, mp.loc.span.clone());
                            eprintln!("{} : {}", mp.loc.file, e);
                            std::process::exit(9);
                        }
                    } else {
                        found[n] = true;

                        self.declare(&fun, &search);
                    }
                }
            }
        }

        for (_n, (module, _search, mpname, mp)) in mp.iter().enumerate() {
            for (name,_) in &module.statics {
                if name == mpname {
                    let e = pest::error::Error::<Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                        message: format!("static {} in {} is always private", mpname, mp.namespace.join("::")),
                    }, mp.loc.span.clone());
                    eprintln!("{} : {}", mp.loc.file, e);
                    std::process::exit(9);
                }
            }
        }

        for (n, (module, _search, mpname, mp)) in mp.iter().enumerate() {
            for (name,v) in &module.constants {
                if name == mpname || mpname == "*" {
                    if let Visibility::Object  = v.vis {
                        if mpname != "*" {
                            let e = pest::error::Error::<Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                                message: format!("constant {} in {} is private", mpname, mp.namespace.join("::")),
                            }, mp.loc.span.clone());
                            eprintln!("{} : {}", mp.loc.file, e);
                            std::process::exit(9);
                        }
                    } else {
                        found[n] = true;
                        self.constant(&v);
                    }
                }
            }
        }


        for (n, (_module, _search, mpname, mp)) in mp.iter().enumerate() {
            if !found[n] {
                let e = pest::error::Error::<Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                    message: format!("cannot find {} in {}", mpname, mp.namespace.join("::")),
                }, mp.loc.span.clone());
                eprintln!("{} : {}", mp.loc.file, e);
                std::process::exit(9);
            }
        }
    }

    pub fn istatic(&mut self, v: &Static) {
        if !self.emitted.insert(format!("{}", v.name)) {
            return;
        }
        let f = &mut self.b.statics;

        if !self.export_header {
            write!(f, "#line {} \"{}\"\n", v.loc.line, v.loc.file).unwrap();
        }

        if v.muta {
            write!(f, "static ").unwrap();
        } else {
            write!(f, "const ").unwrap();
        }

        match v.storage {
            Storage::Atomic => {
                write!(f, "_Atomic ").unwrap();
            },
            Storage::ThreadLocal => {
                write!(f, "_Thread_local ").unwrap();
            },
            Storage::Static  => (),
        }

        write!(f, "{} {} __attribute__ ((visibility (\"hidden\"))) = {};\n", v.typ, v.name, v.expr).unwrap();
    }

    pub fn constant(&mut self, v: &Const) {
        if !self.emitted.insert(format!("{}", v.name)) {
            return;
        }
        let f = &mut self.b.constants;

        if !self.export_header {
            write!(f, "#line {} \"{}\"\n", v.loc.line, v.loc.file).unwrap();
        }
        write!(f, "#define {} (({}){})\n", v.name, v.typ, v.expr.replace("\n", "\\\n")).unwrap();
    }

    pub fn imacro(&mut self, v: &Macro) {
        if !self.emitted.insert(format!("{}", v.name)) {
            return;
        }
        let f = &mut self.b.macros;

        if !self.export_header {
            write!(f, "#line {} \"{}\"\n", v.loc.line, v.loc.file).unwrap();
        }

        write!(f, "#define {}", v.name).unwrap();
        if v.args.len() > 0  {
            write!(f, "({})", v.args.join(",")).unwrap();
        }
        write!(f, " {}\n", v.body[1..v.body.len()-1].replace("\n", "\\\n")).unwrap();
    }

    pub fn struc(&mut self, s: &Struct) {
        if !self.emitted.insert(format!("struct::{}", s.name)) {
            return;
        }
        let f = &mut self.b.structs;

        write!(f, "typedef struct \n").unwrap();
        if !self.export_header {
            write!(f, "#line {} \"{}\"\n", s.loc.line, s.loc.file).unwrap();
        }
        write!(f, "{}\n",
               s.body,
               ).unwrap();

        write!(f, "{} ;\n", s.name).unwrap();
    }



    fn function_args<F:Write>(f:&mut F, v: &Function) {
        let mut first = true ;
        for arg in &v.args {
            if first {
                first = false;
            } else {
                write!(f, ", ").unwrap();
            }

            if !arg.muta {
                write!(f, "const ").unwrap();
            }

            if let Some(ns) = &arg.namespace {
                write!(f, "{}", ns.replace("::", "_")).unwrap();
            }

            write!(f, "{}", arg.typ).unwrap();

            if arg.ptr {
                write!(f, " *").unwrap();
            }

            write!(f, " {}", arg.name).unwrap();
        }
    }

    pub fn declare(&mut self, v: &Function, ns: &Vec<String>) {
        if !self.emitted.insert(format!("{:?}::{}", ns, v.name)) {
            return;
        }
        let f = &mut self.b.decls;

        let mut ns = ns.clone();
        ns.push(v.name.clone());
        let mut fqn = ns.join("_");

        if !self.export_header {
            write!(f, "#line {} \"{}\"\n", v.loc.line, v.loc.file).unwrap();
        }
        match v.vis {
            Visibility::Object => {
                fqn = v.name.clone();
                write!(f, "static ").unwrap();
            },
            _ => {
                if v.name == "main" {
                    fqn = "main".into();
                }
            },
        };
        match &v.ret {
            None       => write!(f, "void "),
            Some(a)    => write!(f, "{} ", &a.typ),
        }.unwrap();
        write!(f, "{} (", fqn).unwrap();
        Self::function_args(f, &v);
        write!(f, ");\n").unwrap();

        if self.export_header {
            return;
        }

        if fqn == "main" {
            return;
        }

        if let Visibility::Object = v.vis {
            return;
        }

        //aliases are broken in clang, so we need to create an inline redirect

        write!(f, "#line {} \"{}\"\n", v.loc.line, v.loc.file).unwrap();

        write!(f, "static inline ").unwrap();
        write!(f, " __attribute__ ((always_inline, unused)) ").unwrap();
        match &v.ret {
            None       => write!(f, "void "),
            Some(a)    => write!(f, "{} ", &a.typ),
        }.unwrap();
        write!(f, "{} (", v.name).unwrap();

        Self::function_args(f, &v);
        write!(f, ")").unwrap();

        match v.vis {
            Visibility::Object => {
                write!(f, ";\n").unwrap();
            }
            _ => {
                write!(f, "{{").unwrap();
                if v.ret.is_some() {
                    write!(f, "return ").unwrap();
                }

                write!(f, "{}(", fqn).unwrap();

                let mut first = true;
                for arg in &v.args {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ").unwrap();
                    }
                    write!(f, " {}", arg.name).unwrap();
                }

                write!(f, ");}}; \n").unwrap();
            }
        }



    }

    pub fn define(&mut self, v: &Function, ns: &Vec<String>, body: &str) {
        let f = &mut self.b.defs;

        write!(f, "#line {} \"{}\"\n", v.loc.line, v.loc.file).unwrap();

        let name = match v.vis {
            Visibility::Object => {
                write!(f, "static ").unwrap();
                v.name.clone()
            }
            _ => {
                if v.name == "main" {
                    v.name.clone()
                } else {
                    let mut ns = ns.clone();
                    ns.push(v.name.clone());
                    ns.join("_")
                }
            }
        };

        match &v.ret {
            None       => write!(f, "void "),
            Some(a)    => write!(f, "{} ", &a.typ),
        }.unwrap();

        match v.vis {
            Visibility::Object => (),
            Visibility::Shared => write!(f, "__attribute__ ((visibility (\"hidden\"))) ").unwrap(),
            Visibility::Export => write!(f, "__attribute__ ((visibility (\"default\"))) ").unwrap(),
        }

        write!(f, "{} (", name).unwrap();
        Self::function_args(f, &v);
        write!(f, ") {}\n\n", body).unwrap();
    }


    pub fn include(&mut self, i: &Include) {
        let f = &mut self.b.includes;
        if !self.emitted.insert(format!("include_{}", i.expr)) {
            return;
        }
        if !self.export_header {
            write!(f, "#line {} \"{}\"\n", i.loc.line, i.loc.file).unwrap();
        }
        write!(f, "#include {}\n", i.expr).unwrap();
    }

}

