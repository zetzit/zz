use std::fs;
use std::collections::HashSet;
use std::collections::HashMap;
use super::ast::*;
use super::project::ArtifactType;
use std::io::Write;
use super::parser::Rule;


macro_rules! compile_error {
    ($loc:expr, $m:expr) => {
        {
            let e = pest::error::Error::<Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                message: $m,
            }, $loc.span.clone());
            error!("compile error\n{}{}", $loc.file, e);
            std::process::exit(9);
        }
    };
}

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
    artifact:       ArtifactType,
    myns:           Vec<String>,
    f:              fs::File,
    b:              Buffers,
    locals:         HashMap<String, (Location, Vec<Location>)>,
    included:       HashSet<String>,
    importstack:    Vec<Location>,
}

impl Drop for Emitter {
    fn drop(&mut self) {

        if let ArtifactType::Header = self.artifact {
            let ns = self.myns.join("::");
            write!(self.f, "#ifndef ZZ_EXPORT_HEADER_{}\n#define ZZ_EXPORT_HEADER_{}\n", ns, ns).unwrap();
        }

        self.f.write_all(&self.b.includes).unwrap();
        self.f.write_all(&self.b.macros).unwrap();
        self.f.write_all(&self.b.structs).unwrap();
        self.f.write_all(&self.b.statics).unwrap();
        self.f.write_all(&self.b.constants).unwrap();
        self.f.write_all(&self.b.decls).unwrap();
        self.f.write_all(&self.b.defs).unwrap();

        if let ArtifactType::Header = self.artifact {
            write!(self.f, "\n#endif\n").unwrap();
        }
    }
}

impl Emitter {
    pub fn new(myns: Vec<String>, artifact: ArtifactType) -> Self {
        let ns = myns.join("::");
        let p = if let ArtifactType::Header = artifact {
            format!("target/include/{}.h", ns)
        } else {
            format!("target/zz/{}.c", ns)
        };
        let f = fs::File::create(&p).expect(&format!("cannot create {}", p));

        Emitter{
            artifact,
            myns,
            f,
            b: Buffers::default(),
            locals: HashMap::new(),
            included: HashSet::new(),
            importstack: Vec::new(),
        }
    }

    pub fn nameguard(&mut self, name: String, source_loc: Location) -> bool {

        if let Some(previous) = self.locals.insert(name.clone(), (source_loc.clone(), self.importstack.clone())) {
            if previous.0 != source_loc {


                let mut errs = format!("conflict in module {}\n", self.myns.join("::"));


                if self.importstack.len() == 0 {
                    let e = pest::error::Error::<Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                        message: format!("conflicting declaration of '{}'\n", name),
                    }, source_loc.span.clone());
                    errs += &format!("{}{}\n", source_loc.file, e);
                }
                for loc in self.importstack.clone() {
                    let e = pest::error::Error::<Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                        message: format!("conflicting import of '{}'\n", name),
                    }, loc.span.clone());
                    errs += &format!("{}{}\n", loc.file, e);
                }

                if previous.1.len() == 0 {
                    let e = pest::error::Error::<Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                        message: "was already defined here".to_string(),
                    }, previous.0.span.clone());
                    errs += &format!("{}{}\n", previous.0.file, e);
                }

                for loc in previous.1 {
                    let e = pest::error::Error::<Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                        message: "was already imported here".to_string(),
                    }, loc.span.clone());
                    errs += &format!("{}{}\n", loc.file, e);
                }

                error!("{}", errs);

                std::process::exit(9);
            }
            false
        } else {
            true
        }
    }

    pub fn import(&mut self, modules: &HashMap<String, Module>, mut imps: Vec<Import>) {


        // import any c file as include
        imps.retain(|imp|{
            if imp.namespace.first().map(|s|s.as_str()) == Some("c") {
                let mut ns = imp.namespace.clone();
                ns.remove(0);

                if ns.last().map(|s|s.as_str()) == Some("*") {
                    ns.pop();
                }

                if ns.len() < 1 {
                    compile_error!(imp.loc, format!("imports nothing"));
                }
                self.include(&Include{
                    expr: format!("<{}.h>", ns.join("/")),
                    vis: imp.vis.clone(),
                    loc: imp.loc.clone(),
                });
                false
            } else {
                true
            }
        });

        // resolve imports to modules
        let imps : Vec<(&Module, Vec<String>, String, Import)> = imps.into_iter().map(|imp|{

            let mut search = imp.namespace.clone();
            let mpname = search.pop().unwrap();

            let module = match modules.get(&search.join("::")) {
                None => {
                    let e = pest::error::Error::<Rule>::new_from_span(pest::error::ErrorVariant::CustomError {
                        message: format!("internal compiler error: module was not resolved"),
                    }, imp.loc.span.clone());
                    error!("{} : {}", imp.loc.file, e);
                    std::process::exit(9);
                }
                Some(m3) => m3,
            };

            /* TODO importing recursively is shitty
            if (self.nameguard(module.namespace.join("::"), format!("imported as {}", module.namespace.join("::")))) {
                for i in &module.includes {
                    if let Visibility::Export = i.vis {
                        self.include(i);
                    }
                }
                self.import(modules, module.imports.clone());
            }
            */

            (module, search, mpname, imp)
        }).collect();

        let mut found = vec![false; imps.len()];

        // macros
        for (n, (module, _search, mpname, imp)) in imps.iter().enumerate() {
            self.importstack.push(imp.loc.clone());
            for (name,v) in &module.macros {
                if name == mpname || mpname == "*" {
                    if let Visibility::Object  = v.vis {
                        compile_error!(imp.loc, format!("macro {} in {} is private", mpname, imp.namespace.join("::")));
                    };
                    found[n] = true;
                    for mp in &v.imports {
                        self.importstack.push(mp.loc.clone());
                        self.import(modules, vec![mp.clone()]);
                        self.importstack.pop();
                    }
                    self.imacro(&v);
                }
            }
            self.importstack.pop();
        }

        // structs
        for (n, (module, _search, mpname, imp)) in imps.iter().enumerate() {
            self.importstack.push(imp.loc.clone());
            for s in &module.structs {
                if &s.name == mpname || mpname == "*" {
                    if let Visibility::Object  = s.vis {
                        if mpname != "*" {
                            compile_error!(imp.loc, format!("struct {} in {} is private", mpname, imp.namespace.join("::")));
                        }
                    } else {
                        found[n] = true;
                        self.struc(&s);
                    }
                }
            }
            self.importstack.pop();
        }

        // function declarations
        for (n, (module, _search, mpname, imp)) in imps.iter().enumerate() {
            self.importstack.push(imp.loc.clone());
            for (name,fun) in &module.functions {
                if name == mpname || mpname == "*" {
                    if let Visibility::Object  = fun.vis {
                        if mpname != "*" {
                            compile_error!(imp.loc, format!("function {} in {} is private", mpname, imp.namespace.join("::")));
                        }
                    } else {
                        found[n] = true;

                        self.declare(&fun, &module.namespace);
                    }
                }
            }
            self.importstack.pop();
        }

        // statics
        for (_n, (module, _search, mpname, imp)) in imps.iter().enumerate() {
            self.importstack.push(imp.loc.clone());
            for (name,_) in &module.statics {
                if name == mpname {
                    compile_error!(imp.loc, format!("{} is static", imp.namespace.join("::")));
                }
            }
            self.importstack.pop();
        }

        // constants
        for (n, (module, _search, mpname, imp)) in imps.iter().enumerate() {
            self.importstack.push(imp.loc.clone());
            for (name,v) in &module.constants {
                if name == mpname || mpname == "*" {
                    if let Visibility::Object  = v.vis {
                        if mpname != "*" {
                            compile_error!(imp.loc, format!("constant {} in {} is private", mpname, imp.namespace.join("::")));
                        }
                    } else {
                        found[n] = true;
                        self.constant(&v);
                    }
                }
            }
            self.importstack.pop();
        }

        // check if we found all imports
        for (n, (_module, _search, mpname, imp)) in imps.iter().enumerate() {
            if !found[n] {
                compile_error!(imp.loc, format!("cannot find {} in {}", mpname, imp.namespace.join("::")));
            }
        }
    }

    pub fn istatic(&mut self, v: &Static) {
        if !self.nameguard(v.name.clone(), v.loc.clone()) {
            return;
        }
        let f = &mut self.b.statics;

        if let ArtifactType::Header = self.artifact {
        } else {
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
        if !self.nameguard(v.name.clone(), v.loc.clone()) {
            return;
        }

        let f = &mut self.b.constants;

        if let ArtifactType::Header = self.artifact {
        } else {
            write!(f, "#line {} \"{}\"\n", v.loc.line, v.loc.file).unwrap();
        }
        write!(f, "#define {} (({}){})\n", v.name, v.typ, v.expr.replace("\n", "\\\n")).unwrap();
    }

    pub fn imacro(&mut self, v: &Macro) {
        if !self.nameguard(v.name.clone(), v.loc.clone()) {
            return;
        }
        let f = &mut self.b.macros;

        if let ArtifactType::Header = self.artifact {
        } else {
            write!(f, "#line {} \"{}\"\n", v.loc.line, v.loc.file).unwrap();
        }

        write!(f, "#define {}", v.name).unwrap();
        if v.args.len() > 0  {
            write!(f, "({})", v.args.join(",")).unwrap();
        }
        write!(f, " {}\n", v.body[1..v.body.len()-1].replace("\n", "\\\n")).unwrap();
    }

    pub fn struc(&mut self, s: &Struct) {
        if !self.nameguard(s.name.clone(), s.loc.clone()) {
            return;
        }
        let f = &mut self.b.structs;

        write!(f, "typedef struct \n").unwrap();
        if let ArtifactType::Header = self.artifact {
        } else {
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
        if !self.nameguard(v.name.clone(), v.loc.clone()) {
            return;
        }

        let f = &mut self.b.decls;

        let mut ns = ns.clone();
        ns.push(v.name.clone());
        let mut fqn = ns.join("_");

        if let ArtifactType::Header = self.artifact {
        } else {
            write!(f, "#line {} \"{}\"\n", v.loc.line, v.loc.file).unwrap();
        }

        if v.name == "main" {
            fqn = "main".into();
        }
        match &v.vis {
            Visibility::Object  => {
                fqn = v.name.clone();
                write!(f, "static ").unwrap();
            },
            _ => (),
        };
        match &v.ret {
            None       => write!(f, "void "),
            Some(a)    => write!(f, "{} ", &a.typ),
        }.unwrap();
        write!(f, "{} (", fqn).unwrap();
        Self::function_args(f, &v);
        write!(f, ");\n").unwrap();

        if let ArtifactType::Header = self.artifact {
            return;
        }
        if let Visibility::Object = v.vis {
            return;
        }
        if fqn == "main" {
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

                write!(f, ");}} \n").unwrap();
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
        if !self.included.insert(i.expr.clone()) {
            return;
        }
        if let ArtifactType::Header = self.artifact {
        } else {
            write!(f, "#line {} \"{}\"\n", i.loc.line, i.loc.file).unwrap();
        }
        write!(f, "#include {}\n", i.expr).unwrap();
    }

}

