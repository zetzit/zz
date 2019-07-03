use std::fs;
use super::flatten;
use super::ast;
use super::project::ArtifactType;
use std::io::Write;

pub struct Emitter{
    f:              fs::File,
    name:           String,
    module:         Option<flatten::Module>,
    artifact:       ArtifactType,
}

impl Emitter {
    pub fn new(name: String, module: flatten::Module, artifact: ArtifactType) -> Self {
        let p = if let ArtifactType::Header = artifact {
            format!("target/include/{}.h", name)
        } else {
            format!("target/zz/{}.c", name)
        };
        let f = fs::File::create(&p).expect(&format!("cannot create {}", p));

        Emitter{
            name,
            f,
            artifact,
            module: Some(module),
        }
    }

    pub fn emit(mut self) {
        if let ArtifactType::Header = self.artifact {
            write!(self.f, "#ifndef ZZ_EXPORT_HEADER_{}\n#define ZZ_EXPORT_HEADER_{}\n", self.name, self.name).unwrap();
        }
        let module = std::mem::replace(&mut self.module, None).unwrap();




        for (name,v) in module.includes {
            write!(self.f, "\n//{}\n", name).unwrap();
            self.emit_include(&v);
        }


        /*
        for (name,v) in module.f_structs {
            write!(self.f, "\n//{}\n", name).unwrap();
            self.emit_struct(&v);
        }
        for (name,v) in module.f_statics{
            write!(self.f, "\n//{}\n", name).unwrap();
            self.emit_static(&v);
        }
        for (name,v) in module.f_constants{
            write!(self.f, "\n//{}\n", name).unwrap();
            self.emit_constant(&v);
        }
        for (name,v) in module.f_decls{
            write!(self.f, "\n//{}\n", name).unwrap();
            self.emit_decl(&v);
        }
        for (name,v) in module.f_defs{
            write!(self.f, "\n//{}\n", name).unwrap();
            self.emit_def(&v);
        }


        */


        if let ArtifactType::Header = self.artifact {
            write!(self.f, "\n#endif\n").unwrap();
        }
    }


    pub fn emit_include(&mut self, i: &ast::Include) {
        if let ArtifactType::Header = self.artifact {
        } else {
            write!(self.f, "#line {} \"{}\"\n", i.loc.line(), i.loc.file).unwrap();
        }
        write!(self.f, "#include {}\n", i.expr).unwrap();
    }

    /*
    pub fn emit_struct(&mut self, s: &ast::Struct) {
        if let ArtifactType::Header = self.artifact {
        } else {
            write!(self.f, "#line {} \"{}\"\n", s.loc.line(), s.loc.file).unwrap();
        }
        write!(self.f, "typedef struct \n").unwrap();

        write!(self.f, "{{\n").unwrap();
        for field in &s.fields {
            write!(self.f, "#line {} \"{}\"\n", field.expr.loc.line(), field.expr.loc.file).unwrap();
            write!(self.f, "{} {}\n",
                   &field.typeref.name, field.expr.expr,
                   ).unwrap();
        }

        write!(self.f, "}} {} ;\n", s.name).unwrap();
    }

    pub fn emit_static(&mut self, v: &ast::Static) {
        if let ArtifactType::Header = self.artifact {
        } else {
            write!(self.f, "#line {} \"{}\"\n", v.loc.line(), v.loc.file).unwrap();
        }

        if v.muta {
            write!(self.f, "static ").unwrap();
        } else {
            write!(self.f, "const ").unwrap();
        }

        match v.storage {
            ast::Storage::Atomic => {
                write!(self.f, "_Atomic ").unwrap();
            },
            ast::Storage::ThreadLocal => {
                write!(self.f, "_Thread_local ").unwrap();
            },
            ast::Storage::Static  => (),
        }

        if let ArtifactType::Header = self.artifact {
        } else {
            write!(self.f, "\n#line {} \"{}\"\n", v.expr.loc.line(), v.expr.loc.file).unwrap();
        }
        write!(self.f, "{} {} __attribute__ ((visibility (\"hidden\"))) = {};\n", v.typeref.name, v.name, v.expr.expr).unwrap();
    }

    pub fn emit_constant(&mut self, v: &ast::Const) {
        if let ArtifactType::Header = self.artifact {
        } else {
            write!(self.f, "#line {} \"{}\"\n", v.expr.loc.line(), v.expr.loc.file).unwrap();
        }
        write!(self.f, "#define {} (({}){})\n", v.name, v.typeref.name, v.expr.expr.replace("\n", "\\\n")).unwrap();
    }

    pub fn emit_macro(&mut self, v: &ast::Macro) {
        if let ArtifactType::Header = self.artifact {
        } else {
            write!(self.f, "#line {} \"{}\"\n", v.body.loc.line(), v.body.loc.file).unwrap();
        }

        write!(self.f, "#define {}", v.name).unwrap();
        if v.args.len() > 0  {
            write!(self.f, "({})", v.args.join(",")).unwrap();
        }
        write!(self.f, " {}\n", v.body.expr[1..v.body.expr.len()-1].replace("\n", "\\\n")).unwrap();
    }



    fn function_args(&mut self, v: &ast::Function) {
        let mut first = true ;
        for arg in &v.args {
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

            write!(self.f, "{}", arg.typeref.name).unwrap();

            if arg.ptr {
                write!(self.f, " *").unwrap();
            }

            write!(self.f, " {}", arg.name).unwrap();
        }
    }

    pub fn emit_decl(&mut self, v: &ast::Function) {
        if let ArtifactType::Header = self.artifact {
        } else {
            write!(self.f, "#line {} \"{}\"\n", v.loc.line(), v.loc.file).unwrap();
        }

        match &v.vis {
            ast::Visibility::Object  => {
                write!(self.f, "static ").unwrap();
            },
            _ => (),
        };

        match &v.ret {
            None       => write!(self.f, "void ").unwrap(),
            Some(a)    => {
                write!(self.f, "{} ", &a.typeref.name).unwrap();
            }
        };

        match &v.vis {
            ast::Visibility::Object => (),
            ast::Visibility::Shared => write!(self.f, "__attribute__ ((visibility (\"hidden\"))) ").unwrap(),
            ast::Visibility::Export => write!(self.f, "__attribute__ ((visibility (\"default\"))) ").unwrap(),
        }

        write!(self.f, "{} (", v.name).unwrap();
        self.function_args(&v);
        write!(self.f, ");\n").unwrap();
    }

    pub fn emit_def(&mut self, v: &ast::Function) {
        if let ArtifactType::Header = self.artifact {
        } else {
            write!(self.f, "#line {} \"{}\"\n", v.loc.line(), v.loc.file).unwrap();
        }

        match &v.vis {
            ast::Visibility::Object  => {
                write!(self.f, "static ").unwrap();
            },
            _ => (),
        };

        match &v.ret {
            None       => write!(self.f, "void ").unwrap(),
            Some(a)    => {
                write!(self.f, "{} ", &a.typeref.name).unwrap();
            }
        };

        match &v.vis {
            ast::Visibility::Object => (),
            ast::Visibility::Shared => write!(self.f, "__attribute__ ((visibility (\"hidden\"))) ").unwrap(),
            ast::Visibility::Export => write!(self.f, "__attribute__ ((visibility (\"default\"))) ").unwrap(),
        }

        write!(self.f, "{} (", v.name).unwrap();
        self.function_args(&v);
        write!(self.f, ")\n").unwrap();

        if let ArtifactType::Header = self.artifact {
        } else {
            write!(self.f, "#line {} \"{}\"\n", v.body.loc.line(), v.body.loc.file).unwrap();
        }

        write!(self.f, "{}\n\n", v.body.expr).unwrap();
    }


    */




























    /*
        //aliases are broken in clang, so we need to create an inline redirect
        let mut f = Vec::new();

        write!(f, "#line {} \"{}\"\n", v.loc.line, v.loc.file).unwrap();

        write!(f, "static inline ").unwrap();
        write!(f, " __attribute__ ((always_inline, unused)) ").unwrap();
        match &v.ret {
            None       => write!(f, "void ").unwrap(),
            Some(a)    => {
                let name = self.require_local(&a.typeref);
                write!(f, "{} ", &name).unwrap();
            }
        };
        write!(f, "{} (", v.name).unwrap();

        self.function_args(&mut f, &v);
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
        self.b.decls.extend(f);
    }
    */
    /*

    pub fn define(&mut self, v: &Function, ns: &Vec<String>, body: &str) {
        let mut f = Vec::new();

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
            None       => write!(f, "void ").unwrap(),
            Some(a)    => {
                let name = self.require_local(&a.typeref);
                write!(f, "{} ", &name).unwrap();
            }
        };

        match v.vis {
            Visibility::Object => (),
            Visibility::Shared => write!(f, "__attribute__ ((visibility (\"hidden\"))) ").unwrap(),
            Visibility::Export => write!(f, "__attribute__ ((visibility (\"default\"))) ").unwrap(),
        }

        write!(f, "{} (", name).unwrap();
        self.function_args(&mut f, &v);
        write!(f, ") {}\n\n", body).unwrap();

        self.b.defs.extend(f);
    }
    */



}


