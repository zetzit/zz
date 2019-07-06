use std::fs;
use super::flatten;
use super::ast;
use super::project::ArtifactType;
use std::io::Write;
use std::collections::HashSet;
use std::path::PathBuf;


pub struct CFile {
    pub name:       String,
    pub filepath:   String,
    pub sources:    HashSet<PathBuf>,
}

pub struct Emitter{
    p:              String,
    f:              fs::File,
    name:           String,
    module:         Option<flatten::FlatModule>,
    artifact:       ArtifactType,
}

impl Emitter {
    pub fn new(name: String, module: flatten::FlatModule, artifact: ArtifactType) -> Self {
        let p = if let ArtifactType::Header = artifact {
            format!("target/include/{}.h", name)
        } else {
            format!("target/zz/{}.c", name)
        };
        let f = fs::File::create(&p).expect(&format!("cannot create {}", p));

        Emitter{
            p,
            name,
            f,
            artifact,
            module: Some(module),
        }
    }

    pub fn emit(mut self) -> CFile {
        if let ArtifactType::Header = self.artifact {
            write!(self.f, "#ifndef ZZ_EXPORT_HEADER_{}\n#define ZZ_EXPORT_HEADER_{}\n", self.name, self.name).unwrap();
        }
        let module = std::mem::replace(&mut self.module, None).unwrap();

        for (name,v) in module.includes {
            write!(self.f, "\n//{}\n", name).unwrap();
            self.emit_include(&v);
        }


        for decl in &module.locals {
            match decl.ast.def {
                ast::Def::Macro{..} => {
                    self.emit_macro(decl)
                }
                _ => (),
            }
        }

        for decl in &module.locals {
            match decl.ast.def {
                ast::Def::Const {..} => {
                    self.emit_const(decl)
                }
                _ => (),
            }
        }

        for decl in &module.locals {
            match decl.ast.def {
                ast::Def::Static {..} => {
                    self.emit_static(decl)
                },
                _ => (),
            }
        };

        for decl in &module.locals {
            match decl.ast.def {
                ast::Def::Struct {..} => {
                    self.emit_struct(decl)
                },
                _ => (),
            }
        };

        for decl in &module.locals {
            match decl.ast.def {
                ast::Def::Function{..} => {
                    self.emit_decl(decl);
                },
                _ => (),
            }
        };

        for decl in &module.locals {
            match decl.ast.def {
                ast::Def::Function{..} => {
                    if !decl.foreign {
                        self.emit_def(decl);
                    }
                },
                _ => (),
            }
        };

        if let ArtifactType::Header = self.artifact {
            write!(self.f, "\n#endif\n").unwrap();
        }

        CFile {
            name:     self.name,
            filepath: self.p,
            sources:  module.sources,
        }
    }


    pub fn emit_include(&mut self, i: &ast::Include) {
        if let ArtifactType::Header = self.artifact {
        } else {
            write!(self.f, "#line {} \"{}\"\n", i.loc.line(), i.loc.file).unwrap();
        }
        write!(self.f, "#include {}\n", i.expr).unwrap();
    }

    pub fn emit_static(&mut self, v: &flatten::FlatLocal) {
        if let ArtifactType::Header = self.artifact {
        } else {
            write!(self.f, "#line {} \"{}\"\n", v.ast.loc.line(), v.ast.loc.file).unwrap();
        }

        let (typeref, expr, muta, storage) = match &v.ast.def {
            ast::Def::Static{typeref, expr, muta, storage} => (typeref, expr, muta, storage),
            _ => unreachable!(),
        };

        if *muta {
            write!(self.f, "static ").unwrap();
        } else {
            write!(self.f, "const ").unwrap();
        }

        match storage {
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
            write!(self.f, "\n#line {} \"{}\"\n", expr.loc.line(), expr.loc.file).unwrap();
        }
        write!(self.f, "{} {} __attribute__ ((visibility (\"hidden\"))) = {};\n", typeref.name, v.ast.name, expr.expr).unwrap();
    }

    pub fn emit_const(&mut self, v: &flatten::FlatLocal) {
        let (typeref, expr) = match &v.ast.def {
            ast::Def::Const{typeref, expr} => (typeref, expr),
            _ => unreachable!(),
        };

        if let ArtifactType::Header = self.artifact {
        } else {
            write!(self.f, "#line {} \"{}\"\n", expr.loc.line(), expr.loc.file).unwrap();
        }

        write!(self.f, "#define {} (({}){})\n", v.ast.name, typeref.name, expr.expr.replace("\n", "\\\n")).unwrap();
    }

    pub fn emit_struct(&mut self, v: &flatten::FlatLocal) {
        let (fields, packed) = match &v.ast.def {
            ast::Def::Struct{fields, packed} => (fields, packed),
            _ => unreachable!(),
        };

        if let ArtifactType::Header = self.artifact {
        } else {
            write!(self.f, "#line {} \"{}\"\n", v.ast.loc.line(), v.ast.loc.file).unwrap();
        }
        write!(self.f, "typedef struct \n").unwrap();

        write!(self.f, "{{\n").unwrap();
        for field in fields {
            write!(self.f, "#line {} \"{}\"\n", field.expr.loc.line(), field.expr.loc.file).unwrap();
            write!(self.f, "{} {}\n",
                   &field.typeref.name, field.expr.expr,
                   ).unwrap();
        }

        write!(self.f, "}} {} ;\n", v.ast.name).unwrap();
    }



    pub fn emit_macro(&mut self, v: &flatten::FlatLocal) {
        let (args, body) = match &v.ast.def {
            ast::Def::Macro{args, body, ..} => (args, body),
            _ => unreachable!(),
        };

        if let ArtifactType::Header = self.artifact {
        } else {
            write!(self.f, "#line {} \"{}\"\n", body.loc.line(), body.loc.file).unwrap();
        }

        write!(self.f, "#define {}", v.ast.name).unwrap();
        if args.len() > 0  {
            write!(self.f, "({})", args.join(",")).unwrap();
        }
        write!(self.f, " {}\n", body.expr[1..body.expr.len()-1].replace("\n", "\\\n")).unwrap();
    }

    fn function_args(&mut self, args: &Vec<ast::NamedArg>) {
        let mut first = true ;
        for arg in args {
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

    pub fn emit_decl(&mut self, v: &flatten::FlatLocal) {
        let (ret, args, body ) = match &v.ast.def {
            ast::Def::Function{ret, args, body} => (ret, args, body),
            _ => unreachable!(),
        };

        if let ArtifactType::Header = self.artifact {
        } else {
            write!(self.f, "#line {} \"{}\"\n", v.ast.loc.line(), v.ast.loc.file).unwrap();
        }

        match &v.ast.vis {
            ast::Visibility::Object  => {
                write!(self.f, "static ").unwrap();
            },
            _ => (),
        };

        match &ret {
            None       => write!(self.f, "void ").unwrap(),
            Some(a)    => {
                write!(self.f, "{} ", &a.typeref.name).unwrap();
            }
        };

        match &v.ast.vis {
            ast::Visibility::Object => (),
            ast::Visibility::Shared => write!(self.f, "__attribute__ ((visibility (\"hidden\"))) ").unwrap(),
            ast::Visibility::Export => write!(self.f, "__attribute__ ((visibility (\"default\"))) ").unwrap(),
        }

        write!(self.f, "{} (", v.ast.name).unwrap();
        self.function_args(args);
        write!(self.f, ");\n").unwrap();
    }

    pub fn emit_def(&mut self, v: &flatten::FlatLocal) {
        let (ret, args, body ) = match &v.ast.def {
            ast::Def::Function{ret, args, body} => (ret, args, body),
            _ => unreachable!(),
        };

        if let ArtifactType::Header = self.artifact {
        } else {
            write!(self.f, "#line {} \"{}\"\n", v.ast.loc.line(), v.ast.loc.file).unwrap();
        }

        match &v.ast.vis {
            ast::Visibility::Object  => {
                write!(self.f, "static ").unwrap();
            },
            _ => (),
        };

        match &ret {
            None       => write!(self.f, "void ").unwrap(),
            Some(a)    => {
                write!(self.f, "{} ", &a.typeref.name).unwrap();
            }
        };

        match &v.ast.vis {
            ast::Visibility::Object => (),
            ast::Visibility::Shared => write!(self.f, "__attribute__ ((visibility (\"hidden\"))) ").unwrap(),
            ast::Visibility::Export => write!(self.f, "__attribute__ ((visibility (\"default\"))) ").unwrap(),
        }

        write!(self.f, "{} (", v.ast.name).unwrap();
        self.function_args(args);
        write!(self.f, ")\n").unwrap();

        if let ArtifactType::Header = self.artifact {
        } else {
            write!(self.f, "#line {} \"{}\"\n", body.loc.line(), body.loc.file).unwrap();
        }

        write!(self.f, "{}\n\n", body.expr).unwrap();
    }



























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


