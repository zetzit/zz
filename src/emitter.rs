use std::fs;
use super::flatten;
use super::ast;
use std::io::Write;
use std::collections::HashSet;
use std::path::PathBuf;
use super::name::Name;


pub struct CFile {
    pub name:       Name,
    pub filepath:   String,
    pub sources:    HashSet<PathBuf>,
    pub deps:       HashSet<Name>,
}

pub struct Emitter{
    p:              String,
    f:              fs::File,
    module:         flatten::Module,
    header:         bool,
}

impl Emitter {
    pub fn new(module: flatten::Module, header: bool) -> Self {
        let p = if header {
            let mut ns = module.name.0.clone();
            ns.remove(0);
            format!("target/include/{}.h", ns.join("_"))
        } else {
            format!("target/zz/{}.c", module.name)
        };
        let f = fs::File::create(&p).expect(&format!("cannot create {}", p));



        Emitter{
            p,
            f,
            header,
            module,
        }
    }

    fn to_local_name(&self, s: &Name) -> String {
        assert!(s.is_absolute());

        if s.0[1] == "libc" {
            return s.0.last().unwrap().clone();
        }

        if self.header {
            let mut s = s.clone();
            s.0.remove(0);
            return s.0.join("_");
        }

        let mut search  = s.clone();
        let mut rem     = Vec::new();
        while search.len() > 1 {

            if self.module.short_names.contains(&search) {
                rem.insert(0, search.pop().unwrap());
                return rem.join("_");
            }
            rem.push(search.pop().unwrap());
        }

        let mut s = s.clone();
        s.0.remove(0);
        return s.0.join("_");
    }

    pub fn emit(mut self) -> CFile {
        let module = self.module.clone();
        debug!("emitting {}", module.name);

        if self.header {
            let headername = module.name.0.join("_");
            write!(self.f, "#ifndef ZZ_EXPORT_HEADER_{}\n#define ZZ_EXPORT_HEADER_{}\n", headername, headername).unwrap();
        }

        for v in &module.d {
            match v {
                flatten::D::Include(i) => {
                    self.emit_include(&i);
                },
                flatten::D::Local(_) => (),
            }
        };

        for v in &module.d {
            match v {
                flatten::D::Include(_) => {},
                flatten::D::Local(d) => {
                    if self.header && d.vis != ast::Visibility::Export {
                        continue
                    }
                    match d.def {
                        ast::Def::Macro{..} => {
                            self.emit_macro(&d)
                        }
                        ast::Def::Const{..} => {
                            self.emit_const(&d)
                        }
                        ast::Def::Static{..} => {
                            self.emit_static(&d)
                        }
                        ast::Def::Struct{..} => {
                            self.emit_struct(&d)
                        }
                        ast::Def::Function{..} => {
                            if !d.name.ends_with("::main") {
                                self.emit_decl(&d);
                            }
                        }
                    }
                    write!(self.f, "\n").unwrap();
                }
            }
        }

        if self.header {
            write!(self.f, "\n#endif\n").unwrap();
        } else {
            for v in &module.d {
                if let flatten::D::Local(d)  = v {
                    if let ast::Def::Function{..} = d.def {
                        let mut name = Name::from(&d.name);
                        name.pop();
                        if name == module.name {
                            self.emit_def(&d);
                        }
                    }
                }
            }
        }



        CFile {
            name:       module.name,
            filepath:   self.p,
            sources:    module.sources,
            deps:       module.deps,
        }
    }


    pub fn emit_include(&mut self, i: &ast::Include) {
        if !self.header {
            write!(self.f, "#line {} \"{}\"\n", i.loc.line(), i.loc.file).unwrap();
        }
        write!(self.f, "#include {}\n", i.expr).unwrap();
    }

    pub fn emit_macro(&mut self, v: &ast::Local) {
        let (args, body) = match &v.def {
            ast::Def::Macro{args, body, ..} => (args, body),
            _ => unreachable!(),
        };

        if !self.header {
            write!(self.f, "#line {} \"{}\"\n", body.loc.line(), body.loc.file).unwrap();
        }

        write!(self.f, "#define {}", self.to_local_name(&Name::from(&v.name))).unwrap();
        if args.len() > 0  {
            write!(self.f, "({})", args.join(",")).unwrap();
        }
        write!(self.f, " {}\n", body.expr[1..body.expr.len()-1].replace("\n", "\\\n")).unwrap();
    }

    pub fn emit_static(&mut self, ast: &ast::Local) {
        if !self.header {
            write!(self.f, "#line {} \"{}\"\n", ast.loc.line(), ast.loc.file).unwrap();
        }

        let (typeref, expr, muta, storage, ptr) = match &ast.def {
            ast::Def::Static{typeref, expr, muta, storage, ptr} => (typeref, expr, muta, storage, ptr),
            _ => unreachable!(),
        };

        if *muta {
            write!(self.f, "static ").unwrap();
        } else {
            write!(self.f, "static const ").unwrap();
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

        if !self.header {
            write!(self.f, "\n#line {} \"{}\"\n", expr.loc.line(), expr.loc.file).unwrap();
        }
        write!(self.f, "{} ", self.to_local_name(&typeref.name)).unwrap();
        if *ptr {
            write!(self.f, "* ").unwrap();
        }
        write!(self.f, "{} = {};\n",
            self.to_local_name(&Name::from(&ast.name)), expr.expr).unwrap();
    }

    pub fn emit_const(&mut self, ast: &ast::Local) {
        let (typeref, expr, ptr) = match &ast.def {
            ast::Def::Const{typeref, expr, ptr} => (typeref, expr, ptr),
            _ => unreachable!(),
        };

        if !self.header {
            write!(self.f, "#line {} \"{}\"\n", expr.loc.line(), expr.loc.file).unwrap();
        }

        write!(self.f, "static const {} ", self.to_local_name(&typeref.name)).unwrap();
        if *ptr {
            write!(self.f, "* ").unwrap();
        }

        write!(self.f, "{} = ", self.to_local_name(&Name::from(&ast.name))).unwrap();
        write!(self.f, "({});\n", expr.expr.replace("\n", "\\\n")).unwrap();
    }

    pub fn emit_struct(&mut self, ast: &ast::Local) {
        let (fields, packed) = match &ast.def {
            ast::Def::Struct{fields, packed} => (fields, packed),
            _ => unreachable!(),
        };

        if !self.header {
            write!(self.f, "#line {} \"{}\"\n", ast.loc.line(), ast.loc.file).unwrap();
        }
        write!(self.f, "typedef struct \n").unwrap();

        write!(self.f, "{{\n").unwrap();
        for field in fields {
            if !self.header {
                write!(self.f, "#line {} \"{}\"\n", field.expr.loc.line(), field.expr.loc.file).unwrap();
            }
            write!(self.f, "   {} {}\n",
                   self.to_local_name(&field.typeref.name), field.expr.expr,
                   ).unwrap();
        }

        write!(self.f, "}} {} ;\n", self.to_local_name(&Name::from(&ast.name))).unwrap();
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

            write!(self.f, "{}", self.to_local_name(&arg.typeref.name)).unwrap();

            if arg.ptr {
                write!(self.f, "* ").unwrap();
            }

            write!(self.f, " {}", arg.name).unwrap();
        }
    }

    pub fn emit_decl(&mut self, ast: &ast::Local) {
        let (ret, args, _body ) = match &ast.def {
            ast::Def::Function{ret, args, body} => (ret, args, body),
            _ => unreachable!(),
        };

        // declare the fqn

        if !self.header {
            write!(self.f, "#line {} \"{}\"\n", ast.loc.line(), ast.loc.file).unwrap();
        }

        match &ast.vis {
            ast::Visibility::Object  => {
                write!(self.f, "static ").unwrap();
            },
            _ => (),
        };

        match &ret {
            None       => write!(self.f, "void ").unwrap(),
            Some(a)    => {
                write!(self.f, "{} ", self.to_local_name(&a.typeref.name)).unwrap();
                if a.ptr {
                    write!(self.f, "* ").unwrap();
                }
            }
        };

        match &ast.vis {
            ast::Visibility::Object => (),
            ast::Visibility::Shared => write!(self.f, "__attribute__ ((visibility (\"hidden\"))) ").unwrap(),
            ast::Visibility::Export => write!(self.f, "__attribute__ ((visibility (\"default\"))) ").unwrap(),
        }

        write!(self.f, "{} (", Name::from(&ast.name).0[1..].join("_")).unwrap();

        self.function_args(args);
        write!(self.f, ");\n").unwrap();

        // declare the short local name
        // aliases are broken in clang, so we need to create an inline redirect

        if self.header {
            return;
        }
        write!(self.f, "#line {} \"{}\"\n", ast.loc.line(), ast.loc.file).unwrap();

        write!(self.f, "static inline ").unwrap();

        match &ret {
            None       => write!(self.f, "void ").unwrap(),
            Some(a)    => {
                write!(self.f, "{} ", self.to_local_name(&a.typeref.name)).unwrap();
                if a.ptr {
                    write!(self.f, "* ").unwrap();
                }
            }
        };

        write!(self.f, " __attribute__ ((always_inline, unused)) ").unwrap();


        write!(self.f, "{} (", self.to_local_name(&Name::from(&ast.name))).unwrap();
        self.function_args(args);
        write!(self.f, ")").unwrap();

        write!(self.f, "{{").unwrap();
        if ret.is_some() {
            write!(self.f, "return ").unwrap();
        }

        write!(self.f, "{}(", Name::from(&ast.name).0[1..].join("_")).unwrap();

        let mut first = true;
        for arg in args {
            if first {
                first = false;
            } else {
                write!(self.f, ", ").unwrap();
            }
            write!(self.f, " {}", arg.name).unwrap();
        }

        write!(self.f, ");}} \n").unwrap();
    }

    pub fn emit_def(&mut self, ast: &ast::Local) {
        let (ret, args, body ) = match &ast.def {
            ast::Def::Function{ret, args, body} => (ret, args, body),
            _ => unreachable!(),
        };

        if !self.header {
            write!(self.f, "#line {} \"{}\"\n", ast.loc.line(), ast.loc.file).unwrap();
        }

        if !ast.name.ends_with("::main") {
            match &ast.vis {
                ast::Visibility::Object  => {
                    write!(self.f, "static ").unwrap();
                },
                _ => (),
            };
        }

        match &ret {
            None       => write!(self.f, "void ").unwrap(),
            Some(a)    => {
                write!(self.f, "{} ", self.to_local_name(&a.typeref.name)).unwrap();
                if a.ptr {
                    write!(self.f, "* ").unwrap();
                }
            }
        };



        if ast.name.ends_with("::main") {
            write!(self.f, "main (").unwrap();
        } else  {
            match &ast.vis {
                ast::Visibility::Object => (),
                ast::Visibility::Shared => write!(self.f, "__attribute__ ((visibility (\"hidden\"))) ").unwrap(),
                ast::Visibility::Export => write!(self.f, "__attribute__ ((visibility (\"default\"))) ").unwrap(),
            }
            write!(self.f, "{} (", Name::from(&ast.name).0[1..].join("_")).unwrap();
        }

        self.function_args(args);
        write!(self.f, ")\n").unwrap();

        if !self.header {
            write!(self.f, "#line {} \"{}\"\n", body.loc.line(), body.loc.file).unwrap();
        }

        write!(self.f, "{}\n\n", body.expr).unwrap();
    }
}
