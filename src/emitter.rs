use super::project::{Project};
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
    cxx:            bool,
    p:              String,
    f:              fs::File,
    module:         flatten::Module,
    header:         bool,
    inside_macro:   bool,
    cur_loc:        Option<ast::Location>,
}


fn outname(project: &Project, module: &flatten::Module, header: bool) -> (bool, String) {
    let mut cxx = false;
    if let Some(std) = &project.std {
        if std.contains("c++") {
            cxx = true;
        }
    }

    if header {
        let mut ns = module.name.0.clone();
        ns.remove(0);
        (cxx, format!("target/include/{}.h", ns.join("_")))
    } else if cxx {
        (cxx, format!("target/zz/{}.cpp", module.name))
    } else {
        (cxx, format!("target/zz/{}.c", module.name))
    }
}

impl Emitter {
    pub fn new(project: &Project, module: flatten::Module, header: bool) -> Self {

        let (cxx, p) = outname(project, &module, header);
        let f = fs::File::create(&p).expect(&format!("cannot create {}", p));



        Emitter{
            cxx,
            p,
            f,
            header,
            module,
            inside_macro: false,
            cur_loc: None,
        }
    }

    fn emit_loc(&mut self, loc: &ast::Location) {

        if let Some(cur_loc) = &self.cur_loc {
            if cur_loc.file  == loc.file && cur_loc.line() == loc.line() {
                return
            }
        }
        self.cur_loc = Some(loc.clone());

        if self.header {
            return
        }
        if self.inside_macro {
            return;
        }
        write!(self.f, "\n#line {} \"{}\"\n", loc.line(), loc.file).unwrap();
    }

    fn to_local_name(&self, s: &Name) -> String {
        if !s.is_absolute() {
            return s.0.join("_");
        }

        assert!(s.is_absolute(), "ICE not abs: '{}'", s);
        if let Some(an) = self.module.aliases.get(&s) {
            return an.clone();
        }

        if s.0[1] == "ext" {
            return s.0.last().unwrap().clone();
        }

        if self.header {
            let mut s = s.clone();
            s.0.remove(0);
            return s.0.join("_");
        }

        /*
        let mut search  = s.clone();
        let mut rem     = Vec::new();
        while search.len() > 1 {
            if self.module.short_names.contains(&search) {
                rem.insert(0, search.pop().unwrap());
                return rem.join("_");
            }
            rem.push(search.pop().unwrap());
        }
        */

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
        trace!("emit include {:?}", i.fqn);
        self.emit_loc(&i.loc);

        if self.cxx && i.expr.contains(".h>") {
            write!(self.f, "extern \"C\" {{\n").unwrap();
        }
        write!(self.f, "#include {}\n", i.expr).unwrap();
        if self.cxx && i.expr.contains(".h>") {
            write!(self.f, "}}\n").unwrap();
        }

        if i.fqn.len() > 3 {
            write!(self.f, "using namespace {} ;\n", i.fqn.0[3..].join("::")).unwrap();
        }
    }

    pub fn emit_macro(&mut self, v: &ast::Local) {
        self.emit_loc(&v.loc);
        self.inside_macro = true;
        let (args, body) = match &v.def {
            ast::Def::Macro{args, body, ..} => (args, body),
            _ => unreachable!(),
        };

        self.emit_loc(&v.loc);
        write!(self.f, "#define {}", self.to_local_name(&Name::from(&v.name))).unwrap();
        if args.len() > 0  {
            write!(self.f, "({}) \\\n", args.join(",")).unwrap();
        }

        write!(self.f, " ").unwrap();
        self.emit_zblock(&body, false);
        write!(self.f, "\n").unwrap();
        self.inside_macro = false;
    }

    pub fn emit_static(&mut self, ast: &ast::Local) {

        self.emit_loc(&ast.loc);

        let (typed, expr, tags, storage) = match &ast.def {
            ast::Def::Static{typed, expr, tags, storage} => (typed, expr, tags, storage),
            _ => unreachable!(),
        };

        if tags.contains_key("mutable") {
            write!(self.f, "static ").unwrap();
        } else {
            write!(self.f, "static const ").unwrap();
        }

        write!(self.f, " __attribute__ ((unused)) ").unwrap();

        match storage {
            ast::Storage::Atomic => {
                write!(self.f, "_Atomic ").unwrap();
            },
            ast::Storage::ThreadLocal => {
                write!(self.f, "_Thread_local ").unwrap();
            },
            ast::Storage::Static  => (),
        }

        write!(self.f, "{} ", self.to_local_name(&typed.name)).unwrap();
        self.emit_pointer(&typed.ptr);

        write!(self.f, "{} = ", self.to_local_name(&Name::from(&ast.name))).unwrap();
        self.emit_expr(&expr);
        write!(self.f, ";\n").unwrap();
    }

    pub fn emit_const(&mut self, ast: &ast::Local) {
        let (typed, expr) = match &ast.def {
            ast::Def::Const{typed, expr} => (typed, expr),
            _ => unreachable!(),
        };

        self.emit_loc(&ast.loc);

        write!(self.f, "static const ").unwrap();
        write!(self.f, " __attribute__ ((unused)) ").unwrap();
        write!(self.f, "{} ", self.to_local_name(&typed.name)).unwrap();
        self.emit_pointer(&typed.ptr);

        write!(self.f, "{} = ", self.to_local_name(&Name::from(&ast.name))).unwrap();

        self.emit_expr(&expr);
        write!(self.f, ";\n").unwrap();
    }

    pub fn emit_struct(&mut self, ast: &ast::Local) {
        let (fields, packed) = match &ast.def {
            ast::Def::Struct{fields, packed} => (fields, packed),
            _ => unreachable!(),
        };

        self.emit_loc(&ast.loc);
        write!(self.f, "typedef struct \n").unwrap();

        write!(self.f, "{{\n").unwrap();
        for field in fields {
            self.emit_loc(&field.loc);
            write!(self.f, "   {}", self.to_local_name(&field.typed.name)).unwrap();
            self.emit_pointer(&field.typed.ptr);
            write!(self.f, " {}", field.name).unwrap();
            if let Some(array) = &field.array {
                write!(self.f, "[").unwrap();
                self.emit_expr(array);
                write!(self.f, "]").unwrap();
            }
            write!(self.f, " ;\n").unwrap();
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


            write!(self.f, "{}", self.to_local_name(&arg.typed.name)).unwrap();

            self.emit_pointer(&arg.typed.ptr);
            if !arg.tags.contains_key("mutable") {
                write!(self.f, " const ").unwrap();
            }

            write!(self.f, " {}", arg.name).unwrap();
        }
    }

    pub fn emit_decl(&mut self, ast: &ast::Local) {
        let (ret, args, _body, vararg) = match &ast.def {
            ast::Def::Function{ret, args, body, vararg} => (ret, args, body, *vararg),
            _ => unreachable!(),
        };

        // declare the fqn

        self.emit_loc(&ast.loc);

        match &ast.vis {
            ast::Visibility::Object  => {
                write!(self.f, "static ").unwrap();
            },
            _ => (),
        };

        match &ret {
            None       => write!(self.f, "void ").unwrap(),
            Some(a)    => {
                write!(self.f, "{} ", self.to_local_name(&a.typed.name)).unwrap();
                self.emit_pointer(&a.typed.ptr);
            }
        };

        match &ast.vis {
            ast::Visibility::Object => (),
            ast::Visibility::Shared => write!(self.f, "__attribute__ ((visibility (\"hidden\"))) ").unwrap(),
            ast::Visibility::Export => write!(self.f, "__attribute__ ((visibility (\"default\"))) ").unwrap(),
        }

        write!(self.f, "{} (", Name::from(&ast.name).0[1..].join("_")).unwrap();

        self.function_args(args);
        if vararg {
            write!(self.f, ", ...").unwrap();
        }
        write!(self.f, ");\n").unwrap();

        // declare the short local name
        // aliases are broken in clang, so we need to create an inline redirect

        if self.to_local_name(&Name::from(&ast.name)) == Name::from(&ast.name).0[1..].join("_") {
            return;
        }
        if self.header {
            return;
        }
        self.emit_loc(&ast.loc);

        write!(self.f, "static inline ").unwrap();

        match &ret {
            None       => write!(self.f, "void ").unwrap(),
            Some(a)    => {
                write!(self.f, "{} ", self.to_local_name(&a.typed.name)).unwrap();
                self.emit_pointer(&a.typed.ptr);
            }
        };

        write!(self.f, " __attribute__ ((always_inline, unused)) ").unwrap();


        write!(self.f, "{} (", self.to_local_name(&Name::from(&ast.name))).unwrap();
        self.function_args(args);
        if vararg {
            write!(self.f, ", ...").unwrap();
        }
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
        let (ret, args, body, vararg) = match &ast.def {
            ast::Def::Function{ret, args, body, vararg} => (ret, args, body, *vararg),
            _ => unreachable!(),
        };

        self.emit_loc(&ast.loc);

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
                write!(self.f, "{} ", self.to_local_name(&a.typed.name)).unwrap();
                self.emit_pointer(&a.typed.ptr);
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
        if vararg {
            write!(self.f, ", ...").unwrap();
        }
        write!(self.f, ")\n").unwrap();
        self.emit_zblock(&body, true);
        write!(self.f, "\n").unwrap();
    }

    fn emit_statement(&mut self, stm: &ast::Statement) {
        match stm {
            ast::Statement::Mark{..} => {},
            ast::Statement::Break{loc} => {
                self.emit_loc(&loc);
                write!(self.f, "break").unwrap();
            },
            ast::Statement::Goto{loc, label} => {
                self.emit_loc(&loc);
                write!(self.f, "goto {}", label).unwrap();
            }
            ast::Statement::Label{loc, label} => {
                self.emit_loc(&loc);
                write!(self.f, "{}:", label).unwrap();
                if self.inside_macro {
                    write!(self.f, "\\\n").unwrap();
                } else {
                    write!(self.f, "\n").unwrap();
                }
            }
            ast::Statement::Block(b2) => {
                self.emit_zblock(b2, true);
            }
            ast::Statement::For{e1, e2, e3, body}  => {
                write!(self.f, "  for (").unwrap();
                if let Some(expr) = e1 {
                    if self.inside_macro {
                        write!(self.f, "\\\n").unwrap();
                    } else {
                        write!(self.f, "\n").unwrap();
                    }
                    self.emit_statement(expr);
                }
                write!(self.f, ";").unwrap();
                if let Some(expr) = e2 {
                    if self.inside_macro {
                        write!(self.f, "\\\n").unwrap();
                    } else {
                        write!(self.f, "\n").unwrap();
                    }
                    self.emit_statement(expr);
                }
                write!(self.f, ";").unwrap();
                if let Some(expr) = e3 {
                    if self.inside_macro {
                        write!(self.f, "\\\n").unwrap();
                    } else {
                        write!(self.f, "\n").unwrap();
                    }
                    self.emit_statement(expr);
                }
                write!(self.f, ")").unwrap();
                self.emit_zblock(body, true);
            },
            ast::Statement::Cond{expr, body, op}  => {
                write!(self.f, "  {} ", op).unwrap();
                if let Some(expr) = expr {
                    write!(self.f, "(").unwrap();
                    self.emit_expr(expr);
                    write!(self.f, ")").unwrap();
                }
                self.emit_zblock(body, true);
            },
            ast::Statement::Assign{lhs, rhs, op, loc}  => {
                self.emit_loc(&loc);
                self.emit_expr(lhs);
                write!(self.f, "  {} ", op).unwrap();
                self.emit_expr(rhs);
            }
            ast::Statement::Var{assign, loc, typed, name, array, tags}  => {
                self.emit_loc(&loc);
                write!(self.f, "  {}", self.to_local_name(&typed.name)).unwrap();

                self.emit_pointer(&typed.ptr);

                if !tags.contains_key("mutable") {
                    write!(self.f, " const ").unwrap();
                }

                write!(self.f, " {} ", name).unwrap();
                if let Some(array) = &array {
                    write!(self.f, " [ ").unwrap();
                    self.emit_expr(array);
                    write!(self.f, " ] ").unwrap();
                }

                self.emit_loc(&loc);
                if let Some(assign) = &assign {
                    write!(self.f, " = ").unwrap();
                    self.emit_expr(assign);
                }
            }
            ast::Statement::Expr{expr, loc}  => {
                self.emit_loc(&loc);
                self.emit_expr(expr);
            }
            ast::Statement::Return{expr, loc}  => {
                self.emit_loc(&loc);
                write!(self.f, "  return ").unwrap();
                if let Some(expr) = expr {
                    self.emit_expr(expr);
                }
            }
        }
    }

    fn emit_zblock(&mut self, v: &ast::Block, realblock: bool) {
        if realblock {
            if self.inside_macro {
                write!(self.f, "{{\\\n").unwrap();
            } else {
                write!(self.f, "{{\n").unwrap();
            }
        }

        for stm in &v.statements {
            self.emit_statement(stm);
            if self.inside_macro {
                write!(self.f, ";\\\n").unwrap();
            } else {
                write!(self.f, ";\n").unwrap();
            }
        }

        if realblock {
            if self.inside_macro {
                write!(self.f, "}}\\\n").unwrap();
            } else {
                write!(self.f, "\n}}\n").unwrap();
            }
        }
    }

    fn emit_pointer(&mut self, v: &Vec<ast::Pointer>) {
        for ptr in v {
            if !ptr.tags.contains_key("mutable") && !ptr.tags.contains_key("mut") {
                write!(self.f, " const ").unwrap();
            }
            write!(self.f, "* ").unwrap();
        }
    }

    fn emit_expr(&mut self, v: &ast::Expression) {
        match v {
            ast::Expression::ArrayInit{fields,loc} => {
                self.emit_loc(&loc);
                write!(self.f, "{{").unwrap();
                for field in fields {
                    self.emit_expr(field);
                    write!(self.f, ",").unwrap();
                }
                write!(self.f, "}}").unwrap();
            },
            ast::Expression::StructInit{typed, fields,loc} => {
                self.emit_loc(&loc);
                write!(self.f, "    ({}", self.to_local_name(&typed.name)).unwrap();
                write!(self.f, "){{").unwrap();
                for (name, field) in fields {
                    write!(self.f, ".{} = ", name).unwrap();
                    self.emit_expr(field);
                    write!(self.f, ",").unwrap();
                }
                write!(self.f, "}}").unwrap();
            },
            ast::Expression::UnaryPost{expr, loc, op} => {
                write!(self.f, "(").unwrap();
                self.emit_loc(&loc);
                self.emit_expr(expr);
                write!(self.f, "{})", op).unwrap();
            },
            ast::Expression::UnaryPre{expr, loc, op} => {
                write!(self.f, "(").unwrap();
                self.emit_loc(&loc);
                write!(self.f, "{}", op).unwrap();
                self.emit_expr(expr);
                write!(self.f, ")").unwrap();
            },
            ast::Expression::Cast{into,expr,..} => {
                write!(self.f, "    ({}", self.to_local_name(&into.name)).unwrap();
                self.emit_pointer(&into.ptr);
                write!(self.f, ")").unwrap();
                self.emit_expr(expr);
            },
            ast::Expression::Name(name) => {
                self.emit_loc(&name.loc);
                write!(self.f, "    {}", self.to_local_name(&name.name)).unwrap();
            },
            ast::Expression::Literal {loc, v} => {
                self.emit_loc(&loc);
                write!(self.f, "    {}", v).unwrap();
            }
            ast::Expression::Call { loc, name, args} => {
                self.emit_loc(&loc);
                write!(self.f, "    {} (", self.to_local_name(&name.name)).unwrap();

                let mut first = true;
                for arg in args {
                    if first {
                        first = false;
                    } else {
                        write!(self.f, ",").unwrap();
                    }
                    self.emit_expr(arg);
                }
                write!(self.f, "    )").unwrap();
            },
            ast::Expression::InfixOperation {lhs, rhs,..} => {
                write!(self.f, "(").unwrap();
                self.emit_expr(lhs);
                for ((op, loc), rhs) in rhs {
                    self.emit_loc(&loc);
                    write!(self.f, " {}", op).unwrap();
                    self.emit_expr(rhs);
                }
                write!(self.f, "  )").unwrap();
            }
            ast::Expression::MemberAccess {loc, lhs, rhs, op} => {
                self.emit_loc(&loc);
                self.emit_expr(lhs);
                write!(self.f, " {}{}", op, rhs).unwrap();
            }
            ast::Expression::ArrayAccess {loc, lhs, rhs} => {
                self.emit_loc(&loc);
                self.emit_expr(lhs);
                write!(self.f, " [ ").unwrap();
                self.emit_expr(rhs);
                write!(self.f, "]").unwrap();
            }
        }
    }
}
