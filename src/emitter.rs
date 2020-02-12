use super::project::{Project};
use std::fs;
use super::flatten;
use super::ast;
use super::make;
use std::io::{Write, Read};
use std::collections::HashSet;
use std::path::PathBuf;
use super::name::Name;
use super::parser::{self, emit_error};


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
    casedir:        String,
    emit_as_extern: HashSet<Name>,
}

pub fn outname(project: &Project, stage: &make::Stage, module: &flatten::Module, header: bool) -> (bool, String) {
    let mut cxx = false;
    if let Some(std) = &project.std {
        if std.contains("c++") {
            cxx = true;
        }
    }

    if header {
        let mut ns = module.name.0.clone();
        ns.remove(0);
        (cxx, format!("target/{}/include/{}.h", stage, ns.join("_")))
    } else if cxx {
        (cxx, format!("target/{}/zz/{}.cpp", stage, module.name.0.join("_")))
    } else {
        (cxx, format!("target/{}/zz/{}.c", stage, module.name.0.join("_")))
    }
}

impl Emitter {
    pub fn new(project: &Project, stage: make::Stage , module: flatten::Module, header: bool) -> Self {

        let (cxx, p) = outname(project, &stage, &module, header);
        let mut f = fs::File::create(&p).expect(&format!("cannot create {}", p));

        let casedir = format!("target/{}/testcases/{}", stage, module.name.0.join("_"));
        std::fs::remove_dir_all(&casedir).ok();
        std::fs::create_dir_all(&casedir).unwrap();


        write!(f, "#include <stdint.h>\n").unwrap();
        write!(f, "#include <stddef.h>\n").unwrap();
        write!(f, "#include <stdbool.h>\n").unwrap();

        Emitter{
            cxx,
            p,
            f,
            header,
            casedir,
            module,
            inside_macro: false,
            cur_loc: None,
            emit_as_extern: HashSet::new(),
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
        write!(self.f, "\n#line {} \"{}\"\n", loc.line(), loc.file.replace("\\", "\\\\")).unwrap();
    }

    fn to_local_typed_name(&self, name: &ast::Typed) -> String {
        match name.t {
            ast::Type::U8   => "uint8_t".to_string(),
            ast::Type::U16  => "uint16_t".to_string(),
            ast::Type::U32  => "uint32_t".to_string(),
            ast::Type::U64  => "uint64_t".to_string(),
            ast::Type::U128 => "uint128_t".to_string(),
            ast::Type::I8   => "int8_t".to_string(),
            ast::Type::I16  => "int16_t".to_string(),
            ast::Type::I32  => "int32_t".to_string(),
            ast::Type::I64  => "int64_t".to_string(),
            ast::Type::I128 => "int128_t".to_string(),
            ast::Type::Int  => "int".to_string(),
            ast::Type::UInt => "unsigned int".to_string(),
            ast::Type::ISize=> "intptr_t".to_string(),
            ast::Type::USize=> "uintptr_t".to_string(),
            ast::Type::Bool => "bool".to_string(),
            ast::Type::F32  => "float".to_string(),
            ast::Type::F64  => "double".to_string(),
            ast::Type::Other(ref n)   => {
                let mut s = self.to_local_name(&n);
                match &name.tail {
                    ast::Tail::Dynamic | ast::Tail::None | ast::Tail::Bind(_,_)=> {},
                    ast::Tail::Static(v,_) => {
                        s = format!("{}_{}", s, v);
                    }
                }
                s
            }
            ast::Type::ILiteral | ast::Type::ULiteral | ast::Type::Elided => {
                parser::emit_error(
                    "ICE: untyped literal ended up in emitter",
                    &[(name.loc.clone(), format!("this should have been resolved earlier"))]
                    );
                std::process::exit(9);
            }
        }
    }
    fn to_local_name(&self, s: &Name) -> String {

        if !s.is_absolute() {
            return s.0.join("_");
        }

        if self.emit_as_extern.contains(s) {
            return s.0.last().unwrap().clone();
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

        let mut s = s.clone();
        s.0.remove(0);
        return s.0.join("_");
    }

    pub fn emit(mut self) -> CFile {
        let module = self.module.clone();
        debug!("emitting {}", module.name.0.join("_"));

        if self.header {
            let headername = module.name.0.join("_");
            write!(self.f, "#ifndef ZZ_EXPORT_HEADER_{}\n#define ZZ_EXPORT_HEADER_{}\n", headername, headername).unwrap();
        }


        for (d,decl_here,def_here) in &module.d {
            debug!("  {} (decl_here: {})", d.name, decl_here);
            if !decl_here{
                continue
            }
            if self.header && d.vis != ast::Visibility::Export {
                continue
            }
            match d.def {
                ast::Def::Struct{..} => {
                    self.emit_struct_def(&d, *def_here, None);
                    if let Some(vs) = module.typevariants.get(&Name::from(&d.name)) {
                        for v in vs {
                            let mut d = d.clone();
                            d.name = format!("{}_{}", d.name, v);
                            self.emit_struct_def(&d, *def_here, Some(*v));
                        }
                    }
                }
                _ => (),
            }
            write!(self.f, "\n").unwrap();
        }

        for (d, decl_here, def_here) in &module.d {
            if !decl_here{
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
                ast::Def::Enum{..} => {
                    self.emit_enum(&d)
                }
                ast::Def::Fntype{..} => {
                    self.emit_fntype(&d);
                }
                ast::Def::Theory{..} => {
                }
                ast::Def::Testcase {..} => {
                    self.emit_testcase(&d);
                }
                ast::Def::Function{..} => {
                    if !d.name.ends_with("::main") {
                        self.emit_decl(&d);
                    }
                }
                ast::Def::Include {inline,..} => {
                    self.emit_include(&d);
                }
                ast::Def::Struct{..} => {
                    self.emit_struct(&d, *def_here, None);
                    if let Some(vs) = module.typevariants.get(&Name::from(&d.name)) {
                        for v in vs {
                            let mut d = d.clone();
                            d.name = format!("{}_{}", d.name, v);
                            self.emit_struct(&d, *def_here, Some(*v));
                        }
                    }
                }

                _ => (),
            }
        }

        if self.header {
            write!(self.f, "\n#endif\n").unwrap();
        } else {
            for (d, decl_here, def_here) in &module.d {
                if !decl_here{
                    continue
                }
                if !def_here {
                    continue
                }
                match d.def {
                    ast::Def::Function{..} => {
                        let mut name = Name::from(&d.name);
                        name.pop();
                        if name == module.name {
                            self.emit_def(&d);
                        }
                    }
                    _ => (),
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



    pub fn emit_include(&mut self, ast: &ast::Local) {

        self.emit_loc(&ast.loc);
        let (expr, loc, fqn, inline, _needs) = match &ast.def {
            ast::Def::Include{expr, loc, fqn, inline, needs} => (expr, loc, fqn, inline, needs),
            _ => unreachable!(),
        };

        trace!("    emit include {} (inline? {})", fqn, inline);
        if *inline {
            let mut f = match fs::File::open(&expr) {
                Err(e) => {
                    parser::emit_error(
                        format!("cannot inline {:?}", expr),
                        &[(loc.clone(), format!("{}", e))]
                        );
                    std::process::exit(9);

                },
                Ok(f) => f,
            };


            let mut v = Vec::new();
            f.read_to_end(&mut v).expect(&format!("read {:?}", expr));

            if !self.inside_macro {
                write!(self.f, "\n#line 1 \"{}\"\n", expr.replace("\\", "\\\\")).unwrap();
            }
            self.f.write_all(&v).unwrap();


            return;
        }
        self.emit_loc(&loc);

        if self.cxx && expr.contains(".h>") {
            write!(self.f, "extern \"C\" {{\n").unwrap();
        }
        write!(self.f, "#include {}\n", expr).unwrap();
        if self.cxx && expr.contains(".h>") {
            write!(self.f, "}}\n").unwrap();
        }

        if fqn.len() > 3 {
            write!(self.f, "using namespace {} ;\n", fqn.0[3..].join("::")).unwrap();
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

        let (typed, expr, tags, storage, array) = match &ast.def {
            ast::Def::Static{typed, expr, tags, storage, array} => (typed, expr, tags, storage, array),
            _ => unreachable!(),
        };

        if tags.contains_key("mut") {
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

        write!(self.f, "{} ", self.to_local_typed_name(&typed)).unwrap();
        self.emit_pointer(&typed.ptr);

        write!(self.f, "{} ", self.to_local_name(&Name::from(&ast.name))).unwrap();

        if let Some(array) = &array {
            write!(self.f, " [ ").unwrap();
            if let Some(array) = &array {
                self.emit_expr(array);
            }
            write!(self.f, " ] ").unwrap();
        }

        write!(self.f, "=").unwrap();
        self.emit_expr(&expr);
        write!(self.f, ";\n").unwrap();
    }

    pub fn emit_const(&mut self, ast: &ast::Local) {
        let (typed, expr) = match &ast.def {
            ast::Def::Const{typed, expr} => (typed, expr),
            _ => unreachable!(),
        };

        self.emit_loc(&ast.loc);


        write!(self.f, "#define {} ((", self.to_local_name(&Name::from(&ast.name))).unwrap();
        write!(self.f, "{} ", self.to_local_typed_name(&typed)).unwrap();
        self.emit_pointer(&typed.ptr);



        write!(self.f, ")").unwrap();
        self.emit_expr(&expr);
        write!(self.f, ")\n").unwrap();
    }

    pub fn emit_enum(&mut self, ast: &ast::Local) {
        let names = match &ast.def {
            ast::Def::Enum{names} => (names),
            _ => unreachable!(),
        };
        self.emit_loc(&ast.loc);
        write!(self.f, "typedef enum {{\n").unwrap();
        for (name, literal) in names {
            write!(self.f, "    {}_{}",
                   self.to_local_name(&Name::from(&ast.name)),
                   name
                   ).unwrap();
            if let Some(literal) = literal {
                write!(self.f, " = {}", literal).unwrap();
            }
            write!(self.f, ",\n").unwrap();
        }
        write!(self.f, "\n}} {};\n", self.to_local_name(&Name::from(&ast.name))).unwrap();
    }

    fn emit_testcase(&mut self, ast: &ast::Local) {
        let fields = match &ast.def {
            ast::Def::Testcase{fields} => fields,
            _ => unreachable!(),
        };

        let testname = Name::from(&ast.name).0.last().cloned().unwrap();
        let dir = format!("{}/{}", self.casedir, testname);
        std::fs::remove_dir_all(&dir).ok();
        std::fs::create_dir_all(&dir).unwrap();
        for (fname, expr) in fields {
            let p = format!("{}/{}", dir, fname);
            let mut f = fs::File::create(&p).expect(&format!("cannot create {}", p));
            match expr {
                ast::Expression::LiteralString{v,..} => {
                    f.write_all(v).unwrap();
                },
                ast::Expression::ArrayInit{fields, ..} => {
                    for field in fields {
                        match field.as_ref() {
                            ast::Expression::LiteralChar{v,..} => {
                                f.write(&[*v as u8]).unwrap();
                            },
                            ast::Expression::Literal{v,loc} => {
                                match parser::parse_u64(v) {
                                    Some(v) if v <= 255 => {
                                        f.write(&[v as u8]).unwrap();
                                    }
                                    _ => {
                                        parser::emit_error(
                                            "testcase field must be literal string or byte array",
                                            &[(loc.clone(), format!("this expression cannot be emitted as testcase file"))]
                                            );
                                        std::process::exit(9);
                                    }
                                }
                            },
                            _ => {
                                parser::emit_error(
                                    "testcase field must be literal string or byte array, not",
                                    &[(field.loc().clone(), format!("this expression cannot be emitted as testcase file"))]
                                    );
                                std::process::exit(9);
                            }
                        }
                    }
                },
                ast::Expression::Literal{v, ..} => {
                    f.write_all(v.as_bytes()).unwrap();
                }
                _ => {
                    parser::emit_error(
                        "testcase field must be literal string or byte array",
                        &[(expr.loc().clone(), format!("this expression cannot be emitted as testcase file"))]
                        );
                    std::process::exit(9);
                }
            }
        }


    }


    pub fn emit_struct_def(&mut self, ast: &ast::Local, def_here: bool, tail_variant: Option<u64>) {
        let (fields, packed, _tail, union) = match &ast.def {
            ast::Def::Struct{fields, packed, tail, union, ..} => (fields, packed, tail, union),
            _ => unreachable!(),
        };

        self.emit_loc(&ast.loc);
        if *union {
            write!(self.f, "union ").unwrap();
        } else {
            write!(self.f, "struct ").unwrap();
        }
        write!(self.f, "{}_t;\n", self.to_local_name(&Name::from(&ast.name))).unwrap();

        write!(self.f, "typedef ").unwrap();
        if *union {
            write!(self.f, "union ").unwrap();
        } else {
            write!(self.f, "struct ").unwrap();
        }
        write!(self.f, "{}_t {};\n",
            self.to_local_name(&Name::from(&ast.name)),
            self.to_local_name(&Name::from(&ast.name))
        ).unwrap();

    }

    pub fn emit_struct(&mut self, ast: &ast::Local, def_here: bool, tail_variant: Option<u64>) {
        let (fields, packed, _tail, union) = match &ast.def {
            ast::Def::Struct{fields, packed, tail, union, ..} => (fields, packed, tail, union),
            _ => unreachable!(),
        };

        self.emit_loc(&ast.loc);
        if *union {
            write!(self.f, "union ").unwrap();
        } else {
            write!(self.f, "struct ").unwrap();
        }
        write!(self.f, "{}_t ", self.to_local_name(&Name::from(&ast.name))).unwrap();




        write!(self.f, "{{\n").unwrap();
        let mut emitted_tail = false;
        for i in 0..fields.len() {
            let field = &fields[i];
            self.emit_loc(&field.loc);
            write!(self.f, "   {}", self.to_local_typed_name(&field.typed)).unwrap();
            self.emit_pointer(&field.typed.ptr);
            if let Some(array) = &field.array {
                if let Some(expr) = array {
                    write!(self.f, " {}", field.name).unwrap();
                    write!(self.f, "[").unwrap();
                    self.emit_expr(expr);
                    write!(self.f, "]").unwrap();
                } else {
                    if i != (fields.len() - 1) {
                        parser::emit_error(
                            "tail field has no be the last field in a struct",
                            &[(field.loc.clone(), format!("tail field would displace next field"))]
                            );
                        std::process::exit(9);
                    }
                    if let Some(tt) = tail_variant {
                        emitted_tail = true;
                        write!(self.f, " {}[{}]", field.name, tt).unwrap();
                    } else {

                        //TODO emit as something else (not as pointer!)
                        // nested flexible arrays are non standard
                        write!(self.f, " {}[]", field.name).unwrap();
                    }
                }
            } else {
                write!(self.f, " {}", field.name).unwrap();
            }
            write!(self.f, " ;\n").unwrap();
        }
        if let Some(tt) = tail_variant {
            if !emitted_tail {
                write!(self.f, "   uint8_t _____tail [{}];\n", tt).unwrap();
            }
        }
        write!(self.f, "}}\n").unwrap();

        if *packed {
            write!(self.f, " __attribute__((__packed__)) ").unwrap();
        }

        write!(self.f, ";\n").unwrap();

        if def_here {
            if ast.vis == ast::Visibility::Export {
                write!(self.f, "const size_t sizeof_{} = sizeof({});\n",
                self.to_local_name(&Name::from(&ast.name)),
                self.to_local_name(&Name::from(&ast.name)),
                ).unwrap();
            }
        }
    }




    fn function_args(&mut self, args: &Vec<ast::NamedArg>) {
        let mut first = true ;
        for arg in args {
            if first {
                first = false;
            } else {
                write!(self.f, ", ").unwrap();
            }


            write!(self.f, "{}", self.to_local_typed_name(&arg.typed)).unwrap();

            self.emit_pointer(&arg.typed.ptr);
            if !arg.tags.contains_key("mut") {
                write!(self.f, " const ").unwrap();
            }

            write!(self.f, " {}", arg.name).unwrap();
        }
    }

    pub fn emit_fntype(&mut self, ast: &ast::Local) {
        let (ret, args, vararg, attr) = match &ast.def {
            ast::Def::Fntype{ret, args, vararg, attr,..} => (ret, args, *vararg, attr),
            _ => unreachable!(),
        };

        self.emit_loc(&ast.loc);
        write!(self.f, "typedef ").unwrap();

        match &ret {
            None       => write!(self.f, "void ").unwrap(),
            Some(a)    => {
                write!(self.f, "{} ", self.to_local_typed_name(&a.typed)).unwrap();
                self.emit_pointer(&a.typed.ptr);
            }
        };

        for (attr, loc) in attr {
            match attr.as_str() {
                o => {
                    parser::emit_error(
                        "ICE: unsupported attr",
                        &[(loc.clone(), format!("'{}' is not a valid c attribute", o))]
                        );
                    std::process::exit(9);

                }
            }
        }

        write!(self.f, "(*{}) (", self.to_local_name(&Name::from(&ast.name))).unwrap();

        self.function_args(args);
        if vararg {
            write!(self.f, ", ...").unwrap();
        }
        write!(self.f, ");\n").unwrap();

    }

    pub fn emit_decl(&mut self, ast: &ast::Local) {
        let (ret, args, _body, vararg, attr) = match &ast.def {
            ast::Def::Function{ret, args, body, vararg, attr, ..} => (ret, args, body, *vararg, attr),
            _ => unreachable!(),
        };

        // declare the fqn

        self.emit_loc(&ast.loc);

        match &ast.vis {
            ast::Visibility::Object  => {
                write!(self.f, "static ").unwrap();
            },
            ast::Visibility::Shared  => {
                write!(self.f, "extern ").unwrap();
            },
            _ => (),
        };

        match &ret {
            None       => write!(self.f, "void ").unwrap(),
            Some(a)    => {
                write!(self.f, "{} ", self.to_local_typed_name(&a.typed)).unwrap();
                self.emit_pointer(&a.typed.ptr);
            }
        };

        match &ast.vis {
            ast::Visibility::Object => (),
            ast::Visibility::Shared => write!(self.f, "__attribute__ ((visibility (\"hidden\"))) ").unwrap(),
            ast::Visibility::Export => write!(self.f, "__attribute__ ((visibility (\"default\"))) ").unwrap(),
        }

        let mut name = Name::from(&ast.name);
        for (attr, loc) in attr {
            match attr.as_str() {
                "extern" => {
                    self.emit_as_extern.insert(name.clone());
                    name = Name::from(name.0.last().unwrap());
                    name.0.insert(0, String::new());
                },
                "inline" => {
                },
                o => {
                    parser::emit_error(
                        "ICE: unsupported attr",
                        &[(loc.clone(), format!("'{}' is not a valid c attribute", o))]
                        );
                    std::process::exit(9);

                }
            }
        }

        write!(self.f, "{} (", name.0[1..].join("_")).unwrap();

        self.function_args(args);
        if vararg {
            write!(self.f, ", ...").unwrap();
        }
        write!(self.f, ");\n").unwrap();


        // declare the aliased local name
        // aliases are broken in clang, so we need to create an inline redirect

        if self.emit_as_extern.contains(&Name::from(&ast.name)) {
            return;
        }

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
                write!(self.f, "{} ", self.to_local_typed_name(&a.typed)).unwrap();
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
        let (ret, args, body, vararg, attr) = match &ast.def {
            ast::Def::Function{ret, args, body, vararg, attr, ..} => (ret, args, body, *vararg, attr),
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

        let mut name = Name::from(&ast.name);
        for (attr, loc) in attr {
            match attr.as_str() {
                "extern" => {
                    self.emit_as_extern.insert(name.clone());
                    name = Name::from(name.0.last().unwrap());
                    name.0.insert(0, String::new());
                },
                "inline" => {
                    write!(self.f, " inline ").unwrap();
                },
                o => {
                    parser::emit_error(
                        "ICE: unsupported attr",
                        &[(loc.clone(), format!("'{}' is not a valid c attribute", o))]
                        );
                    std::process::exit(9);

                }
            }
        }

        match &ret {
            None       => write!(self.f, "void ").unwrap(),
            Some(a)    => {
                write!(self.f, "{} ", self.to_local_typed_name(&a.typed)).unwrap();
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
            write!(self.f, "{} (", name.0[1..].join("_")).unwrap();
        }



        self.function_args(args);
        if vararg {
            write!(self.f, ", ...").unwrap();
        }
        write!(self.f, ")\n").unwrap();
        self.emit_zblock(&body, true);
        write!(self.f, "\n").unwrap();
    }

    fn emit_statement(&mut self, stm: &ast::Statement) -> bool /* ends with semicolon */ {
        match stm {
            ast::Statement::Mark{..} => {false},
            ast::Statement::Break{loc} => {
                self.emit_loc(&loc);
                write!(self.f, "break").unwrap();
                true
            },
            ast::Statement::Label{loc, label} => {
                self.emit_loc(&loc);
                write!(self.f, "{}:", label).unwrap();
                if self.inside_macro {
                    write!(self.f, "\\\n").unwrap();
                } else {
                    write!(self.f, "\n").unwrap();
                }
                false
            }
            ast::Statement::Unsafe(b2) => {
                self.emit_zblock(b2, true);
                false
            }
            ast::Statement::CBlock{loc,lit} => {
                self.emit_loc(&loc);
                write!(self.f, "{}", lit).unwrap();
                false
            }
            ast::Statement::Block(b2) => {
                self.emit_zblock(b2, true);
                false
            }
            ast::Statement::For{e1, e2, e3, body}  => {
                write!(self.f, "  for (").unwrap();
                let mut first = true;
                for expr in e1 {
                    if first {
                        first = false;
                    } else {
                        write!(self.f, ",").unwrap();
                    }
                    if self.inside_macro {
                        write!(self.f, "\\\n").unwrap();
                    } else {
                        write!(self.f, "\n").unwrap();
                    }
                    self.emit_statement(expr);
                }
                write!(self.f, ";").unwrap();

                if let Some(expr) = e2 {
                    self.emit_expr(expr);
                }
                write!(self.f, ";").unwrap();

                let mut first = true;
                for expr in e3 {
                    if first {
                        first = false;
                    } else {
                        write!(self.f, ",").unwrap();
                    }
                    if self.inside_macro {
                        write!(self.f, "\\\n").unwrap();
                    } else {
                        write!(self.f, "\n").unwrap();
                    }
                    self.emit_statement(expr);
                }
                write!(self.f, ")").unwrap();
                self.emit_zblock(body, true);
                false
            },
            ast::Statement::While{expr, body}  => {
                write!(self.f, "while (").unwrap();
                self.emit_expr(expr);
                write!(self.f, ")").unwrap();
                self.emit_zblock(body, true);
                false
            },
            ast::Statement::If{branches} => {
                if branches.len() < 1 {
                    return false;
                }
                let mut branches = branches.iter();

                write!(self.f, "if (").unwrap();
                let ifc = branches.next().unwrap();
                self.emit_expr(ifc.1.as_ref().unwrap());
                write!(self.f, ")").unwrap();
                self.emit_zblock(&ifc.2, true);

                for branch in branches {
                    if let Some(expr) = &branch.1 {
                        write!(self.f, " else if (").unwrap();
                        self.emit_expr(expr);
                        write!(self.f, ")").unwrap();
                    } else {
                        write!(self.f, " else ").unwrap();
                    }
                    self.emit_zblock(&branch.2, true);
                }

                false

            }
            ast::Statement::Assign{lhs, rhs, loc, op}  => {
                self.emit_loc(&loc);
                self.emit_expr(lhs);
                write!(self.f, " {} ", match op {
                    ast::AssignOperator::Bitor  => "|=",
                    ast::AssignOperator::Bitand => "&=",
                    ast::AssignOperator::Add    => "+=",
                    ast::AssignOperator::Sub    => "-=",
                    ast::AssignOperator::Eq     => "=" ,
                }).unwrap();
                self.emit_expr(rhs);
                true
            }
            ast::Statement::Var{assign, loc, typed, name, array, tags}  => {
                self.emit_loc(&loc);
                write!(self.f, "  {}", self.to_local_typed_name(&typed)).unwrap();

                self.emit_pointer(&typed.ptr);

                if !tags.contains_key("mut") {
                    write!(self.f, " const ").unwrap();
                }

                write!(self.f, " {} ", name).unwrap();
                if let Some(array) = &array {
                    write!(self.f, " [ ").unwrap();
                    if let Some(array) = &array {
                        self.emit_expr(array);
                    }
                    write!(self.f, " ] ").unwrap();
                }

                self.emit_loc(&loc);
                if let Some(assign) = &assign {
                    write!(self.f, " = ").unwrap();
                    self.emit_expr(assign);
                }
                true
            }
            ast::Statement::Expr{expr, loc}  => {
                self.emit_loc(&loc);
                self.emit_expr(expr);
                true
            }
            ast::Statement::Continue{loc}  => {
                self.emit_loc(&loc);
                write!(self.f, "continue").unwrap();
                true
            }
            ast::Statement::Return{expr, loc}  => {
                self.emit_loc(&loc);
                write!(self.f, "  return ").unwrap();
                if let Some(expr) = expr {
                    self.emit_expr(expr);
                }
                true
            }
            ast::Statement::Switch{loc, expr, cases, default}  => {
                self.emit_loc(&loc);
                write!(self.f, "switch (\n").unwrap();
                self.emit_expr(expr);
                write!(self.f, ") {{\n").unwrap();
                for (conds, block) in cases {
                    for expr in conds {
                        write!(self.f, "case ").unwrap();
                        self.emit_expr(expr);
                        write!(self.f, ":\n").unwrap();
                    }
                    write!(self.f, "{{\n").unwrap();
                    self.emit_zblock(block, true);
                    write!(self.f, "break;}}\n").unwrap();
                }
                if let Some(default) = default {
                    write!(self.f, "default: {{\n").unwrap();
                    self.emit_zblock(default, true);
                    write!(self.f, "break;}}\n").unwrap();
                }
                write!(self.f, "}}\n").unwrap();
                false
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
            if self.emit_statement(stm) {
                write!(self.f, ";").unwrap();
            }
            if self.inside_macro {
                write!(self.f, "\\\n").unwrap();
            } else {
                write!(self.f, "\n").unwrap();
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
            if !ptr.tags.contains_key("mut") && !ptr.tags.contains_key("mut") {
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
                write!(self.f, "    ({}", self.to_local_typed_name(&typed)).unwrap();
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
                write!(self.f, " {}", match op {
                    ast::PostfixOperator::Increment    =>  "++",
                    ast::PostfixOperator::Decrement    =>  "--",
                }).unwrap();
                write!(self.f, ")").unwrap();
            },
            ast::Expression::UnaryPre{expr, loc, op} => {
                write!(self.f, "(").unwrap();
                self.emit_loc(&loc);
                write!(self.f, " {}", match op {
                    ast::PrefixOperator::Boolnot   =>  "!",
                    ast::PrefixOperator::Bitnot    =>  "~",
                    ast::PrefixOperator::Increment =>  "++",
                    ast::PrefixOperator::Decrement =>  "--",
                    ast::PrefixOperator::AddressOf =>  "&",
                    ast::PrefixOperator::Deref     =>  "*",
                }).unwrap();
                self.emit_expr(expr);
                write!(self.f, ")").unwrap();
            },
            ast::Expression::Cast{into,expr,..} => {
                write!(self.f, "    ({}", self.to_local_typed_name(&into)).unwrap();
                self.emit_pointer(&into.ptr);
                write!(self.f, ")").unwrap();
                write!(self.f, "(").unwrap();
                self.emit_expr(expr);
                write!(self.f, ")").unwrap();
            },
            ast::Expression::Name(name) => {
                self.emit_loc(&name.loc);
                write!(self.f, "    {}", self.to_local_typed_name(&name)).unwrap();
            },
            ast::Expression::LiteralString {loc, v} => {
                self.emit_loc(&loc);
                write!(self.f, "    \"").unwrap();
                for c in v {
                    self.write_escaped_literal(*c, true);
                }
                write!(self.f, "\"").unwrap();
            }
            ast::Expression::LiteralChar {loc, v} => {
                self.emit_loc(&loc);
                write!(self.f, "    '").unwrap();
                self.write_escaped_literal(*v, false);
                write!(self.f, "'").unwrap();
            }
            ast::Expression::Literal {loc, v} => {
                self.emit_loc(&loc);
                write!(self.f, "    {}", v).unwrap();
            }
            ast::Expression::Call { loc, name, args, emit , ..} => {
                match emit {
                    ast::EmitBehaviour::Default     => {},
                    ast::EmitBehaviour::Skip        => {return;},
                    ast::EmitBehaviour::Error{loc, message}   => {
                        emit_error(format!("{}", message), &[
                            (loc.clone(), "here")
                        ]);
                        std::process::exit(9);
                    }
                };

                self.emit_loc(&loc);
                self.emit_expr(&name);
                write!(self.f, "(").unwrap();

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
            ast::Expression::Infix {lhs, rhs, op, loc, ..} => {
                write!(self.f, "(").unwrap();
                self.emit_expr(lhs);
                self.emit_loc(&loc);
                write!(self.f, " {}", match op {
                    ast::InfixOperator::Equals      =>  "==",
                    ast::InfixOperator::Nequals     =>  "!=",
                    ast::InfixOperator::Add         =>  "+" ,
                    ast::InfixOperator::Subtract    =>  "-" ,
                    ast::InfixOperator::Multiply    =>  "*" ,
                    ast::InfixOperator::Divide      =>  "/" ,
                    ast::InfixOperator::Bitxor      =>  "^" ,
                    ast::InfixOperator::Booland     =>  "&&",
                    ast::InfixOperator::Boolor      =>  "||",
                    ast::InfixOperator::Moreeq      =>  ">=",
                    ast::InfixOperator::Lesseq      =>  "<=",
                    ast::InfixOperator::Lessthan    =>  "<" ,
                    ast::InfixOperator::Morethan    =>  ">" ,
                    ast::InfixOperator::Shiftleft   =>  "<<",
                    ast::InfixOperator::Shiftright  =>  ">>",
                    ast::InfixOperator::Modulo      =>  "%" ,
                    ast::InfixOperator::Bitand      =>  "&" ,
                    ast::InfixOperator::Bitor       =>  "|" ,
                }).unwrap();
                self.emit_expr(rhs);
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


    fn write_escaped_literal(&mut self, c: u8, isstr: bool) {
        let c = c as char;
        match c {
            '"' if isstr => {
                write!(self.f, "\\\"").unwrap();
            }
            '\'' if !isstr => {
                write!(self.f, "\\'").unwrap();
            }
            '\\' => {
                write!(self.f, "\\\\").unwrap();
            }
            '\t' => {
                write!(self.f, "\\t").unwrap();
            }
            '\r' => {
                write!(self.f, "\\r").unwrap();
            }
            '\n' => {
                write!(self.f, "\\n").unwrap();
            }
            _ if c.is_ascii() && !c.is_ascii_control() => {
                write!(self.f, "{}", c).unwrap();
            }
            _ => {
                if isstr {
                    write!(self.f, "\"\"\\x{:x}\"\"", c as u8).unwrap();
                } else {
                    write!(self.f, "\\x{:x}", c as u8).unwrap();
                }
            }
        }
    }
}
