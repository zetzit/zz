use super::project::{Project};
use std::fs;
use super::flatten;
use super::ast;
use super::make;
use std::io::{Write};
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
    p:              String,
    f:              fs::File,
    module:         flatten::Module,
    inside_macro:   bool,
    cur_loc:        Option<ast::Location>,
}

pub fn outname(_project: &Project, stage: &make::Stage, module: &flatten::Module) -> String {
    format!("target/{}/rs/{}.rs", stage, module.name.0[1..].join("_"))
}

impl Emitter {
    pub fn new(project: &Project, stage: make::Stage , module: flatten::Module) -> Self {

        std::fs::create_dir_all(format!("target/{}/rs/", stage)).unwrap();
        let p = outname(project, &stage, &module);
        let f = fs::File::create(&p).expect(&format!("cannot create {}", p));

        Emitter{
            p,
            f,
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
        //write!(self.f, "// line {} \"{}\"\n", loc.line(), loc.file).unwrap();
    }

    fn to_local_typed_name(&self, name: &ast::Typed) -> Option<String> {
        Some(match name.t {
            ast::Type::U8   => "u8".to_string(),
            ast::Type::U16  => "u16".to_string(),
            ast::Type::U32  => "u32".to_string(),
            ast::Type::U64  => "u64".to_string(),
            ast::Type::U128 => "u128".to_string(),
            ast::Type::I8   => "i8".to_string(),
            ast::Type::I16  => "i16".to_string(),
            ast::Type::I32  => "i32".to_string(),
            ast::Type::I64  => "i64".to_string(),
            ast::Type::I128 => "i128".to_string(),
            ast::Type::Int  => "std::os::raw::c_int".to_string(),
            ast::Type::UInt => "std::os::raw::c_uint".to_string(),
            ast::Type::ISize=> "isize".to_string(),
            ast::Type::USize=> "usize".to_string(),
            ast::Type::Bool => "bool".to_string(),
            ast::Type::F32  => "f32".to_string(),
            ast::Type::F64  => "f64".to_string(),
            ast::Type::Other(ref _n)   => {
                if name.ptr.len() != 1 {
                    return None;
                }
                "u8".to_string()
            }
                /*
                let mut s = self.to_local_name(&n);
                match &name.tail {
                    ast::Tail::Dynamic | ast::Tail::None | ast::Tail::Bind(_,_)=> {},
                    ast::Tail::Static(v,_) => {
                        s = format!("{}_{}", s, v);
                    }
                }
                s
            }
                */
            ast::Type::ILiteral | ast::Type::ULiteral | ast::Type::Elided | ast::Type::New => {
                parser::emit_error(
                    "ICE: untyped ended up in emitter",
                    &[(name.loc.clone(), format!("this should have been resolved earlier"))]
                    );
                std::process::exit(9);
            }
        })
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


        let mut s = s.clone();
        s.0.remove(0);
        return s.0.join("_");
    }

    pub fn emit(mut self) {
        let module = self.module.clone();
        debug!("emitting rs {}", module.name);



        write!(self.f, "extern crate libc;\n").unwrap();


        for (d,complete) in &module.d {
            let mut dmodname = Name::from(&d.name);
            dmodname.pop();
            if dmodname != module.name {
                continue;
            }
            if complete != &flatten::TypeComplete::Complete {
                continue
            }
            self.emit_loc(&d.loc);
            match d.def {
                ast::Def::Struct{..} => {
                    self.emit_struct(&d, None);
                    if let Some(vs) = module.typevariants.get(&Name::from(&d.name)) {
                        for v in vs {
                            let mut d = d.clone();
                            d.name = format!("{}_{}", d.name, v);
                            self.emit_struct(&d, Some(*v));
                        }
                    }
                },
                _ => (),
            }
        }

        write!(self.f, "extern {{\n").unwrap();

        for (d,complete) in &module.d {
            let mut dmodname = Name::from(&d.name);
            dmodname.pop();
            if dmodname != module.name {
                continue;
            }
            if complete != &flatten::TypeComplete::Complete {
                continue
            }

            let mut dmodname = Name::from(&d.name);
            dmodname.pop();
            if dmodname != module.name {
                continue;
            }


            self.emit_loc(&d.loc);
            match d.def {
                ast::Def::Macro{..} => {}
                ast::Def::Const{..} => {
                    self.emit_const(&d)
                }
                ast::Def::Static{..} => {
                    self.emit_static(&d)
                }
                ast::Def::Struct{..} => {
                    self.emit_struct_len(&d, None);
                    if let Some(vs) = module.typevariants.get(&Name::from(&d.name)) {
                        for v in vs {
                            let mut d = d.clone();
                            d.name = format!("{}_{}", d.name, v);
                            self.emit_struct_len(&d, Some(*v));
                        }
                    }
                }
                ast::Def::Enum{..} => {
                    self.emit_enum(&d)
                }
                ast::Def::Function{..} => {
                    if !d.name.ends_with("::main") {
                        self.emit_decl(&d);
                    }
                }
                ast::Def::Fntype{..} => {
                    self.emit_fntype(&d);
                }
                ast::Def::Theory{..} => {}
                ast::Def::Testcase {..} => {}
                ast::Def::Include{..} => {}
            }
            write!(self.f, "\n").unwrap();
        }

        write!(self.f, "}}\n").unwrap();
    }

    pub fn emit_static(&mut self, ast: &ast::Local) {
        self.emit_loc(&ast.loc);
        let (_typed, _expr, _tags, storage, _array) = match &ast.def {
            ast::Def::Static{typed, expr, tags, storage, array} => (typed, expr, tags, storage, array),
            _ => unreachable!(),
        };

        match storage {
            ast::Storage::Atomic => {
                return;
            },
            ast::Storage::ThreadLocal => {
                return;
            },
            ast::Storage::Static  => (),
        }

    }

    pub fn emit_const(&mut self, ast: &ast::Local) {
        self.emit_loc(&ast.loc);
        let (_typed, _expr) = match &ast.def {
            ast::Def::Const{typed, expr} => (typed, expr),
            _ => unreachable!(),
        };

    }

    pub fn emit_enum(&mut self, ast: &ast::Local) {
        self.emit_loc(&ast.loc);
        let names = match &ast.def {
            ast::Def::Enum{names} => (names),
            _ => unreachable!(),
        };
        write!(self.f, "enum {} {{\n", self.to_local_name(&Name::from(&ast.name))).unwrap();
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
        write!(self.f, "\n}};\n").unwrap();
    }


    pub fn emit_struct_len(&mut self, ast: &ast::Local, _tail_variant: Option<u64>) {
        let (_fields, _packed, _tail, _union) = match &ast.def {
            ast::Def::Struct{fields, packed, tail, union, ..} => (fields, packed, tail, union),
            _ => unreachable!(),
        };
        let shortname   = Name::from(&ast.name).0.last().unwrap().clone();
        write!(self.f, "    #[link_name = \"sizeof_{}\"]\n", self.to_local_name(&Name::from(&ast.name))).unwrap();
        write!(self.f, "    pub static sizeof_{}: libc::size_t;\n", shortname).unwrap();
    }

    pub fn emit_struct(&mut self, ast: &ast::Local, tail_variant: Option<u64>) {
        let (fields, _packed, tail, _union) = match &ast.def {
            ast::Def::Struct{fields, packed, tail, union, ..} => (fields, packed, tail, union),
            _ => unreachable!(),
        };
        let shortname   = Name::from(&ast.name).0.last().unwrap().clone();

         write!(self.f, r#"
pub struct {name} {{
    inner:  Box<__Inner{name}>,
    tail:   usize,
}}

impl std::ops::Deref for {name} {{
    type Target = __Inner{name};

    fn deref(&self) -> &__Inner{name} {{
        self.inner.deref()
    }}
}}

impl std::clone::Clone for {name} {{
    fn clone(&self) -> Self {{
        unsafe {{
            let size = sizeof_{name} + self.tail;


            let mut s = Box::new(vec![0u8; size]);
            std::ptr::copy_nonoverlapping(self._self(), s.as_mut_ptr(), size);

            let ss : *mut __Inner{name}= std::mem::transmute(Box::leak(s).as_mut_ptr());

            Self {{ inner: Box::from_raw(ss), tail: self.tail }}
        }}
    }}
}}

impl {name} {{
    pub fn _tail(&mut self) -> usize {{
        self.tail
    }}
    pub fn _self_mut(&mut self) -> *mut u8 {{
        unsafe {{ std::mem::transmute(self.inner.as_mut() as *mut __Inner{name}) }}
    }}
    pub fn _self(&self) -> *const u8 {{
        unsafe {{ std::mem::transmute(self.inner.as_ref() as *const __Inner{name}) }}
    }}
}}

"#, name = shortname).unwrap();
        write!(self.f, "\n\n#[repr(C)]\npub struct __Inner{} {{\n", shortname).unwrap();

        for i in 0..fields.len() {
            let field = &fields[i];

            let fieldtype = match self.to_local_typed_name(&field.typed) {
                Some(v) => v,
                None => {
                    continue;
                }
            };

            if let Some(array) = &field.array {
                if let Some(expr) = array {
                    write!(self.f, "    pub {} : [", field.name).unwrap();
                    self.emit_pointer(&field.typed.ptr);
                    write!(self.f, "{};", fieldtype).unwrap();
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
                        write!(self.f, "    pub {} : [", field.name).unwrap();
                        self.emit_pointer(&field.typed.ptr);
                        write!(self.f, ";{}]", tt).unwrap();
                    } else {
                        // rust makes unsized types 128bit pointers.
                        // maybe it also stores the size somewhere, but we can't use that to be
                        // compatible because obviously the ABI isnt guaranteed
                        write!(self.f, "    // {}", field.name).unwrap();
                    }
                }
            } else {
                write!(self.f, "    pub {} :", field.name).unwrap();
                write!(self.f, "{}",fieldtype).unwrap();
            }
            write!(self.f, " ,\n").unwrap();
        }
        write!(self.f, "}}\n").unwrap();
        write!(self.f, "impl {} {{\n", shortname).unwrap();

        if tail == &ast::Tail::None || tail_variant.is_some() {
            write!(self.f, "    pub fn new() -> Self {{\n").unwrap();
            write!(self.f, "        let tail = 0;\n").unwrap();
            write!(self.f, "        let size = unsafe{{sizeof_{}}};\n", shortname).unwrap();
        } else {
            write!(self.f, "    pub fn new(tail:  usize) -> Self {{\n").unwrap();
            write!(self.f, "        let size = unsafe{{sizeof_{}}} + tail;\n", shortname).unwrap();
        }

        write!(self.f, "        unsafe {{\n").unwrap();
        write!(self.f, "            let s = Box::new(vec![0u8; size]);\n").unwrap();
        write!(self.f, "            let ss : *mut __Inner{}= std::mem::transmute(Box::leak(s).as_mut_ptr());\n", shortname).unwrap();
        write!(self.f, "            Self {{ inner: Box::from_raw(ss), tail }} \n").unwrap();
        write!(self.f, "        }}\n").unwrap();
        write!(self.f, "    }}\n").unwrap();
        write!(self.f, "}}\n").unwrap();
   }




    fn function_args(&mut self, args: &Vec<ast::NamedArg>) {
        let mut first = true ;
        for arg in args {
            let argtype = match self.to_local_typed_name(&arg.typed) {
                Some(v) => v,
                None => {
                    continue
                }
            };
            if first {
                first = false;
            } else {
                write!(self.f, ", ").unwrap();
            }

            write!(self.f, " Z{}: ", arg.name).unwrap();



            self.emit_pointer(&arg.typed.ptr);
            write!(self.f, "{}", argtype).unwrap();

        }
    }

    pub fn emit_fntype(&mut self, ast: &ast::Local) {
        let (_ret, _args, _vararg, _attr) = match &ast.def {
            ast::Def::Fntype{ret, args, vararg, attr, ..} => (ret, args, *vararg, attr),
            _ => unreachable!(),
        };
        self.emit_loc(&ast.loc);
    }

    pub fn emit_decl(&mut self, ast: &ast::Local) {
        let (ret, args, _body, _vararg, _attr) = match &ast.def {
            ast::Def::Function{ret, args, body, vararg, attr, ..} => (ret, args, body, *vararg, attr),
            _ => unreachable!(),
        };

        let shortname   = Name::from(&ast.name).0.last().unwrap().clone();
        let rettype = match &ret {
            None => None,
            Some(a) => match self.to_local_typed_name(&a.typed) {
                None => return,
                Some(v) => Some(v),
            },
        };

        write!(self.f, "    #[link_name = \"{}\"]\n", self.to_local_name(&Name::from(&ast.name))).unwrap();
        write!(self.f, "    pub fn {}(", shortname).unwrap();

        self.function_args(args);
        write!(self.f, ")").unwrap();

        if let (Some(a),Some(rettype)) = (&ret, &rettype) {
            write!(self.f, "  -> ").unwrap();
            self.emit_pointer(&a.typed.ptr);
            write!(self.f, "{}", rettype).unwrap();
        };

        write!(self.f, ";\n").unwrap();
    }


    fn emit_pointer(&mut self, v: &Vec<ast::Pointer>) {
        for ptr in v {
            write!(self.f, "*").unwrap();
            if ptr.tags.contains_key("mut") || ptr.tags.contains_key("mut") {
                 write!(self.f, "mut ").unwrap();
            } else {
                write!(self.f, "const ").unwrap();
            }
        }
    }

    fn emit_expr(&mut self, v: &ast::Expression) {
        match v {
            ast::Expression::ArrayInit{..} => {
            },
            ast::Expression::StructInit{..} => {
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
            ast::Expression::Cast{..} => {
            },
            ast::Expression::Name(name) => {
                self.emit_loc(&name.loc);
                write!(self.f, "    {}", self.to_local_typed_name(&name).unwrap_or("()".to_string())).unwrap();
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
