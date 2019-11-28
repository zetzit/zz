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
    p:              String,
    f:              fs::File,
    module:         flatten::Module,
    inside_macro:   bool,
    cur_loc:        Option<ast::Location>,
}

pub fn outname(project: &Project, stage: &make::Stage, module: &flatten::Module) -> String {
    format!("target/{}/rs/{}.rs", stage, module.name)
}

impl Emitter {
    pub fn new(project: &Project, stage: make::Stage , module: flatten::Module) -> Self {

        std::fs::create_dir_all(format!("target/{}/rs/", stage)).unwrap();
        let p = outname(project, &stage, &module);
        let mut f = fs::File::create(&p).expect(&format!("cannot create {}", p));

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

    fn to_local_typed_name(&self, name: &ast::Typed) -> String {
        match name.t {
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
            ast::Type::Other(ref n)   => "u8".to_string(),
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
            ast::Type::ILiteral | ast::Type::ULiteral => {
                parser::emit_error(
                    "ICE: untyped ended up in emitter",
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

    pub fn emit(mut self) -> CFile {
        let module = self.module.clone();
        debug!("emitting {}", module.name);

        write!(self.f, "extern {{\n").unwrap();

        for (d,decl_here,def_here) in &module.d {
            if !decl_here{
                continue
            }
            if d.vis != ast::Visibility::Export {
                continue;
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
                    self.emit_struct(&d, *def_here, None);
                    if let Some(vs) = module.typevariants.get(&Name::from(&d.name)) {
                        for v in vs {
                            let mut d = d.clone();
                            d.name = format!("{}_{}", d.name, v);
                            self.emit_struct(&d, *def_here, Some(*v));
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
            }
            write!(self.f, "\n").unwrap();
        }

        write!(self.f, "}}\n").unwrap();

        CFile {
            name:       module.name,
            filepath:   self.p,
            sources:    module.sources,
            deps:       module.deps,
        }
    }

    pub fn emit_static(&mut self, ast: &ast::Local) {
        self.emit_loc(&ast.loc);
        let (typed, expr, tags, storage, array) = match &ast.def {
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
        let (typed, expr) = match &ast.def {
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


    pub fn emit_struct(&mut self, ast: &ast::Local, def_here: bool, tail_variant: Option<u64>) {
        let (fields, packed, tail, union) = match &ast.def {
            ast::Def::Struct{fields, packed, tail, union} => (fields, packed, tail, union),
            _ => unreachable!(),
        };
        write!(self.f, "    pub static sizeof_{}: libc::size_t;\n", self.to_local_name(&Name::from(&ast.name))).unwrap();
    }




    fn function_args(&mut self, args: &Vec<ast::NamedArg>) {
        let mut first = true ;
        for arg in args {
            if first {
                first = false;
            } else {
                write!(self.f, ", ").unwrap();
            }

            write!(self.f, " Z{}: ", arg.name).unwrap();



            self.emit_pointer(&arg.typed.ptr);
            write!(self.f, "{}", self.to_local_typed_name(&arg.typed)).unwrap();

        }
    }

    pub fn emit_fntype(&mut self, ast: &ast::Local) {
        let (ret, args, vararg, attr) = match &ast.def {
            ast::Def::Fntype{ret, args, vararg, attr} => (ret, args, *vararg, attr),
            _ => unreachable!(),
        };
        self.emit_loc(&ast.loc);
    }

    pub fn emit_decl(&mut self, ast: &ast::Local) {
        let (ret, args, _body, vararg, attr) = match &ast.def {
            ast::Def::Function{ret, args, body, vararg, attr, ..} => (ret, args, body, *vararg, attr),
            _ => unreachable!(),
        };

        write!(self.f, "    pub fn {}(", Name::from(&ast.name).0[1..].join("_")).unwrap();

        self.function_args(args);
        write!(self.f, ")").unwrap();

        if let Some(a) = &ret {
            write!(self.f, "  -> ").unwrap();
            self.emit_pointer(&a.typed.ptr);
            write!(self.f, "{}", self.to_local_typed_name(&a.typed)).unwrap();
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
            ast::Expression::ArrayInit{fields,loc} => {
            },
            ast::Expression::StructInit{typed, fields,loc} => {
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
