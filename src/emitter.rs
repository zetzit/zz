#![allow(unused)]

use super::ast;
use super::flatten;
use super::make;
use super::name::Name;
use super::parser::{self, emit_error};
use super::project::Project;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::fs;
use std::io::{Read, Write};
use std::path::PathBuf;
use super::project;
use super::mergecc;

#[derive(Serialize, Deserialize)]
pub struct CFile {
    pub name:       Name,
    pub filepath:   String,
    pub sources:    HashSet<PathBuf>,
    pub deps:       HashSet<Name>,
    pub symbols:    HashSet<Name>,

    pub cflags:     Vec<String>,
    pub lflags:     Vec<String>,
}

pub struct Emitter {
    cxx: bool,
    p: String,
    f: fs::File,
    module: flatten::Module,
    cincludes: Vec<String>,
    header: bool,
    inside_macro: bool,
    cur_loc: Option<ast::Location>,
    casedir: PathBuf,
    emit_as_extern: HashSet<Name>,
    symbols: HashSet<Name>,
    inside_constant_expr: bool,
}

pub fn outname(
    project: &Project,
    stage: &make::Stage,
    module: &Name,
    header: bool,
) -> (bool, String) {

    let td = project::target_dir().join(stage.to_string());

    let mut cxx = false;
    if let Some(std) = &project.std {
        if std.contains("c++") {
            cxx = true;
        }
    }

    let mut ns = module.0.clone();
    ns.remove(0);
    if header {
        (
            (cxx, format!("{}/include/zz/{}.h",
                          project::target_dir().to_string_lossy(),
                          ns.join("_")))
        )
    } else if cxx {
        (cxx, format!("{}/zz/{}.cpp", td.to_string_lossy(), ns.join("_")))
    } else {
        (cxx, format!("{}/zz/{}.c", td.to_string_lossy(), ns.join("_")))
    }
}

impl Emitter {
    pub fn new(
        project: &Project,
        stage: make::Stage,
        module: flatten::Module,
        header: bool,
    ) -> Self {
        let (cxx, p) = outname(project, &stage, &module.name, header);
        let mut f = fs::File::create(&p).expect(&format!("cannot create {}", p));

        let casedir = project::target_dir()
            .join(stage.to_string())
            .join("testcases")
            .join(module.name.0[1..].join("_"));
        std::fs::remove_dir_all(&casedir).ok();
        std::fs::create_dir_all(&casedir).unwrap();

        write!(f, "#include <stdint.h>\n").unwrap();
        write!(f, "#include <stddef.h>\n").unwrap();
        write!(f, "#include <stdbool.h>\n").unwrap();

        Emitter {
            cincludes: project.cincludes.clone(),
            cxx,
            p,
            f,
            header,
            casedir,
            module,
            inside_macro: false,
            cur_loc: None,
            emit_as_extern: HashSet::new(),
            symbols: HashSet::new(),
            inside_constant_expr: false,
        }
    }

    fn emit_loc(&mut self, loc: &ast::Location) {
        if let Some(cur_loc) = &self.cur_loc {
            if cur_loc.file == loc.file && cur_loc.line == loc.line {
                return;
            }
        }
        self.cur_loc = Some(loc.clone());

        if self.header {
            return;
        }
        if self.inside_macro {
            return;
        }
        write!(
            self.f,
            "\n#line {} \"{}\"\n",
            loc.line,
            loc.file.replace("\\", "\\\\")
        )
        .unwrap();
    }

    pub fn to_local_typed_name(&self, name: &ast::Typed) -> String {
        match name.t {
            ast::Type::Void => "void".to_string(),
            ast::Type::Char => "char".to_string(),
            ast::Type::U8 => "uint8_t".to_string(),
            ast::Type::U16 => "uint16_t".to_string(),
            ast::Type::U32 => "uint32_t".to_string(),
            ast::Type::U64 => "uint64_t".to_string(),
            ast::Type::U128 => "uint128_t".to_string(),
            ast::Type::I8 => "int8_t".to_string(),
            ast::Type::I16 => "int16_t".to_string(),
            ast::Type::I32 => "int32_t".to_string(),
            ast::Type::I64 => "int64_t".to_string(),
            ast::Type::I128 => "int128_t".to_string(),
            ast::Type::Int => "int".to_string(),
            ast::Type::UInt => "unsigned int".to_string(),
            ast::Type::ISize => "intptr_t".to_string(),
            ast::Type::USize => "uintptr_t".to_string(),
            ast::Type::Bool => "bool".to_string(),
            ast::Type::F32 => "float".to_string(),
            ast::Type::F64 => "double".to_string(),
            ast::Type::Other(ref n) => {
                let mut s = self.to_local_name(&n);
                match &name.tail {
                    ast::Tail::Dynamic(_) | ast::Tail::None | ast::Tail::Bind(_, _) => {}
                    ast::Tail::Static(v, _) => {
                        s = format!("{}_{}", s, v);
                    }
                }
                s
            }
            ast::Type::ILiteral | ast::Type::ULiteral | ast::Type::Elided | ast::Type::New | ast::Type::Typeid => {
                parser::emit_error(
                    "ICE: untyped literal ended up in emitter",
                    &[(
                        name.loc.clone(),
                        format!("this should have been resolved earlier"),
                    )],
                );
                std::process::exit(9);
            }
        }
    }
    pub fn to_local_name(&self, s: &Name) -> String {
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

        let mut s = s.clone();
        s.0.remove(0);
        return s.0.join("_");
    }

    fn to_local_name_mangle(&self, s: &Name) -> String {
        self.to_local_name(s)
            .chars()
            .map(|x| match x {
                'A'..='Z' => x,
                'a'..='z' => x,
                '0'..='9' => x,
                '_' => x,
                _ => '_',
            })
            .collect()
    }

    pub fn emit(mut self) -> CFile {
        let module = self.module.clone();
        debug!("emitting {}", module.name.0.join("_"));

        let mut dup = HashSet::new();

        // forward declarations first
        // these have no dependencies, so just put them first.
        // this shouldnt be nessesary, but dependency ordering for struct A { fntype(A*) } is still broken
        for (d, _) in &module.d {
            debug!("    emitting0 {}", d.name);
            write!(self.f, "#ifndef ZZ_FORWARD_{tn}\n#define ZZ_FORWARD_{tn}\n",
                   tn = self.to_local_name_mangle(&Name::from(&d.name))
                  ).unwrap();
            match &d.def {
                ast::Def::Struct { .. } => {

                    self.emit_struct_def(&d, None);
                    if let Some(vs) = module.typevariants.get(&Name::from(&d.name)) {
                        for (v, loc) in vs {
                            write!(self.f, "#endif\n#ifndef ZZ_FORWARD_{tn}_{v}\n#define ZZ_FORWARD_{tn}_{v}\n",
                                   tn = self.to_local_name_mangle(&Name::from(&d.name)),
                                   v = v
                                  ).unwrap();
                            let mut d = d.clone();
                            d.name = format!("{}_{}", d.name, v);
                            self.emit_struct_def(&d, Some((v.clone(), loc.clone())));
                        }
                    }
                }
                ast::Def::Type { .. } => {
                    self.emit_typealias(&d);
                }
                _ => (),
            }
            write!(self.f, "#endif\n").unwrap();
        }

        for (d, complete) in &module.d {

            if complete != &flatten::TypeComplete::Complete {
                match d.def {
                    ast::Def::Struct { .. } |
                    ast::Def::Symbol { .. } |
                    ast::Def::Enum   { .. } |
                    ast::Def::Testcase {.. } => continue,
                    _ => (),
                }
            }
            if self.header {
                match d.def {
                    ast::Def::Static   { .. } |
                    ast::Def::Testcase { .. } => continue,
                    _ => (),
                }
            }


            debug!("    emitting {}", d.name);
            write!(self.f, "#ifndef ZZ_EXPORT_{tn}\n#define ZZ_EXPORT_{tn}\n",
                tn = self.to_local_name_mangle(&Name::from(&d.name))
            ).unwrap();

            match d.def {
                ast::Def::Macro { .. } => {}
                ast::Def::Const { .. } => {
                    self.emit_const(&d);
                }
                ast::Def::Static { .. } => {
                    self.emit_static(&d)
                }
                ast::Def::Symbol { .. } => {
                    self.emit_symbol(&d);
                    self.symbols.insert(Name::from(&d.name));
                }
                ast::Def::Enum { .. } => {
                    self.emit_enum(&d);
                }
                ast::Def::Closure { .. } => {
                    self.emit_closure(&d);
                }
                ast::Def::Theory { .. } => {}
                ast::Def::Testcase { .. } => {
                    self.emit_testcase(&d);
                }
                ast::Def::Function { .. } => {
                    self.emit_decl(&d);
                }
                ast::Def::Flags { .. } => {
                    self.emit_flags(&d);
                }
                ast::Def::Include { inline, .. } => {
                    if dup.insert(d.name.clone()) {
                        self.emit_include(&d);
                    }
                }
                ast::Def::Struct { .. } => {
                    let mut name = Name::from(&d.name);
                    name.pop();
                    let isimpl = name == module.name;

                    self.emit_struct(&d, isimpl, None);

                    if let Some(vs) = module.typevariants.get(&Name::from(&d.name)) {
                        for (v, tvloc) in vs {
                            let mut d = d.clone();
                            d.name = format!("{}_{}", d.name, v);
                            write!(self.f, "#endif\n#ifndef ZZ_EXPORT_{tn}_{v}\n#define ZZ_EXPORT_{tn}_{v}\n",
                                   tn = self.to_local_name_mangle(&Name::from(&d.name)),
                                   v = v
                                  ).unwrap();
                            self.emit_struct(&d, isimpl, Some((*v, tvloc.clone())));
                        }
                    }
                }
                _ => (),
            }
            write!(self.f, "\n#endif\n").unwrap();
        }

        if !self.header {
            // function impls are always last.
            // so we can be a bit more relaxed about emitting the correct decleration order
            for (d, complete) in &module.d {
                match &d.def {
                    ast::Def::Function { attr, .. } => {
                        debug!("    emitting2 {}", d.name);


                        let mut mname = Name::from(&d.name);
                        mname.pop();
                        if complete == &flatten::TypeComplete::Complete
                            && (mname == module.name || attr.contains_key("inline"))
                        {
                            write!(self.f, "#ifndef ZZ_IMPL_{tn}\n#define ZZ_IMPL_{tn}\n",
                                   tn = self.to_local_name_mangle(&Name::from(&d.name))
                            ).unwrap();

                            self.emit_def(&d);

                            write!(self.f, "\n#endif\n").unwrap();
                        }


                    }
                    _ => (),
                }
            }
        }

        CFile {
            name: module.name,
            filepath: self.p,
            sources: module.sources,
            deps: module.deps,
            symbols: self.symbols,
            cflags: Vec::new(),
            lflags: Vec::new(),
        }
    }

    pub fn emit_include(&mut self, ast: &ast::Local) {
        self.emit_loc(&ast.loc);
        let (expr, loc, fqn, inline, _needs) = match &ast.def {
            ast::Def::Include {
                expr,
                loc,
                fqn,
                inline,
                needs,
            } => (expr, loc, fqn, inline, needs),
            _ => unreachable!(),
        };

        //// TODO this is always going to be subtily broken
        //// flatten should run a separate dependency tree for the header
        //// some inlines are unessesary in headers, as no exported type needs them,
        //// but we have no idea which ones here.
        ////
        //if *inline && self.header {
        //    return;
        //}



        trace!("    emit include {} (inline? {})", fqn, inline);
        if *inline {

            let thisdir = std::env::current_dir().expect("PWD broke somehow");
            let path = pathdiff::diff_paths(
                std::path::Path::new(&expr),
                std::path::Path::new(&thisdir),
            ).expect(&format!("include path {} broke somehow", expr));

            let outbase = super::project::target_dir()
                .join("c");
            let fi = mergecc::mergecc(
                &self.cincludes,
                &outbase,
                &path,
                );

            let mut f = match fs::File::open(&fi) {
                Err(e) => {
                    parser::emit_error(
                        format!("cannot inline {:?}", expr),
                        &[(loc.clone(), format!("{}", e))],
                    );
                    std::process::exit(9);
                }
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

        let mut expr = expr.clone();
        if expr.starts_with("\"") {
            expr.remove(0);
            expr.remove(expr.len() -1);

            let thisdir = std::env::current_dir().expect("PWD broke somehow");
            let path = pathdiff::diff_paths(
                std::path::Path::new(&expr),
                std::path::Path::new(&thisdir),
            ).expect(&format!("include path {} broke somehow", expr));

            let outbase = super::project::target_dir()
                .join("c");
            let fi = mergecc::mergecc(
                &self.cincludes,
                &outbase,
                &path,
                );

            let thispath = std::fs::canonicalize(&self.p).expect("PWD broke somehow");
            let thisdir  = thispath.parent().expect("PWD broke somehow");
            let path = pathdiff::diff_paths(
                std::path::Path::new(&fi),
                std::path::Path::new(&thisdir),
            ).expect(&format!("include path {} broke somehow", expr));
            write!(self.f, "#include \"{}\"\n", path.to_string_lossy()).unwrap();
        } else {
            write!(self.f, "#include {}\n", expr).unwrap();
        }

        if self.cxx && expr.contains(".h>") {
            write!(self.f, "}}\n").unwrap();
        }

        if fqn.len() > 3 {
            write!(self.f, "using namespace {} ;\n", fqn.0[3..].join("::")).unwrap();
        }

    }

    pub fn emit_static(&mut self, ast: &ast::Local) {
        self.emit_loc(&ast.loc);

        let (typed, expr, tags, storage, array) = match &ast.def {
            ast::Def::Static {
                typed,
                expr,
                tags,
                storage,
                array,
            } => (typed, expr, tags, storage, array),
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
            }
            ast::Storage::ThreadLocal => {
                write!(self.f, "_Thread_local ").unwrap();
            }
            ast::Storage::Static => (),
        }

        write!(self.f, "{} ", self.to_local_typed_name(&typed)).unwrap();
        self.emit_pointer(&typed.ptr);

        write!(self.f, "{} ", self.to_local_name(&Name::from(&ast.name))).unwrap();


        match &array {
            ast::Array::Sized(expr) => {
                write!(self.f, " [ ").unwrap();
                self.emit_expr(expr);
                write!(self.f, " ] ").unwrap();
            }
            ast::Array::Unsized => {
                write!(self.f, " [ ").unwrap();
                write!(self.f, " ] ").unwrap();
            }
            ast::Array::None => {}
        }

        write!(self.f, "=").unwrap();
        self.inside_constant_expr = true;
        self.emit_expr(&expr);
        self.inside_constant_expr = false;
        write!(self.f, ";\n").unwrap();
    }

    pub fn emit_const(&mut self, ast: &ast::Local) {
        let (typed, expr) = match &ast.def {
            ast::Def::Const { typed, expr } => (typed, expr),
            _ => unreachable!(),
        };

        self.emit_loc(&ast.loc);

        self.inside_macro = true;
        write!(
            self.f,
            "#define {} (",
            self.to_local_name(&Name::from(&ast.name))
        )
        .unwrap();

        // gcc doesnt like this. if we want it back,
        // it needs to be conditionally disabled if it would cast to the same type
        /*
        write!(self.f, "({} ", self.to_local_typed_name(&typed)).unwrap();
        self.emit_pointer(&typed.ptr);
        write!(self.f, ")").unwrap();
        */


        self.emit_expr(&expr);
        write!(self.f, ")\n").unwrap();

        self.inside_macro = false;
    }

    pub fn emit_symbol(&mut self, ast: &ast::Local) {
        self.emit_loc(&ast.loc);

        write!(
            self.f,
            "extern const __attribute__ ((unused)) size_t {};\n",
            self.to_local_name(&Name::from(&ast.name))
        )
        .unwrap();
    }

    pub fn emit_typealias(&mut self, ast: &ast::Local) {
        let alias = match &ast.def {
            ast::Def::Type { alias, ..} => (alias),
            _ => unreachable!(),
        };

        self.emit_loc(&ast.loc);
        write!(self.f, "typedef {} {};\n",
            self.to_local_typed_name(alias),
            self.to_local_name(&Name::from(&ast.name)),
        ).unwrap();
    }

    pub fn emit_enum(&mut self, ast: &ast::Local) {
        let names = match &ast.def {
            ast::Def::Enum { names, ..} => (names),
            _ => unreachable!(),
        };
        self.emit_loc(&ast.loc);
        write!(self.f, "typedef enum {{\n").unwrap();
        for (name, literal) in names {
            write!(
                self.f,
                "    {}_{}",
                self.to_local_name(&Name::from(&ast.name)),
                name
            )
            .unwrap();
            if let Some(literal) = literal {
                write!(self.f, " = {}", literal).unwrap();
            }
            write!(self.f, ",\n").unwrap();
        }
        write!(
            self.f,
            "\n}} {};\n",
            self.to_local_name(&Name::from(&ast.name))
        )
        .unwrap();
    }

    fn emit_testcase(&mut self, ast: &ast::Local) {
        let fields = match &ast.def {
            ast::Def::Testcase { fields } => fields,
            _ => unreachable!(),
        };

        let testname = Name::from(&ast.name).0.last().cloned().unwrap();
        let dir = self.casedir.join(testname);
        std::fs::remove_dir_all(&dir).ok();
        std::fs::create_dir_all(&dir).unwrap();
        for (fname, expr) in fields {
            let p = dir.join(fname);
            let mut f = fs::File::create(&p).expect(&format!("cannot create {}", p.to_string_lossy()));
            match expr {
                ast::Expression::LiteralString { v, .. } => {
                    let mut v = v.clone();

                    // TODO this is a hack to work around string literals containing
                    // native file endings
                    v.retain(|i| *i != b'\r');
                    f.write_all(&v).unwrap();
                }
                ast::Expression::ArrayInit { fields, .. } => {
                    for field in fields {
                        match field.as_ref() {
                            ast::Expression::LiteralChar { v, .. } => {
                                f.write(&[*v as u8]).unwrap();
                            }
                            ast::Expression::Literal { v, loc } => match parser::parse_int(v) {
                                Some(parser::Integer::Unsigned(v)) if v <= 255 => {
                                    f.write(&[v as u8]).unwrap();
                                }
                                _ => {
                                    parser::emit_error(
                                            "testcase field must be literal string or byte array",
                                            &[(loc.clone(), format!("this expression cannot be emitted as testcase file"))]
                                            );
                                    std::process::exit(9);
                                }
                            },
                            _ => {
                                parser::emit_error(
                                    "testcase field must be literal string or byte array, not",
                                    &[(
                                        field.loc().clone(),
                                        format!(
                                            "this expression cannot be emitted as testcase file"
                                        ),
                                    )],
                                );
                                std::process::exit(9);
                            }
                        }
                    }
                }
                ast::Expression::Literal { v, .. } => {
                    f.write_all(v.as_bytes()).unwrap();
                }
                _ => {
                    parser::emit_error(
                        "testcase field must be literal string or byte array",
                        &[(
                            expr.loc().clone(),
                            format!("this expression cannot be emitted as testcase file"),
                        )],
                    );
                    std::process::exit(9);
                }
            }
        }
    }

    pub fn emit_struct_def(
        &mut self,
        ast: &ast::Local,
        tail_variant: Option<(u64, ast::Location)>,
    ) {
        let (fields, packed, _tail, union) = match &ast.def {
            ast::Def::Struct {
                fields,
                packed,
                tail,
                union,
                ..
            } => (fields, packed, tail, union),
            _ => unreachable!(),
        };

        self.emit_loc(&ast.loc);
        if *union {
            write!(self.f, "union ").unwrap();
        } else {
            write!(self.f, "struct ").unwrap();
        }
        write!(
            self.f,
            "{}_t;\n",
            self.to_local_name(&Name::from(&ast.name))
        )
        .unwrap();

        write!(self.f, "typedef ").unwrap();
        if *union {
            write!(self.f, "union ").unwrap();
        } else {
            write!(self.f, "struct ").unwrap();
        }
        write!(
            self.f,
            "{}_t {};\n",
            self.to_local_name(&Name::from(&ast.name)),
            self.to_local_name(&Name::from(&ast.name))
        )
        .unwrap();
    }

    pub fn emit_struct(
        &mut self,
        ast: &ast::Local,
        isimpl: bool,
        tail_variant: Option<(u64, ast::Location)>,
    ) {
        let (fields, packed, structtail, union) = match &ast.def {
            ast::Def::Struct {
                fields,
                packed,
                tail,
                union,
                ..
            } => (fields, packed, tail, union),
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
        let mut emitted_exact_tail = false;
        for i in 0..fields.len() {
            let field = &fields[i];
            self.emit_loc(&field.loc);
            write!(self.f, "   {}", self.to_local_typed_name(&field.typed)).unwrap();
            self.emit_pointer(&field.typed.ptr);

            match &field.array {
                ast::Array::Sized(expr) => {
                    write!(self.f, " {}", field.name).unwrap();
                    write!(self.f, "[").unwrap();
                    self.emit_expr(expr);
                    write!(self.f, "]").unwrap();
                }
                ast::Array::Unsized => {
                    if i != (fields.len() - 1) {
                        parser::emit_error(
                            "tail field has no be the last field in a struct",
                            &[(
                                field.loc.clone(),
                                format!("tail field would displace next field"),
                            )],
                        );
                        std::process::exit(9);
                    }
                    if let Some((tt, _)) = &tail_variant {
                        emitted_exact_tail = true;
                        write!(self.f, " {}[{}]", field.name, tt).unwrap();
                    } else {
                        //TODO emit as something else (not as pointer!)
                        // nested flexible arrays are non standard
                        write!(self.f, " {}[]", field.name).unwrap();
                    }
                }
                ast::Array::None => {
                    write!(self.f, " {}", field.name).unwrap();
                }
            }

            write!(self.f, " ;\n").unwrap();
        }

        if let Some((tt, loc)) = &tail_variant {
            if !emitted_exact_tail {
                match &structtail {
                    ast::Tail::Dynamic(Some(t)) => {
                        write!(
                            self.f,
                            "   {} _____tail [{}];\n",
                            self.to_local_typed_name(&t),
                            tt
                        )
                        .unwrap();
                    }
                    o => {
                        parser::emit_error(
                            "no field available",
                            &[
                                (ast.loc.clone(), format!("tail field is {:?}", o)),
                                (loc.clone(), format!("when expanding type here")),
                            ],
                        );
                        std::process::exit(9);
                    }
                }
            }
        }
        write!(self.f, "}}\n").unwrap();

        if *packed {
            write!(self.f, " __attribute__((__packed__)) ").unwrap();
        }

        write!(self.f, ";\n").unwrap();

        if (ast.vis == ast::Visibility::Export && isimpl) || self.header {
            if self.header {
                if structtail == &ast::Tail::None || tail_variant.is_some() {
                    write!(
                        self.f,
                        "size_t sizeof_{name}();\n",
                        name = self.to_local_name(&Name::from(&ast.name))
                    )
                    .unwrap();
                } else if let ast::Tail::Dynamic(_) = structtail {
                    write!(
                        self.f,
                        "size_t sizeof_{name}(size_t tail);\n",
                        name = self.to_local_name(&Name::from(&ast.name))
                    )
                    .unwrap();
                }
            } else {
                write!(self.f, "#endif\n#ifndef ZZ_EXPORT_SIZEOF_{tn}\n#define ZZ_EXPORT_SIZEOF_{tn}\n",
                       tn = self.to_local_name_mangle(&Name::from(&ast.name)),
                      ).unwrap();
                if structtail == &ast::Tail::None || tail_variant.is_some() {
                    write!(
                        self.f,
                        "size_t sizeof_{name}() {{ return sizeof({name}); }}\n",
                        name = self.to_local_name(&Name::from(&ast.name)),
                    )
                    .unwrap();
                } else if let ast::Tail::Dynamic(Some(t)) = structtail {
                    write!(self.f, "size_t sizeof_{name}(size_t tail) {{ return sizeof({name}) + (tail * sizeof({tailt})); }}\n",
                    name  = self.to_local_name(&Name::from(&ast.name)),
                    tailt = self.to_local_typed_name(&t),
                    ).unwrap();
                }
            }
        }
    }

    pub fn function_args(&mut self, args: &Vec<ast::NamedArg>) {
        let mut first = true;
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

    pub fn emit_closure(&mut self, ast: &ast::Local) {
        let (ret, args, attr) = match &ast.def {
            ast::Def::Closure {
                ret,
                args,
                attr,
                ..
            } => (ret, args, attr),
            _ => unreachable!(),
        };
        self.emit_loc(&ast.loc);

        write!(self.f, "#if !defined(ZZ_HAS_DEFINED_CLOSURE_{ln})\n#define ZZ_HAS_DEFINED_CLOSURE_{ln} 1\n",
        ln = self.to_local_name(&Name::from(&ast.name))).unwrap();

        write!(self.f, "typedef struct {{\n    void *ctx;\n").unwrap();
        match &ret {
            None => write!(self.f, "    void ").unwrap(),
            Some(a) => {
                write!(self.f, "    {} ", self.to_local_typed_name(&a.typed)).unwrap();
                self.emit_pointer(&a.typed.ptr);
            }
        };

        for (attr, loc) in attr {
            match attr.as_str() {
                o => {
                    parser::emit_error(
                        "ICE: unsupported attr",
                        &[(loc.clone(), format!("'{}' is not a valid c attribute", o))],
                    );
                    std::process::exit(9);
                }
            }
        }

        write!(self.f, "(*fn) (").unwrap();
        self.function_args(args);

        if args.len() > 0 {
            write!(self.f, ", ").unwrap();
        }
        write!(self.f, "void * _ctx").unwrap();
        write!(self.f, ");\n").unwrap();

        write!(
            self.f,
            "}} {};\n",
            self.to_local_name(&Name::from(&ast.name))
        )
        .unwrap();

        write!(self.f, "#endif // !defined(ZZ_HAS_DEFINED_CLOSURE_{ln})\n",
        ln = self.to_local_name(&Name::from(&ast.name))).unwrap();
    }

    pub fn emit_decl(&mut self, ast: &ast::Local) {
        let (ret, args, _body, vararg, attr) = match &ast.def {
            ast::Def::Function {
                ret,
                args,
                body,
                vararg,
                attr,
                ..
            } => (ret, args, body, *vararg, attr),
            _ => unreachable!(),
        };

        // declare the fqn

        let mut inline = false;
        for (attr, _) in attr {
            match attr.as_str() {
                "inline" => {
                    inline = true;
                }
                _ => (),
            }
        }

        self.emit_loc(&ast.loc);

        match &ast.vis {
            ast::Visibility::Object => {
                write!(self.f, "static ").unwrap();
            }
            ast::Visibility::Shared if !inline => {
                write!(self.f, "extern ").unwrap();
            }
            _ => (),
        };

        match &ret {
            None => write!(self.f, "void ").unwrap(),
            Some(a) => {
                write!(self.f, "{} ", self.to_local_typed_name(&a.typed)).unwrap();
                self.emit_pointer(&a.typed.ptr);
            }
        };

        //match &ast.vis {
        //    ast::Visibility::Object => (),
        //    ast::Visibility::Shared => write!(self.f, "__attribute__ ((visibility (\"hidden\"))) ").unwrap(),
        //    ast::Visibility::Export => write!(self.f, "__attribute__ ((visibility (\"default\"))) ").unwrap(),
        //}

        let mut name = Name::from(&ast.name);
        for (attr, loc) in attr {
            match attr.as_str() {
                "extern" => {
                    self.emit_as_extern.insert(name.clone());
                    name = Name::from(name.0.last().unwrap());
                    name.0.insert(0, String::new());
                }
                "inline" => {
                    write!(self.f, " static inline ").unwrap();
                }
                o => {
                    parser::emit_error(
                        "ICE: unsupported attr",
                        &[(loc.clone(), format!("'{}' is not a valid c attribute", o))],
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
            None => write!(self.f, "void ").unwrap(),
            Some(a) => {
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
            ast::Def::Function {
                ret,
                args,
                body,
                vararg,
                attr,
                ..
            } => (ret, args, body, *vararg, attr),
            _ => unreachable!(),
        };

        self.emit_loc(&ast.loc);

        if !ast.name.ends_with("::main") {
            match &ast.vis {
                ast::Visibility::Object => {
                    write!(self.f, "static ").unwrap();
                }
                _ => (),
            };
        }

        let mut vis = ast.vis.clone();
        let mut name = Name::from(&ast.name);
        for (attr, loc) in attr {
            match attr.as_str() {
                "extern" => {
                    self.emit_as_extern.insert(name.clone());
                    name = Name::from(name.0.last().unwrap());
                    name.0.insert(0, String::new());
                }
                "inline" => {
                    write!(self.f, " static inline ").unwrap();
                    vis = ast::Visibility::Object;
                }
                o => {
                    parser::emit_error(
                        "ICE: unsupported attr",
                        &[(loc.clone(), format!("'{}' is not a valid c attribute", o))],
                    );
                    std::process::exit(9);
                }
            }
        }

        match &ret {
            None => write!(self.f, "void ").unwrap(),
            Some(a) => {
                write!(self.f, "{} ", self.to_local_typed_name(&a.typed)).unwrap();
                self.emit_pointer(&a.typed.ptr);
            }
        };

        if ast.name.ends_with("::main") {
            write!(self.f, "main (").unwrap();
        } else {
            match &vis {
                ast::Visibility::Object => (),
                ast::Visibility::Shared => {
                    write!(self.f, "__attribute__ ((visibility (\"hidden\"))) ").unwrap()
                }
                ast::Visibility::Export => {
                    write!(self.f, "__attribute__ ((visibility (\"default\"))) ").unwrap()
                }
            }
            write!(self.f, "{} (", name.0[1..].join("_")).unwrap();
        }

        self.function_args(args);
        if vararg {
            write!(self.f, ", ...").unwrap();
        }
        write!(self.f, ")\n").unwrap();

        write!(self.f, "#if 0\n").unwrap();

        let mut has_default = false;
        for (loc, expr, body) in &body.branches {
            if let Some(expr) = &expr {
                self.emit_loc(&expr.loc());
                self.inside_macro = true;
                write!(self.f, "#elif ").unwrap();
                self.emit_cppexpr(expr);
                write!(self.f, "\n").unwrap();
                self.inside_macro = false;
            } else {
                has_default = true;
                write!(self.f, "#else\n").unwrap();
            }
            self.emit_zblock(&body, true, None);
        }

        if !has_default {
            write!(self.f, "#else\n;\n").unwrap();
        }

        write!(self.f, "#endif\n").unwrap();

        write!(self.f, "\n").unwrap();
    }

    fn emit_flags(&mut self, ast: &ast::Local)
    {
        let (body) = match &ast.def {
            ast::Def::Flags{
                body,
                ..
            } => (body),
            _ => unreachable!(),
        };

        write!(self.f, "#if 0\n").unwrap();

        for (loc, expr, body) in &body.branches {
            if let Some(expr) = &expr {
                self.emit_loc(&expr.loc());
                self.inside_macro = true;
                write!(self.f, "#elif ").unwrap();
                self.emit_cppexpr(expr);
                write!(self.f, "\n").unwrap();
                self.inside_macro = false;
            } else {
                write!(self.f, "#else\n").unwrap();
            }

            for stm in &body.statements {
                if let ast::Statement::Expr{expr, loc} = stm.as_ref() {
                    if let ast::Expression::Call{name, args, ..} = expr {
                        if let ast::Expression::Name(name) = name.as_ref() {
                            if let ast::Type::Other(name) = &name.t {
                                if name.to_string() == "linker" {
                                    if args.len() != 1 {
                                        emit_error(format!("invalid flags statement"),
                                            &[(loc.clone(), "link flag tage a single string arg")]);
                                        std::process::exit(9);
                                    }
                                    if let ast::Expression::LiteralString{v,..} = args[0].as_ref() {
                                        write!(self.f, "#pragma comment(linker, \"").unwrap();
                                        self.f.write(&v).unwrap();
                                        write!(self.f, "\")\n").unwrap();
                                        continue;
                                    } else {
                                        emit_error(format!("invalid flags statement"),
                                        &[(expr.loc().clone(), "literal string required")]);
                                    }
                                }
                                emit_error(format!("invalid flags statement"),
                                    &[(expr.loc().clone(), format!("unknown flag {}", name))]);
                            }
                        }
                    }
                    emit_error(format!("invalid flags statement"), &[(loc.clone(), "this expression does not add a flag")]);
                    std::process::exit(9);
                }
            }
        }

        write!(self.f, "#endif\n").unwrap();

    }

    fn emit_statement(&mut self, stm: &ast::Statement) -> bool /* ends with semicolon */ {
        match stm {
            ast::Statement::Mark { .. } => false,
            ast::Statement::Break { loc, label } => {
                self.emit_loc(&loc);
                if let Some(label) = label {
                    write!(self.f, "goto ___EXIT_HERE_{}", label).unwrap();
                } else {
                    write!(self.f, "break").unwrap();
                }
                true
            }
            ast::Statement::Label { loc, label } => {
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
                self.emit_zblock(b2, true, None);
                false
            }
            ast::Statement::CBlock { loc, lit } => {
                self.emit_loc(&loc);
                write!(self.f, "{}", lit).unwrap();
                false
            }
            ast::Statement::Block(b2) => {
                self.emit_zblock(b2, true, None);
                false
            }
            ast::Statement::For {label, e1, e2, e3, body } => {
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
                self.emit_zblock(body, true, label.clone());
                false
            }
            ast::Statement::While { expr, body } => {
                write!(self.f, "while (").unwrap();
                self.emit_expr(expr);
                write!(self.f, ")").unwrap();
                self.emit_zblock(body, true, None);
                false
            }
            ast::Statement::If { branches } => {
                if branches.len() < 1 {
                    return false;
                }
                let mut branches = branches.iter();

                write!(self.f, "if (").unwrap();
                let ifc = branches.next().unwrap();
                self.emit_expr(ifc.1.as_ref().unwrap());
                write!(self.f, ")").unwrap();
                self.emit_zblock(&ifc.2, true, None);

                for branch in branches {
                    if let Some(expr) = &branch.1 {
                        write!(self.f, " else if (").unwrap();
                        self.emit_expr(expr);
                        write!(self.f, ")").unwrap();
                    } else {
                        write!(self.f, " else ").unwrap();
                    }
                    self.emit_zblock(&branch.2, true, None);
                }

                false
            }
            ast::Statement::Assign { lhs, rhs, loc, op } => {
                self.emit_loc(&loc);
                self.emit_expr(lhs);
                write!(
                    self.f,
                    " {} ",
                    match op {
                        ast::AssignOperator::Bitor => "|=",
                        ast::AssignOperator::Bitand => "&=",
                        ast::AssignOperator::Add => "+=",
                        ast::AssignOperator::Sub => "-=",
                        ast::AssignOperator::Eq => "=",
                    }
                )
                .unwrap();
                self.emit_expr(rhs);
                true
            }
            ast::Statement::Var {
                assign,
                loc,
                typed,
                name,
                array,
                tags,
            } => {
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
            ast::Statement::Expr { expr, loc } => {
                self.emit_loc(&loc);
                self.emit_expr(expr);
                true
            }
            ast::Statement::Continue { loc } => {
                self.emit_loc(&loc);
                write!(self.f, "continue").unwrap();
                true
            }
            ast::Statement::Return { expr, loc } => {
                self.emit_loc(&loc);
                write!(self.f, "  return ").unwrap();
                if let Some(expr) = expr {
                    self.emit_expr(expr);
                }
                true
            }
            ast::Statement::Switch {
                loc,
                expr,
                cases,
                default,
            } => {
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
                    self.emit_zblock(block, true, None);
                    write!(self.f, "break;}}\n").unwrap();
                }
                if let Some(default) = default {
                    write!(self.f, "default: {{\n").unwrap();
                    self.emit_zblock(default, true, None);
                    write!(self.f, "break;}}\n").unwrap();
                }
                write!(self.f, "}}\n").unwrap();
                false
            }
            ast::Statement::MacroCall { .. } => false,
        }
    }

    fn emit_zblock(&mut self, v: &ast::Block, realblock: bool, exitlabel: Option<String>) {
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

        if let Some(label) = &exitlabel {
            write!(self.f, "___CONTINUE_HERE_{}: ((void)0);\n", label).unwrap();
        }

        if realblock {
            if self.inside_macro {
                write!(self.f, "}}\\\n").unwrap();
            } else {
                write!(self.f, "\n}}\n").unwrap();
            }
        }

        if let Some(label) = &exitlabel {
            write!(self.f, "___EXIT_HERE_{}: ((void)0);\n", label).unwrap();
        }
    }

    pub fn emit_pointer(&mut self, v: &Vec<ast::Pointer>) {
        for ptr in v {
            if !ptr.tags.contains_key("mut") && !ptr.tags.contains_key("mut") {
                write!(self.f, " const ").unwrap();
            }
            write!(self.f, "* ").unwrap();
        }
    }

    fn emit_cppexpr(&mut self, v: &ast::Expression) {
        match v {
            ast::Expression::Cpp {loc, expr} => {
                self.emit_expr(expr);
            }
            _ =>  {
                emit_error(format!("expression not usable (yet?) in cpp context"), &[(v.loc().clone(), "here")]);
                std::process::exit(9);
            }
        }
    }

    fn emit_expr(&mut self, v: &ast::Expression) {
        match v {
            ast::Expression::Unsafe { expr, .. } => {
                self.emit_expr(expr);
            }
            ast::Expression::MacroCall { loc, .. } => {
                emit_error(format!("internal compiler error"), &[(loc.clone(), "ICE: macro not available yet")]);
                std::process::exit(9);
            }
            ast::Expression::ArrayInit { fields, loc } => {
                self.emit_loc(&loc);
                write!(self.f, "{{").unwrap();
                for field in fields {
                    self.emit_expr(field);
                    write!(self.f, ",").unwrap();
                }
                write!(self.f, "}}").unwrap();
            }
            ast::Expression::StructInit { typed, fields, loc } => {
                self.emit_loc(&loc);

                // gcc thinks this isnt const. bleh
                if !self.inside_constant_expr {
                    write!(self.f, "    ({})", self.to_local_typed_name(&typed)).unwrap();
                }
                write!(self.f, "{{").unwrap();
                for (name, field) in fields {
                    write!(self.f, ".{} = ", name).unwrap();
                    self.emit_expr(field);
                    write!(self.f, ",").unwrap();
                }
                write!(self.f, "}}").unwrap();
            }
            ast::Expression::UnaryPost { expr, loc, op } => {
                write!(self.f, "(").unwrap();
                self.emit_loc(&loc);
                self.emit_expr(expr);
                write!(
                    self.f,
                    " {}",
                    match op {
                        ast::PostfixOperator::Increment => "++",
                        ast::PostfixOperator::Decrement => "--",
                    }
                )
                .unwrap();
                write!(self.f, ")").unwrap();
            }
            ast::Expression::UnaryPre { expr, loc, op } => {
                write!(self.f, "(").unwrap();
                self.emit_loc(&loc);
                write!(
                    self.f,
                    " {}",
                    match op {
                        ast::PrefixOperator::Boolnot => "!",
                        ast::PrefixOperator::Bitnot => "~",
                        ast::PrefixOperator::Increment => "++",
                        ast::PrefixOperator::Decrement => "--",
                        ast::PrefixOperator::AddressOf => "&",
                        ast::PrefixOperator::Deref => "*",
                    }
                )
                .unwrap();
                self.emit_expr(expr);
                write!(self.f, ")").unwrap();
            }
            ast::Expression::Cast { into, expr, .. } => {
                write!(self.f, "    ({}", self.to_local_typed_name(&into)).unwrap();
                self.emit_pointer(&into.ptr);
                write!(self.f, ")").unwrap();
                write!(self.f, "(").unwrap();
                self.emit_expr(expr);
                write!(self.f, ")").unwrap();
            }
            ast::Expression::Name(name) => {
                self.emit_loc(&name.loc);
                write!(self.f, "    {}", self.to_local_typed_name(&name)).unwrap();
            }
            ast::Expression::LiteralString { loc, v } => {
                self.emit_loc(&loc);
                write!(self.f, "    \"").unwrap();
                for c in v {
                    self.write_escaped_literal(*c, true);
                }
                write!(self.f, "\"").unwrap();
            }
            ast::Expression::LiteralChar { loc, v } => {
                self.emit_loc(&loc);
                write!(self.f, "    '").unwrap();
                self.write_escaped_literal(*v, false);
                write!(self.f, "'").unwrap();
            }
            ast::Expression::Literal { loc, v } => {
                self.emit_loc(&loc);
                match parser::parse_int(&v) {
                    // Output as decimal if an integer to avoid warnings for binary literals
                    // (binary literals are a GNU extension)
                    Some(parser::Integer::Signed(int)) => {
                        write!(self.f, "    {}", int).unwrap();
                    }
                    Some(parser::Integer::Unsigned(int)) => {
                        write!(self.f, "    {}", int).unwrap();
                    }
                    // If the literal is not an integer (boolean literals, for example), write
                    // it as-is
                    None => {
                        write!(self.f, "    {}", v).unwrap();
                    }
                }
            }
            ast::Expression::Call {
                loc,
                name,
                args,
                emit,
                ..
            } => {
                match emit {
                    ast::EmitBehaviour::Default => {}
                    ast::EmitBehaviour::Skip => {
                        return;
                    }
                    ast::EmitBehaviour::Error { loc, message } => {
                        emit_error(format!("{}", message), &[(loc.clone(), "here")]);
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
            }
            ast::Expression::Infix {
                lhs, rhs, op, loc, ..
            } => {
                write!(self.f, "(").unwrap();
                self.emit_expr(lhs);
                self.emit_loc(&loc);
                write!(
                    self.f,
                    " {}",
                    match op {
                        ast::InfixOperator::Equals => "==",
                        ast::InfixOperator::Nequals => "!=",
                        ast::InfixOperator::Add => "+",
                        ast::InfixOperator::Subtract => "-",
                        ast::InfixOperator::Multiply => "*",
                        ast::InfixOperator::Divide => "/",
                        ast::InfixOperator::Bitxor => "^",
                        ast::InfixOperator::Booland => "&&",
                        ast::InfixOperator::Boolor => "||",
                        ast::InfixOperator::Moreeq => ">=",
                        ast::InfixOperator::Lesseq => "<=",
                        ast::InfixOperator::Lessthan => "<",
                        ast::InfixOperator::Morethan => ">",
                        ast::InfixOperator::Shiftleft => "<<",
                        ast::InfixOperator::Shiftright => ">>",
                        ast::InfixOperator::Modulo => "%",
                        ast::InfixOperator::Bitand => "&",
                        ast::InfixOperator::Bitor => "|",
                    }
                )
                .unwrap();
                self.emit_expr(rhs);
                write!(self.f, "  )").unwrap();
            }
            ast::Expression::MemberAccess { loc, lhs, rhs, op } => {
                self.emit_loc(&loc);
                self.emit_expr(lhs);
                write!(self.f, " {}{}", op, rhs).unwrap();
            }
            ast::Expression::ArrayAccess { loc, lhs, rhs } => {
                self.emit_loc(&loc);
                self.emit_expr(lhs);
                write!(self.f, " [ ").unwrap();
                self.emit_expr(rhs);
                write!(self.f, "]").unwrap();
            }
            ast::Expression::Cpp {loc, ..} => {
                parser::emit_error(
                    "invalid c preprocessor directive in local expression location".to_string(),
                    &[(
                        loc.clone(),
                        format!("c preprocessor expression not possible in this location"),
                    )],
                );
                std::process::exit(9);
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

impl CFile {
    pub fn is_newer_than(&self, target: &str) -> bool {
        let itarget = match std::fs::metadata(&target) {
            Ok(v) => v,
            Err(_) => return true,
        };
        let itarget = itarget
            .modified()
            .expect(&format!("cannot stat {}", target));

        for source in &self.sources {
            let isource = std::fs::metadata(source).expect(&format!("cannot stat {:?}", source));

            let isource = isource
                .modified()
                .expect(&format!("cannot stat {:?}", source));

            if isource > itarget {
                return true;
            }
        }
        return false;
    }
}

pub fn builtin(
    project: &Project,
    stage: &make::Stage,
    artifact: &super::project::Artifact,
    symbols: HashSet<Name>,
) -> CFile {
    let p = format!(
        "target/gen/zz_builtins_{}_{}_{:?}.c",
        project.name, artifact.name, artifact.typ
    );
    let mut f = fs::File::create(&p).expect(&format!("cannot create {}", p));

    write!(
        f,
        "#ifndef ZZ_EXPORT_HEADER___zz__builtins\n#define ZZ_EXPORT_HEADER___zz__builtins\n"
    )
    .unwrap();
    write!(f, "#include <stddef.h>\n").unwrap();

    let mut i = 1;
    for symbol in &symbols {
        write!(
            f,
            "const __attribute__ ((unused)) size_t {} = {};\n",
            symbol.0[1..].join("_"),
            i
        )
        .unwrap();
        i += 1;
    }

    write!(
        f,
        "size_t __attribute__ ((unused)) __zz_symbol_names_len = {};\n",
        i
    )
    .unwrap();
    write!(
        f,
        "const char * __attribute__ ((unused)) __zz_symbol_names[] = {{0,\n"
    )
    .unwrap();
    for symbol in &symbols {
        write!(f, "    \"{}\",\n", symbol.0[1..].join("::")).unwrap();
    }
    write!(f, "}};\n").unwrap();

    write!(f, "#endif\n").unwrap();

    CFile {
        name: Name::from("__zz_builtins"),
        filepath: p,
        sources: HashSet::new(),
        deps: HashSet::new(),
        symbols: HashSet::new(),
        cflags: Vec::new(),
        lflags: Vec::new(),
    }
}
