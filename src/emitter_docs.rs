#![allow(unused)]

use super::ast;
use super::flatten;
use super::make;
use super::name::Name;
use super::project::Project;
use askama::Template;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;
use std::io::Write;
use std::path::PathBuf;

pub struct CFile {
    pub name: Name,
    pub filepath: String,
    pub sources: HashSet<PathBuf>,
    pub deps: HashSet<Name>,
}

pub struct Emitter {
    p: String,
    project_name: String,
    f: fs::File,
    module: flatten::Module,
    cur_loc: Option<ast::Location>,
    register_structs: Vec<String>,
    register_fns: Vec<(String, String)>,
}

pub fn outname(_project: &Project, stage: &make::Stage, module: &flatten::Module) -> String {
    super::project::target_dir().join("docs")
        .join(format!("{}.html", module.name.0[1..].join("_")))
        .to_string_lossy().to_string()
}

impl Emitter {
    pub fn new(project: &Project, stage: make::Stage, module: flatten::Module) -> Self {
        let td = super::project::target_dir().join(stage.to_string());
        std::fs::create_dir_all(super::project::target_dir().join("docs")).unwrap();

        let p = outname(project, &stage, &module);
        let f = fs::File::create(&p).expect(&format!("cannot create {}", p));

        Emitter {
            p,
            project_name: project.name.clone(),
            f,
            module,
            cur_loc: None,
            register_structs: Vec::new(),
            register_fns: Vec::new(),
        }
    }

    pub fn emit(mut self) {
        let module = self.module.clone();
        write!(
            self.f,
            r#"
<h1> module {} </h1>
"#,
            self.module.name.0[1..].join("::")
        )
        .unwrap();

        for (d, complete) in &module.d {
            if d.vis == ast::Visibility::Object {
                continue;
            }
            if complete != &flatten::TypeComplete::Complete {
                continue;
            }
            match d.def {
                ast::Def::Function { .. } => {
                    self.emit_fndecl(&d);
                }
                _ => (),
            }
        }
        for (d, complete) in &module.d {
            if d.vis == ast::Visibility::Object {
                continue;
            }
            if complete != &flatten::TypeComplete::Complete {
                continue;
            }

            let mut dmodname = Name::from(&d.name);
            dmodname.pop();
            if dmodname != module.name {
                continue;
            }

            match d.def {
                ast::Def::Macro { .. } => {}
                ast::Def::Const { .. } => self.emit_const(&d),
                ast::Def::Static { .. } => self.emit_static(&d),
                ast::Def::Struct { .. } => {
                    self.emit_struct(&d, None);
                    if let Some(vs) = module.typevariants.get(&Name::from(&d.name)) {
                        for (v, _) in vs {
                            let mut d = d.clone();
                            d.name = format!("{}_{}", d.name, v);
                            self.emit_struct(&d, Some(*v));
                        }
                    }
                }
                ast::Def::Symbol { .. } => {}
                ast::Def::Enum { .. } => self.emit_enum(&d),
                ast::Def::Function { .. } => {}
                ast::Def::Closure { .. } => {
                    self.emit_closure(&d);
                }
                ast::Def::Theory { .. } => {}
                ast::Def::Testcase { .. } => {}
                ast::Def::Include { .. } => {}
            }
            write!(self.f, "\n").unwrap();
        }
    }

    pub fn emit_static(&mut self, ast: &ast::Local) {
        let (_typed, _expr, _tags, storage, _array) = match &ast.def {
            ast::Def::Static {
                typed,
                expr,
                tags,
                storage,
                array,
            } => (typed, expr, tags, storage, array),
            _ => unreachable!(),
        };

        match storage {
            ast::Storage::Atomic => {
                return;
            }
            ast::Storage::ThreadLocal => {
                return;
            }
            ast::Storage::Static => (),
        }
    }

    pub fn emit_const(&mut self, ast: &ast::Local) {
        let (_typed, _expr) = match &ast.def {
            ast::Def::Const { typed, expr } => (typed, expr),
            _ => unreachable!(),
        };
    }

    pub fn emit_enum(&mut self, ast: &ast::Local) {
        let _names = match &ast.def {
            ast::Def::Enum { names } => (names),
            _ => unreachable!(),
        };
    }

    pub fn emit_struct(&mut self, ast: &ast::Local, _tail_variant: Option<u64>) {
        let (fields, _packed, _tail, _union, impls) = match &ast.def {
            ast::Def::Struct {
                fields,
                packed,
                tail,
                union,
                impls,
                ..
            } => (fields, packed, tail, union, impls),
            _ => unreachable!(),
        };
    }

    pub fn emit_closure(&mut self, ast: &ast::Local) {
        let (_ret, _args, _attr) = match &ast.def {
            ast::Def::Closure {
                ret,
                args,
                attr,
                ..
            } => (ret, args, attr),
            _ => unreachable!(),
        };
    }

    pub fn emit_fndecl(&mut self, ast: &ast::Local) {
        let (ret, args, _body, _vararg, _attr) = match &ast.def {
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

        let mut sargs = HashMap::new();
        for arg in args {
            sargs.insert(arg.name.clone(), format!("{}", arg.typed));
        }
        let tpl = FunctionHtml {
            doc: ast.doc.clone().replace("\n", "<br>"),
            name: Name::from(&ast.name).0.last().unwrap().clone(),
            ret: ret.as_ref().map(|r| format!("{}", r.typed)),
            args: sargs,
        };
        write!(self.f, "{}", tpl.render().unwrap()).unwrap();
    }
}

#[derive(Template)]
#[template(path = "function.html")]
struct FunctionHtml {
    name: String,
    doc: String,
    ret: Option<String>,
    args: HashMap<String, String>,
}
