#![allow(unused)]

use super::ast;
use super::flatten;
use super::make;
use super::name::Name;
use super::parser::{self, emit_error};
use super::project::Project;
use std::collections::HashSet;
use std::fs;
use std::io::Write;
use std::path::PathBuf;

pub struct Emitter {
    p: String,
    project_name: String,
    f: fs::File,
    module: flatten::Module,
}

pub fn make_module(make: &super::make::Make) {
    let td = super::project::target_dir();

    let pdir_ = td.join("go").join(&make.artifact.name);
    let pdir = std::path::Path::new(&pdir_);
    std::fs::create_dir_all(&pdir).unwrap();

    let p = pdir.join(format!("{}.c", make.artifact.name));
    let mut f = fs::File::create(&p).expect(&format!("cannot create {:?}", p));

    for step in &make.steps {
        let s = step.source.parent().unwrap();
        if s.file_name().unwrap() == "zz" || s.file_name().unwrap() == "gen"{
            let s = s
                .parent()
                .unwrap()
                .join("js")
                .join(step.source.file_name().unwrap());

            let c = fs::read_to_string(&step.source).unwrap();
            f.write(c.as_bytes());
        } else {
            let f = step.source.to_string_lossy().replace("/", "_");
            let p = pdir.join(f);
            let mut f = fs::File::create(&p).expect(&format!("cannot create {:?}", p));
            write!(f, "#include \"../../../{}\"\n", step.source.to_string_lossy()).unwrap();
        }
    }



    let p = pdir.join("go.mod");
    let mut f = fs::File::create(&p).expect(&format!("cannot create {:?}", p));
    write!(f, "module {}\n", make.artifact.name).unwrap();
    write!(f, "go 1.14\n").unwrap();



    let p = pdir.join(format!("{}.h", make.artifact.name));
    let mut f = fs::File::create(&p).expect(&format!("cannot create {:?}", p));
    for step in &make.steps {
        let mut b = step.source.file_stem().unwrap().to_string_lossy();
        let p = format!("target/include/zz/{}.h", b);
        let p = std::path::Path::new(&p);
        if p.exists() {
            let c = fs::read_to_string(&p).unwrap();
            f.write(c.as_bytes());
        }
    }



    let p = pdir.join(format!("{}.go", make.artifact.name));
    let mut f = fs::File::create(&p).expect(&format!("cannot create {:?}", p));
    write!(f, "package {}\n\n/*\n", make.artifact.name).unwrap();
    write!(f, "#include \"{}.h\"\n", make.artifact.name).unwrap();
    write!(f, "\n*/\nimport \"C\"\n").unwrap();
}
