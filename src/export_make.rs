use super::make::Make;
use std::fs;
use std::io::Write;
use super::emitter_common;

pub fn export(make: Make) {
    let td      = super::project::target_dir();
    let pdir_   = td.join("make").join(&make.artifact.name);
    let pdir = std::path::Path::new(&pdir_);
    std::fs::create_dir_all(&pdir).unwrap();

    let p = pdir.join("Makefile.include");
    let mut f = fs::File::create(&p).expect(&format!("cannot create {:?}", p));





    write!(
        f,
        r#"

{an}_TARGET_DIR := $(dir $(lastword $(MAKEFILE_LIST)))

{an}_SOURCES=\
"#,
        an=make.artifact.name
    )
    .unwrap();

    for step in &make.steps {
        write!(
            f,
            "     ${{{an}_TARGET_DIR}}/{ss}\\\n",
            an = make.artifact.name,
            ss = emitter_common::path_rel(&pdir, &step.source).to_string_lossy().to_string(),
        )
        .unwrap();
    }

    write!(f, "\n").unwrap();


    write!(f, "{an}_CINCLUDES += ${{{an}_TARGET_DIR}} \\\n", an = make.artifact.name).unwrap();
    for ss in &make.cincludes{
        write!(f, "  ${{{an}_TARGET_DIR}}/{ss}\\\n", an = make.artifact.name,
               ss = emitter_common::path_rel(&pdir,  ss).to_string_lossy().to_string(),
        ).unwrap();
    }
    write!(f, "\n").unwrap();


}
