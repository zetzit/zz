use super::make::Make;
use std::fs;
use std::io::Write;

pub fn export(make: Make) {
    let pdir_ = format!("target/make/{}/", make.artifact.name);
    let pdir = std::path::Path::new(&pdir_);
    std::fs::create_dir_all(&pdir).unwrap();

    let p = pdir.join("Makefile.include");
    let mut f = fs::File::create(&p).expect(&format!("cannot create {:?}", p));





    write!(
        f,
        r#"

{an}_SOURCE_DIR := $(dir $(lastword $(MAKEFILE_LIST)))/../../..

{an}_SOURCES=\
"#,
        an=make.artifact.name
    )
    .unwrap();

    for step in &make.steps {
        write!(
            f,
            "     ${{{an}_SOURCE_DIR}}/{ss}\\\n",
            an = make.artifact.name,
            ss = step.source.to_string_lossy(),
        )
        .unwrap();
    }

    write!(f, "\n").unwrap();


    write!(f, "{an}_CINCLUDES += ${{{an}_SOURCE_DIR}} \\\n", an = make.artifact.name).unwrap();
    for ss in &make.cincludes{
        write!(f, "  ${{{an}_SOURCE_DIR}}/{ss}\\\n", an = make.artifact.name, ss = ss).unwrap();
    }
    write!(f, "\n").unwrap();


}
