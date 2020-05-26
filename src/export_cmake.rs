use super::make::Make;
use std::fs;
use std::io::Write;

pub fn export(make: Make) {
    let pdir_ = format!("target/cmake/{}/", make.artifact.name);
    let pdir = std::path::Path::new(&pdir_);
    std::fs::create_dir_all(&pdir).unwrap();

    let p = pdir.join("CMakeLists.txt");
    let mut f = fs::File::create(&p).expect(&format!("cannot create {:?}", p));

    write!(
        f,
        r#"
include_directories("${{CMAKE_CURRENT_LIST_DIR}}/../../{}/")
set({}_SOURCES
"#,
        make.stage, make.artifact.name
    )
    .unwrap();

    for step in &make.steps {
        write!(
            f,
            "     ${{CMAKE_CURRENT_LIST_DIR}}/../../../{}\n",
            step.source.to_string_lossy()
        )
        .unwrap();
    }

    write!(f, ")\n").unwrap();
}
