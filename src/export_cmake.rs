use super::make::Make;
use std::fs;
use std::io::Write;

pub fn export(make: Make) {
    let p = std::path::PathBuf::from(&format!("{}.cmake", make.artifact.name));
    let mut f = fs::File::create(&p).expect(&format!("cannot create {:?}", p));


//include_directories("/home/aep/proj/devguard/carrier/target/release/include/")


    write!(f, r#"
include_directories("${{CMAKE_CURRENT_LIST_DIR}}/target/{}/include/")
set({}_SOURCES
"#, make.stage, make.artifact.name).unwrap();

    for step in &make.steps {
        write!(f, "     ${{CMAKE_CURRENT_LIST_DIR}}/{}\n", step.source.to_string_lossy()).unwrap();
    }

    write!(f, ")\n").unwrap();
}

