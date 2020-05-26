use super::make::Make;
use std::fs;
use std::io::Write;

pub fn export(make: Make) {
    let pdir_ = format!("target/esp32/{}/", make.artifact.name);
    let pdir = std::path::Path::new(&pdir_);
    std::fs::create_dir_all(&pdir).unwrap();

    let p = pdir.join("CMakeLists.txt");
    let mut f = fs::File::create(&p).expect(&format!("cannot create {:?}", p));

    write!(f, "idf_component_register(SRCS \n").unwrap();
    for step in &make.steps {
        if step.source.is_absolute() {
            write!(f, "     {}\n", step.source.to_string_lossy()).unwrap();
        } else {
            write!(
                f,
                "     ${{CMAKE_CURRENT_LIST_DIR}}/../../../{}\n",
                step.source.to_string_lossy()
            )
            .unwrap();
        }
    }
    write!(
        f,
        "INCLUDE_DIRS ${{CMAKE_CURRENT_LIST_DIR}}/../../{}/include/\n",
        make.stage
    )
    .unwrap();

    if make.artifact.requires.len() > 0 {
        write!(f, " REQUIRES ").unwrap();
    }
    for r in &make.artifact.requires {
        write!(f, " {} \n", r).unwrap();
    }

    write!(f, ")\n").unwrap();

    write!(f, " target_compile_options(${{COMPONENT_LIB}} PRIVATE \n").unwrap();
    write!(
        f,
        "
    -fdiagnostics-color=always
    -Wno-error=duplicate-decl-specifier
    -Wno-unused-function
    -Wno-attributes
"
    )
    .unwrap();
    for cflag in &make.cflags {
        write!(f, "{}\n", cflag).unwrap();
    }
    write!(f, ")\n").unwrap();
}
