use super::make::Make;
use std::fs;
use std::io::Write;

pub fn export(make: Make) {

    //let pdir_ = format!("target/export/esp/{}/", make.artifact.name);
    //let pdir = std::path::Path::new(&pdir_);
    //std::fs::create_dir_all(&pdir).unwrap();

    let p = std::path::Path::new("CMakeLists.txt");
    let mut f = fs::File::create(&p).expect(&format!("cannot create {:?}", p));


    write!(f, "idf_component_register(SRCS \n").unwrap();
    for step in &make.steps {
        write!(f, "     ${{CMAKE_CURRENT_LIST_DIR}}/{}\n", step.source.to_string_lossy()).unwrap();
    }
    write!(f, "INCLUDE_DIRS ${{CMAKE_CURRENT_LIST_DIR}}/target/{}/include/", make.stage).unwrap();


    write!(f, ")\n").unwrap();



    //TODO need to copy all the flags from make
    write!(f, " target_compile_options(${{COMPONENT_LIB}} PRIVATE -Wno-attributes)\n").unwrap();

}

