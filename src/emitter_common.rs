use std::path::PathBuf;
use std::path::Path;

pub fn path_rel<A: AsRef<Path>, B: AsRef<Path>> (base: A, src: B) -> PathBuf {
    let mut src : PathBuf = src.as_ref().into();
    if !src.is_absolute() {
        src = src.canonicalize().expect(&format!("canonicalize {:?}", src));
    }
    return pathdiff::diff_paths(&src, base.as_ref()).expect(&format!("pathdiff {:?} ", src));
}
