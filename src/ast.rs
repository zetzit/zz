use std::collections::HashMap;
use std::fmt;
use std::path::PathBuf;

pub struct Location {
    pub line:   usize,
    pub file:   String,
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.file, self.line)
    }
}

pub enum Visibility {
    Shared,
    Object,
    Export,
}

pub struct Import {
    pub name:       String,
    pub namespace:  Vec<String>,
    pub loc:        Location,
}

pub struct Const {
    pub typ:    String,
    pub name:   String,
    pub expr:   String,
    pub vis:    Visibility,
    pub loc:    Location,
}

#[derive(Default)]
pub struct Module {
    pub namespace:  Vec<String>,
    pub functions:  HashMap<String, Function>,
    pub imports:    Vec<Import>,
    pub structs:    Vec<Struct>,
    pub includes:   Vec<String>,
    pub constants:  HashMap<String, Const>,
    pub sources:    Vec<PathBuf>,
}

pub struct AnonArg {
    pub typ:    String
}

pub struct NamedArg {
    pub typ:    String,
    pub name:   String,
    pub muta:   bool,
    pub ptr:    bool,
    pub namespace: Option<String>,
}

pub struct Function {
    pub ret:    Option<AnonArg>,
    pub args:   Vec<NamedArg>,
    pub name:   String,
    pub body:   String,
    pub vis:    Visibility,
    pub loc:    Location,
}

pub struct Struct {
    pub name:   String,
    pub body:   String,
    pub vis:    Visibility,
    pub loc:    Location,
}
