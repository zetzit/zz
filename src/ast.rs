use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::path::PathBuf;

#[derive(PartialEq, Clone)]
pub struct Location {
    pub line:   usize,
    pub file:   String,
    pub span:   pest::Span<'static>,
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.file, self.line)
    }
}


#[derive(Clone)]
pub enum Storage {
    Static,
    ThreadLocal,
    Atomic,
}

#[derive(Clone)]
pub enum Visibility {
    Shared,
    Object,
    Export,
}

#[derive(Clone)]
pub struct Import {
    pub namespace:  Vec<String>,
    pub vis:        Visibility,
    pub loc:        Location,
}

#[derive(Clone)]
pub struct Static {
    pub typ:    String,
    pub name:   String,
    pub expr:   String,
    pub muta:   bool,
    pub storage: Storage,
    pub loc:    Location,
}

#[derive(Clone)]
pub struct Const {
    pub typ:    String,
    pub name:   String,
    pub expr:   String,
    pub vis:    Visibility,
    pub loc:    Location,
}

#[derive(Clone)]
pub struct Include {
    pub expr:   String,
    pub loc:    Location,
    pub vis:    Visibility,
}

#[derive(Default, Clone)]
pub struct Module {
    pub namespace:  Vec<String>,
    pub source:     PathBuf,
    pub functions:  HashMap<String, Function>,
    pub macros:     HashMap<String, Macro>,
    pub constants:  HashMap<String, Const>,
    pub statics:    HashMap<String, Static>,
    pub structs:    Vec<Struct>,
    pub imports:    Vec<Import>,
    pub includes:   Vec<Include>,
    pub sources:    HashSet<PathBuf>,
}

#[derive(Clone)]
pub struct AnonArg {
    pub typ:    String
}

#[derive(Clone)]
pub struct NamedArg {
    pub typ:    String,
    pub name:   String,
    pub muta:   bool,
    pub ptr:    bool,
    pub namespace: Option<String>,
}

#[derive(Clone)]
pub struct Function {
    pub ret:    Option<AnonArg>,
    pub args:   Vec<NamedArg>,
    pub name:   String,
    pub body:   String,
    pub vis:    Visibility,
    pub loc:    Location,
}

#[derive(Clone)]
pub struct Macro {
    pub args:       Vec<String>,
    pub name:       String,
    pub body:       String,
    pub imports:    Vec<Import>,
    pub vis:        Visibility,
    pub loc:        Location,
}

#[derive(Clone)]
pub struct Struct {
    pub name:   String,
    pub body:   String,
    pub vis:    Visibility,
    pub loc:    Location,
    pub packed: bool,
}
