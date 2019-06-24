use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::path::PathBuf;

#[derive(Clone)]
pub struct Location<'a> {
    pub line:   usize,
    pub file:   String,
    pub span:   pest::Span<'a>,
}

impl<'a> std::fmt::Display for Location<'a> {
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
pub struct Import<'a> {
    pub namespace:  Vec<String>,
    pub vis:        Visibility,
    pub loc:        Location<'a>,
}

#[derive(Clone)]
pub struct Static<'a> {
    pub typ:    String,
    pub name:   String,
    pub expr:   String,
    pub muta:   bool,
    pub storage: Storage,
    pub loc:    Location<'a>,
}

#[derive(Clone)]
pub struct Const<'a> {
    pub typ:    String,
    pub name:   String,
    pub expr:   String,
    pub vis:    Visibility,
    pub loc:    Location<'a>,
}

#[derive(Clone)]
pub struct Include<'a> {
    pub expr:   String,
    pub loc:    Location<'a>,
    pub vis:    Visibility,
}

#[derive(Default, Clone)]
pub struct Module<'a> {
    pub namespace:  Vec<String>,
    pub source:     PathBuf,
    pub functions:  HashMap<String, Function<'a>>,
    pub macros:     HashMap<String, Macro<'a>>,
    pub constants:  HashMap<String, Const<'a>>,
    pub statics:    HashMap<String, Static<'a>>,
    pub structs:    Vec<Struct<'a>>,
    pub imports:    Vec<Import<'a>>,
    pub includes:   Vec<Include<'a>>,
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
pub struct Function<'a> {
    pub ret:    Option<AnonArg>,
    pub args:   Vec<NamedArg>,
    pub name:   String,
    pub body:   String,
    pub vis:    Visibility,
    pub loc:    Location<'a>,
}

#[derive(Clone)]
pub struct Macro<'a> {
    pub args:       Vec<String>,
    pub name:       String,
    pub body:       String,
    pub imports:    Vec<Import<'a>>,
    pub vis:        Visibility,
    pub loc:        Location<'a>,
}

#[derive(Clone)]
pub struct Struct<'a> {
    pub name:   String,
    pub body:   String,
    pub vis:    Visibility,
    pub loc:    Location<'a>,
    pub packed: bool,
}
