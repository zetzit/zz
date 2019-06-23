use std::collections::HashMap;
use std::fmt;
use std::path::PathBuf;

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

pub enum Visibility {
    Shared,
    Object,
    Export,
}

pub struct Import<'a> {
    pub name:       String,
    pub namespace:  Vec<String>,
    pub loc:        Location<'a>,
}

pub struct Const<'a> {
    pub typ:    String,
    pub name:   String,
    pub expr:   String,
    pub vis:    Visibility,
    pub loc:    Location<'a>,
}

pub struct Include<'a> {
    pub expr:   String,
    pub loc:        Location<'a>,
}

#[derive(Default)]
pub struct Module<'a> {
    pub namespace:  Vec<String>,
    pub functions:  HashMap<String, Function<'a>>,
    pub imports:    Vec<Import<'a>>,
    pub structs:    Vec<Struct<'a>>,
    pub includes:   Vec<Include<'a>>,
    pub constants:  HashMap<String, Const<'a>>,
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

pub struct Function<'a> {
    pub ret:    Option<AnonArg>,
    pub args:   Vec<NamedArg>,
    pub name:   String,
    pub body:   String,
    pub vis:    Visibility,
    pub loc:    Location<'a>,
}

pub struct Struct<'a> {
    pub name:   String,
    pub body:   String,
    pub vis:    Visibility,
    pub loc:    Location<'a>,
}
