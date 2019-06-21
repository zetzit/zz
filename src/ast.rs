use std::collections::HashMap;

pub struct Location {
    pub line:  usize,
    pub file:   String,
}

pub enum Visibility {
    Shared,
    Object,
    Export,
}

pub struct Import {
    pub name:       String,
    pub namespace:  String,
    pub loc:        Location,
}

#[derive(Default)]
pub struct Module {
    pub name:       String,
    pub functions:  HashMap<String, Function>,
    pub imports:    Vec<Import>,
    pub structs:    Vec<Struct>,
    pub includes:   Vec<String>,
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
