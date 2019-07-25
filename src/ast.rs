use std::collections::HashSet;
use std::path::PathBuf;
use std::fmt;
use super::name::Name;

#[derive(PartialEq, Clone)]
pub struct Location {
    pub file:   String,
    pub span:   pest::Span<'static>,
}

impl Location {
    pub fn line(&self) -> usize {
        self.span.start_pos().line_col().0
    }
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.file, self.line())
    }
}


#[derive(Clone)]
pub enum Storage {
    Static,
    ThreadLocal,
    Atomic,
}

#[derive(Clone, PartialEq)]
pub enum Visibility {
    Shared,
    Object,
    Export,
}

#[derive(Clone, PartialEq)]
pub struct Import {
    pub name:   Name,
    pub local:  Vec<(String, Option<String>)>,
    pub vis:    Visibility,
    pub loc:    Location,
}


#[derive(Clone)]
pub enum Def {
    Static {
        typeref:    NameUse,
        expr:       Expression,
        muta:       bool,
        storage:    Storage,
    },
    Const {
        typeref:    NameUse,
        expr:       Expression,
    },
    Function {
        ret:        Option<AnonArg>,
        args:       Vec<NamedArg>,
        body:       Block,
    },
    Struct {
        fields:     Vec<Field>,
        packed:     bool,
    },
    Macro {
        args:       Vec<String>,
        body:       Block,
    }
}


#[derive(Clone)]
pub struct Local {
    pub name:   String,
    pub export_as: Option<String>,
    pub vis:    Visibility,
    pub loc:    Location,
    pub def:    Def,
}


#[derive(Clone)]
pub struct Include {
    pub expr:   String,
    pub loc:    Location,
}

#[derive(Clone)]
pub struct NameUse {
    pub name:   Name,
    pub loc:    Location,
    pub ptr:    bool,
}

#[derive(Default, Clone)]
pub struct Module {
    pub name:       Name,
    pub source:     PathBuf,
    pub locals:     Vec<Local>,
    pub includes:   Vec<Include>,
    pub imports:    Vec<Import>,
    pub sources:    HashSet<PathBuf>,
}

#[derive(Clone)]
pub struct AnonArg {
    pub typeref:    NameUse,
}

#[derive(Clone)]
pub struct NamedArg {
    pub typeref:    NameUse,
    pub name:       String,
    pub muta:       bool,
}


#[derive(Clone)]
pub enum Value{
    Literal(String),
    Name(NameUse),
}

#[derive(Clone)]
pub struct Field {
    pub typeref:    NameUse,
    pub name:       String,
    pub array:      Option<Value>,
    pub loc:        Location,
}


#[derive(Clone)]
pub enum Expression {
    Name(NameUse),
    MemberAccess {
        loc:    Location,
        lhs:    Box<Expression>,
        op:     String,
        rhs:    String,
    },
    ArrayAccess {
        loc:    Location,
        lhs:    Box<Expression>,
        rhs:    Box<Expression>,
    },
    Literal {
        loc:    Location,
        v:      String,
    },
    Call {
        loc:    Location,
        name:   NameUse,
        args:   Vec<Box<Expression>>,
    },
    InfixOperation {
        lhs:    Box<Expression>,
        rhs:    Vec<((String, Location), Box<Expression>)>,
    },
    Cast {
        into: NameUse,
        expr: Box<Expression>,
    },
    UnaryPost {
        loc:    Location,
        op:     String,
        expr:   Box<Expression>,
    },
    UnaryPre {
        loc:    Location,
        op:     String,
        expr:   Box<Expression>,
    },
}


#[derive(Clone)]
pub enum Statement {
    Label{
        loc:    Location,
        label:  String
    },
    Goto{
        loc:    Location,
        label:  String
    },
    Assign {
        loc: Location,
        lhs: Expression,
        op:  String,
        rhs: Expression,
    },
    Expr {
        loc:  Location,
        expr: Expression,
    },
    Return {
        loc:  Location,
        expr: Expression,
    },
    Var {
        loc:        Location,
        typeref:    NameUse,
        name:       Name,
        array:      Option<Expression>,
        assign:     Option<Expression>,
    },
    For {
        e1:     Option<Box<Statement>>,
        e2:     Option<Box<Statement>>,
        e3:     Option<Box<Statement>>,
        body:   Block,
    },
    Cond {
        op:         String,
        expr:       Option<Expression>,
        body:       Block,
    },
    Block(Box<Block>),
}

#[derive(Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
}
