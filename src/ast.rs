use std::collections::HashMap;
use std::collections::HashSet;
use std::path::PathBuf;
use std::fmt;
use super::name::Name;

#[derive(PartialEq, Clone, Debug)]
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
        tags:       HashMap<String, Location>,
        typed:      Typed,
        expr:       Expression,
        storage:    Storage,
    },
    Const {
        typed:      Typed,
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
    pub name:       String,
    pub export_as:  Option<String>,
    pub vis:        Visibility,
    pub loc:        Location,
    pub def:        Def,
}


#[derive(Clone)]
pub struct Include {
    pub expr:   String,
    pub loc:    Location,
}

#[derive(Clone, Debug)]
pub struct Pointer {
    pub loc:    Location,
    pub tags:   HashMap<String, Location>,
}

#[derive(Clone, Debug)]
pub struct Typed {
    pub name:   Name,
    pub loc:    Location,
    pub ptr:    Vec<Pointer>,
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
    pub typed:    Typed,
}

#[derive(Clone)]
pub struct NamedArg {
    pub typed:      Typed,
    pub name:       String,
    pub tags:       HashMap<String, Location>,
    pub loc:        Location,
}


#[derive(Clone)]
pub struct Field {
    pub typed:      Typed,
    pub name:       String,
    pub array:      Option<Expression>,
    pub tags:       HashMap<String, Location>,
    pub loc:        Location,
}


#[derive(Clone, Debug)]
pub enum Expression {
    Name(Typed),
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
        name:   Typed,
        args:   Vec<Box<Expression>>,
    },
    InfixOperation {
        loc:    Location,
        lhs:    Box<Expression>,
        rhs:    Vec<((String, Location), Box<Expression>)>,
    },
    Cast {
        loc:    Location,
        into:   Typed,
        expr:   Box<Expression>,
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
    StructInit {
        loc:        Location,
        typed:      Typed,
        fields:     Vec<(String,Box<Expression>)>,
    },
    ArrayInit {
        loc:        Location,
        fields:     Vec<Box<Expression>>,
    }
}

impl Expression {
    pub fn loc(&self) -> &Location {
        match self {
            Expression::Name(name)              => &name.loc,
            Expression::MemberAccess {loc,..}   => loc,
            Expression::ArrayAccess {loc,..}    => loc,
            Expression::Literal {loc,..}        => loc,
            Expression::Call {loc,..}           => loc,
            Expression::InfixOperation {loc,..} => loc,
            Expression::Cast {loc,..}           => loc,
            Expression::UnaryPost {loc,..}      => loc,
            Expression::UnaryPre {loc,..}       => loc,
            Expression::StructInit {loc,..}     => loc,
            Expression::ArrayInit {loc,..}      => loc,
        }
    }
}


#[derive(Clone)]
pub enum Statement {
    Mark{
        lhs:        Expression,
        loc:        Location,
        mark:       String,
    },
    Label{
        loc:        Location,
        label:      String
    },
    Goto{
        loc:        Location,
        label:      String
    },
    Assign {
        loc:        Location,
        lhs:        Expression,
        op:         String,
        rhs:        Expression,
    },
    Expr {
        loc:        Location,
        expr:       Expression,
    },
    Return {
        loc:        Location,
        expr:       Option<Expression>,
    },
    Var {
        loc:        Location,
        typed:      Typed,
        tags:       HashMap<String, Location>,
        name:       String,
        array:      Option<Expression>,
        assign:     Option<Expression>,
    },
    For {
        e1:         Option<Box<Statement>>,
        e2:         Option<Box<Statement>>,
        e3:         Option<Box<Statement>>,
        body:       Block,
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
    pub end:        Location,
    pub statements: Vec<Statement>,
}
