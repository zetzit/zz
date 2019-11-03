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

#[derive(Default, Clone, Debug)]
pub struct Tags(pub HashMap<String, HashMap<String, Location>>);



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
    pub alias:  Option<String>,
    pub local:  Vec<(String, Option<String>)>,
    pub vis:    Visibility,
    pub loc:    Location,
    pub inline: bool,
}


#[derive(Clone)]
pub enum Def {
    Static {
        tags:       Tags,
        typed:      Typed,
        expr:       Expression,
        storage:    Storage,
        array:      Option<Option<Expression>>,
    },
    Const {
        typed:      Typed,
        expr:       Expression,
    },
    Function {
        ret:        Option<AnonArg>,
        args:       Vec<NamedArg>,
        attr:       Vec<(String, Location)>,
        body:       Block,
        vararg:     bool,
    },
    Fntype {
        ret:        Option<AnonArg>,
        args:       Vec<NamedArg>,
        attr:       Vec<(String, Location)>,
        vararg:     bool,
    },
    Struct {
        fields:     Vec<Field>,
        packed:     bool,
    },
    Enum {
        names:      Vec<(String, Option<i64>)>,
    },
    Macro {
        args:       Vec<String>,
        body:       Block,
    },
    Testcase {
        fields:     Vec<(String, Expression)>,
    },
}


#[derive(Clone)]
pub struct Local {
    pub name:       String,
    pub vis:        Visibility,
    pub loc:        Location,
    pub def:        Def,
}


#[derive(Clone)]
pub struct Include {
    pub expr:   String,
    pub loc:    Location,
    pub fqn:    Name,
    pub inline: bool,
}

#[derive(Clone, Debug)]
pub struct Pointer {
    pub loc:    Location,
    pub tags:   Tags,
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
    pub tags:       Tags,
    pub loc:        Location,
}


#[derive(Clone)]
pub struct Field {
    pub typed:      Typed,
    pub name:       String,
    pub array:      Option<Expression>,
    pub tags:       Tags,
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
        name:   Box<Expression>,
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
        key:        String,
        value:      String,
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
    Switch {
        loc:        Location,
        expr:       Expression,
        cases:      Vec<(Expression, Block)>,
        default:    Option<Block>,
    },
    Continue{
        loc:        Location,
    },
    Break {
        loc:        Location,
    },
    Return {
        loc:        Location,
        expr:       Option<Expression>,
    },
    Var {
        loc:        Location,
        typed:      Typed,
        tags:       Tags,
        name:       String,
        array:      Option<Option<Expression>>,
        assign:     Option<Expression>,
    },
    For {
        e1:         Vec<Box<Statement>>,
        e2:         Vec<Box<Statement>>,
        e3:         Vec<Box<Statement>>,
        body:       Block,
    },
    Cond {
        op:         String,
        expr:       Option<Expression>,
        body:       Block,
    },
    Block(Box<Block>),
    Unsafe(Box<Block>),
    CBlock{
        loc:        Location,
        lit:        String,
    }
}

#[derive(Clone)]
pub struct Block {
    pub end:        Location,
    pub statements: Vec<Statement>,
}



impl Tags {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, k:&str) -> Option<&HashMap<String,Location>> {
        self.0.get(k)
    }

    pub fn remove(&mut self, key: &str, value: Option<&str>) {
        if let Some(mut r) = self.0.remove(key) {
            if let Some(value) = value {
                r.remove(value);
                if r.len() > 0 {
                    self.0.insert(key.to_string(), r);
                }
            }
        }
    }
    pub fn insert(&mut self, key: String , value: String, loc: Location) {
        self.0.entry(key).or_insert(HashMap::new()).insert(value,loc);
    }
    pub fn contains_key(&self, s: &str) -> bool {
        self.0.contains_key(s)
    }
    pub fn contains(&self, s: &str) -> bool {
        self.0.contains_key(s)
    }
}

