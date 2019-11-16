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
    pub fn builtin() -> Self {
        Self {
            file: "prelude".to_string(),
            span: pest::Span::new(" ",0,1).unwrap(),
        }
    }
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.file, self.line())
    }
}

#[derive(Default, Clone, Debug)]
pub struct Tags(pub HashMap<String, HashMap<String, Location>>);



#[derive(Clone, Debug)]
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

#[derive(Clone, Debug, PartialEq)]
pub enum Tail {
    None,
    Dynamic,
    Static(u64, Location),
    Bind(String, Location),
}


#[derive(Clone, Debug)]
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
        callassert: Vec<Expression>,
        calleffect: Vec<Expression>,
    },
    Theory {
        ret:        Option<AnonArg>,
        args:       Vec<NamedArg>,
        attr:       Vec<(String, Location)>,
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
        tail:       Tail,
    },
    Enum {
        names:      Vec<(String, Option<u64>)>,
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


#[derive(Clone, Debug, PartialEq)]
pub enum Type {

    // usigned int of x bytes
    U8,
    U16,
    U32,
    U64,
    U128,

    // signed int of x bytes
    I8,
    I16,
    I32,
    I64,
    I128,

    // int/uint are c directly emited as c typed. they're compiler specific
    Int,
    UInt,

    // size of a pointer
    ISize,
    USize,

    // may be just emitted as int
    Bool,

    // IEEE floating point.
    F32,
    F64,

    // untyped literal int,
    ULiteral,
    ILiteral,

    Other(Name),
}

impl Type {
    pub fn signed(&self) -> bool {
        match self {
            Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::U128
            | Type::UInt
            | Type::USize
            | Type::Bool
            | Type::F32
            | Type::F64
            | Type::ULiteral
            | Type::Other(_)
                => false,

            Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::I128
            | Type::Int
            | Type::ISize
            | Type::ILiteral
                => true,
        }
    }
}


#[derive(Clone, Debug)]
pub struct Typed {
    pub t:      Type,
    pub loc:    Location,
    pub ptr:    Vec<Pointer>,
    pub tail:   Tail,
}

impl PartialEq for Typed{
    fn eq(&self, other: &Self) -> bool {
        self.t == other.t
        && self.ptr.len() == other.ptr.len()
        && self.tail == other.tail
    }
}
impl std::fmt::Display for Typed{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.t {
            Type::U8            => write!(f, "u8"),
            Type::U16           => write!(f, "u16"),
            Type::U32           => write!(f, "u32"),
            Type::U64           => write!(f, "u64"),
            Type::U128          => write!(f, "u128"),
            Type::I8            => write!(f, "i8"),
            Type::I16           => write!(f, "i16"),
            Type::I32           => write!(f, "i32"),
            Type::I64           => write!(f, "i64"),
            Type::I128          => write!(f, "i128"),
            Type::Int           => write!(f, "int"),
            Type::UInt          => write!(f, "uint"),
            Type::ISize         => write!(f, "isize"),
            Type::USize         => write!(f, "usize"),
            Type::Bool          => write!(f, "bool"),
            Type::F32           => write!(f, "f32"),
            Type::F64           => write!(f, "f64"),
            Type::ILiteral      => write!(f, "iliteral"),
            Type::ULiteral      => write!(f, "uliteral"),
            Type::Other(name)   => write!(f, "{}", name),
        }?;

        for _ in &self.ptr {
            write!(f, "*")?;
        }
        match &self.tail {
            Tail::None          => (),
            Tail::Dynamic       => write!(f, "+")?,
            Tail::Static(v, _)  => write!(f, "+{}", v)?,
            Tail::Bind(v,_)     => write!(f, "+{}", v)?,
        }
        Ok(())
    }
}

#[derive(Default, Clone)]
pub struct Module {
    pub name:       Name,
    pub source:     PathBuf,
    pub locals:     Vec<Local>,
    pub imports:    Vec<Import>,
    pub sources:    HashSet<PathBuf>,
}

#[derive(Clone, Debug)]
pub struct AnonArg {
    pub typed:    Typed,
}

#[derive(Clone, Debug)]
pub struct NamedArg {
    pub typed:      Typed,
    pub name:       String,
    pub tags:       Tags,
    pub loc:        Location,
}


#[derive(Clone, Debug)]
pub struct Field {
    pub typed:      Typed,
    pub name:       String,
    pub array:      Option<Option<Expression>>,
    pub tags:       Tags,
    pub loc:        Location,
}

#[derive(Clone, Debug, PartialEq)]
pub enum InfixOperator {
    Equals,
    Nequals,
    Add,
    Subtract,
    Multiply,
    Divide,
    Bitxor,
    Booland,
    Boolor,
    Moreeq,
    Lesseq,
    Lessthan,
    Morethan,
    Shiftleft,
    Shiftright,
    Modulo,
    Bitand,
    Bitor,
}


impl InfixOperator {
    pub fn returns_boolean(&self) -> bool {
        match self {
            InfixOperator::Equals
            | InfixOperator::Nequals
            | InfixOperator::Booland
            | InfixOperator::Boolor
            | InfixOperator::Moreeq
            | InfixOperator::Lesseq
            | InfixOperator::Lessthan
            | InfixOperator::Morethan
            => true,

            InfixOperator::Add
            | InfixOperator::Subtract
            | InfixOperator::Multiply
            | InfixOperator::Divide
            | InfixOperator::Bitxor
            | InfixOperator::Shiftleft
            | InfixOperator::Shiftright
            | InfixOperator::Modulo
            | InfixOperator::Bitand
            | InfixOperator::Bitor
            => false,
        }
    }
    pub fn takes_boolean(&self) -> bool {
        match self {
            InfixOperator::Equals
            | InfixOperator::Nequals
            | InfixOperator::Booland
            | InfixOperator::Boolor
            => true,

            InfixOperator::Add
            | InfixOperator::Subtract
            | InfixOperator::Multiply
            | InfixOperator::Divide
            | InfixOperator::Bitxor
            | InfixOperator::Shiftleft
            | InfixOperator::Shiftright
            | InfixOperator::Modulo
            | InfixOperator::Bitand
            | InfixOperator::Bitor
            | InfixOperator::Moreeq
            | InfixOperator::Lesseq
            | InfixOperator::Lessthan
            | InfixOperator::Morethan
            => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum PrefixOperator {
    Boolnot,
    Bitnot,
    Increment,
    Decrement,
    AddressOf,
    Deref,
}

#[derive(Clone, Debug, PartialEq)]
pub enum PostfixOperator {
    Increment,
    Decrement,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AssignOperator {
    Bitor,
    Bitand,
    Add,
    Sub,
    Eq,
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
    LiteralString {
        loc:    Location,
        v:      Vec<u8>,
    },
    LiteralChar {
        loc:    Location,
        v:      u8,
    },
    Literal{
        loc:    Location,
        v:      String,
    },
    Call {
        loc:        Location,
        name:       Box<Expression>,
        args:       Vec<Box<Expression>>,
        expanded:   bool,
    },
    Infix {
        loc:    Location,
        lhs:    Box<Expression>,
        rhs:    Box<Expression>,
        op:     InfixOperator,
    },
    Cast {
        loc:    Location,
        into:   Typed,
        expr:   Box<Expression>,
    },
    UnaryPost {
        loc:    Location,
        op:     PostfixOperator,
        expr:   Box<Expression>,
    },
    UnaryPre {
        loc:    Location,
        op:     PrefixOperator,
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
    },
    StaticError {
        loc:        Location,
        message:    String,
    }
}

impl Expression {
    pub fn loc(&self) -> &Location {
        match self {
            Expression::Name(name)              => &name.loc,
            Expression::MemberAccess {loc,..}   => loc,
            Expression::ArrayAccess {loc,..}    => loc,
            Expression::Literal{loc,..}         => loc,
            Expression::LiteralString{loc,..}   => loc,
            Expression::LiteralChar{loc,..}     => loc,
            Expression::Call {loc,..}           => loc,
            Expression::Infix{loc,..}           => loc,
            Expression::Cast {loc,..}           => loc,
            Expression::UnaryPost {loc,..}      => loc,
            Expression::UnaryPre {loc,..}       => loc,
            Expression::StructInit {loc,..}     => loc,
            Expression::ArrayInit {loc,..}      => loc,
            Expression::StaticError{loc,..}     => loc,
        }
    }
}


#[derive(Clone, Debug)]
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
        op:         AssignOperator,
        rhs:        Expression,
    },
    Expr {
        loc:        Location,
        expr:       Expression,
    },
    Switch {
        loc:        Location,
        expr:       Expression,
        cases:      Vec<(Vec<Expression>, Block)>,
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
    While {
        expr:       Expression,
        body:       Block,
    },
    For {
        e1:         Vec<Box<Statement>>,
        e2:         Option<Expression>,
        e3:         Vec<Box<Statement>>,
        body:       Block,
    },
    If {
        branches:   Vec<(Option<Expression>, Block)>,
    },
    Block(Box<Block>),
    Unsafe(Box<Block>),
    CBlock{
        loc:        Location,
        lit:        String,
    }
}

#[derive(Clone, Debug)]
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

