use super::name::Name;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::path::PathBuf;


#[serde(rename_all = "snake_case")]
#[derive(PartialEq, Clone, Debug, Serialize, Deserialize, Default)]
pub struct Location {
    pub file: String,
    pub line: usize,
    pub start: usize,
    pub end: usize,
}

impl Location {
    pub fn from_span(file: String, span: &pest::Span) -> Self {
        Self {
            file,
            line: span.start_pos().line_col().0,
            start: span.start(),
            end: span.end(),
        }
    }

    pub fn to_span(&self) -> pest::Span<'static> {
        let (file, _) = read_source(self.file.clone());
        pest::Span::new(file, self.start, self.end).unwrap()
    }

    pub fn builtin() -> Self {
        Self {
            file: "".to_string(),
            line: 1,
            start: 0,
            end: 0,
        }
    }
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.file, self.line)
    }
}

#[serde(rename_all = "snake_case")]
#[derive(Default, Clone, Debug, Serialize, Deserialize)]
pub struct Tags(pub HashMap<String, HashMap<String, Location>>);

#[serde(rename_all = "snake_case")]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Storage {
    Static,
    ThreadLocal,
    Atomic,
}

#[serde(rename_all = "snake_case")]
#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub enum Visibility {
    Shared,
    Object,
    Export,
}

#[serde(rename_all = "snake_case")]
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Import {
    pub name: Name,
    pub alias: Option<String>,
    pub local: Vec<(String, Option<String>)>,
    pub vis: Visibility,
    pub loc: Location,
    pub inline: bool,
    pub needs: Vec<(Typed, Location)>,
}

#[serde(rename_all = "snake_case")]
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Tail {
    None,
    Dynamic(Option<Box<Typed>> /*final*/),
    Static(u64, Location),
    Bind(String, Location),
}

impl Default for Tail {
    fn default() -> Self {
        Tail::None
    }
}

#[serde(rename_all = "snake_case")]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Derive {
    pub loc: Location,
    pub makro: String,
    pub args: Vec<Box<Expression>>,
}

#[serde(rename_all = "snake_case")]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Array {
    None,
    Unsized,
    Sized(Expression),
}

#[serde(rename_all = "snake_case")]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Def {
    Static {
        tags: Tags,
        typed: Typed,
        expr: Expression,
        storage: Storage,
        array: Array,
    },
    Const {
        typed: Typed,
        expr: Expression,
    },
    Function {
        nameloc: Location,
        ret: Option<AnonArg>,
        args: Vec<NamedArg>,
        derives: Vec<Derive>,
        attr: HashMap<String, Location>,
        body:       ConditionalBlock,
        vararg: bool,
        callassert: Vec<Expression>,
        calleffect: Vec<Expression>,

        // never checked, only asserted into smt
        callattests: Vec<Expression>,

        argsexpanded: bool,
    },
    Theory {
        ret:    Option<AnonArg>,
        args:   Vec<NamedArg>,
        attr:   HashMap<String, Location>,
        body:   Option<Expression>,
    },
    Closure {
        nameloc: Location,
        ret: Option<AnonArg>,
        args: Vec<NamedArg>,
        attr: HashMap<String, Location>,
    },
    Struct {
        fields: Vec<Field>,
        derives: Vec<Derive>,
        packed: bool,
        tail: Tail,
        union: bool,
        impls: HashMap<String, (Name, Location)>,
    },
    Symbol {},
    Enum {
        names: Vec<(String, Option<u64>)>,
        derives: Vec<Derive>,
    },
    Macro {
        args: Vec<String>,
        body: Block,
    },
    Flags {
        body: ConditionalBlock,
    },
    Testcase {
        fields: Vec<(String, Expression)>,
    },
    Include {
        expr: String,
        loc: Location,
        fqn: Name,
        inline: bool,
        needs: Vec<(Typed, Location)>,
    },
    Type {
        alias:      Typed,
        derives:    Vec<Derive>,
    }
}

#[serde(rename_all = "snake_case")]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Local {
    pub name: String,
    pub vis: Visibility,
    pub loc: Location,
    pub def: Def,
    pub doc: String,
}

#[serde(rename_all = "snake_case")]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Pointer {
    pub loc: Location,
    pub tags: Tags,
}

#[serde(rename_all = "snake_case")]
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Type {
    Void,
    New,
    Elided,

    // usigned int of x bytes
    U8,
    U16,
    U32,
    U64,
    U128,

    // signed int of x bytes
    Char,
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

    Typeid,

    Other(Name),
}

impl Type {
    pub fn signed(&self) -> bool {
        match self {
            Type::Elided
            | Type::New
            | Type::U8
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
            | Type::Typeid
            | Type::Void
            | Type::Other(_) => false,

            Type::Char
            | Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::I128
            | Type::Int
            | Type::ISize
            | Type::ILiteral => true,
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Type::Elided
    }
}

#[serde(rename_all = "snake_case")]
#[derive(Clone, Debug, Serialize, Deserialize, Default)]
pub struct Typed {
    pub t:      Type,
    pub loc:    Location,
    pub ptr:    Vec<Pointer>,
    pub tail:   Tail,
    pub params: Vec<Expression>
}

impl PartialEq for Typed {
    fn eq(&self, other: &Self) -> bool {
        self.t == other.t && self.ptr.len() == other.ptr.len() && self.tail == other.tail
    }
}
impl std::fmt::Display for Typed {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.t {
            Type::Void => write!(f, "void"),
            Type::Char => write!(f, "char"),
            Type::New => write!(f, "new"),
            Type::Elided => write!(f, "elided"),
            Type::U8 => write!(f, "u8"),
            Type::U16 => write!(f, "u16"),
            Type::U32 => write!(f, "u32"),
            Type::U64 => write!(f, "u64"),
            Type::U128 => write!(f, "u128"),
            Type::I8 => write!(f, "i8"),
            Type::I16 => write!(f, "i16"),
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),
            Type::I128 => write!(f, "i128"),
            Type::Int => write!(f, "int"),
            Type::UInt => write!(f, "uint"),
            Type::ISize => write!(f, "isize"),
            Type::USize => write!(f, "usize"),
            Type::Bool => write!(f, "bool"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
            Type::ILiteral => write!(f, "iliteral"),
            Type::ULiteral => write!(f, "uliteral"),
            Type::Typeid  => write!(f, "type"),
            Type::Other(name) => write!(f, "{}", name),
        }?;

        for _ in &self.ptr {
            write!(f, "*")?;
        }
        match &self.tail {
            Tail::None => (),
            Tail::Dynamic(_) => write!(f, "+")?,
            Tail::Static(v, _) => write!(f, "+{}", v)?,
            Tail::Bind(v, _) => write!(f, "+{}", v)?,
        }
        Ok(())
    }
}

#[serde(rename_all = "snake_case")]
#[derive(Default, Clone, Serialize, Deserialize)]
pub struct Module {
    pub name: Name,
    pub source: PathBuf,
    pub locals: Vec<Local>,
    pub imports: Vec<Import>,
    pub sources: HashSet<PathBuf>,
}

#[serde(rename_all = "snake_case")]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AnonArg {
    pub typed: Typed,
}

#[serde(rename_all = "snake_case")]
#[derive(Clone, Debug, Serialize, Deserialize, Default)]
pub struct NamedArg {
    pub typed:      Typed,
    pub name:       String,
    pub tags:       Tags,
    pub loc:        Location,
    pub assign:     Option<Expression>,
}

#[serde(rename_all = "snake_case")]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Field {
    pub typed: Typed,
    pub name: String,
    pub array: Array,
    pub tags: Tags,
    pub loc: Location,
}

#[serde(rename_all = "snake_case")]
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
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
            | InfixOperator::Morethan => true,

            InfixOperator::Add
            | InfixOperator::Subtract
            | InfixOperator::Multiply
            | InfixOperator::Divide
            | InfixOperator::Bitxor
            | InfixOperator::Shiftleft
            | InfixOperator::Shiftright
            | InfixOperator::Modulo
            | InfixOperator::Bitand
            | InfixOperator::Bitor => false,
        }
    }
    pub fn takes_boolean(&self) -> bool {
        match self {
            InfixOperator::Equals
            | InfixOperator::Nequals
            | InfixOperator::Booland
            | InfixOperator::Boolor => true,

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
            | InfixOperator::Morethan => false,
        }
    }
    pub fn takes_integer(&self) -> bool {
        match self {
            InfixOperator::Booland | InfixOperator::Boolor => false,

            InfixOperator::Equals
            | InfixOperator::Nequals
            | InfixOperator::Add
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
            | InfixOperator::Morethan => true,
        }
    }
}

#[serde(rename_all = "snake_case")]
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum PrefixOperator {
    Boolnot,
    Bitnot,
    Increment,
    Decrement,
    AddressOf,
    Deref,
}

#[serde(rename_all = "snake_case")]
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum PostfixOperator {
    Increment,
    Decrement,
}

#[serde(rename_all = "snake_case")]
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum AssignOperator {
    Bitor,
    Bitand,
    Add,
    Sub,
    Eq,
}

#[serde(rename_all = "snake_case")]
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum EmitBehaviour {
    Default,
    Skip,
    Error { loc: Location, message: String },
}

#[serde(rename_all = "snake_case")]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Expression {
    Name(Typed),
    MemberAccess {
        loc: Location,
        lhs: Box<Expression>,
        op: String,
        rhs: String,
    },
    ArrayAccess {
        loc: Location,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    LiteralString {
        loc: Location,
        v:  Vec<u8>,
    },
    LiteralChar {
        loc: Location,
        v: u8,
    },
    Literal {
        loc: Location,
        v: String,
    },
    Call {
        loc: Location,
        name: Box<Expression>,
        args: Vec<Box<Expression>>,
        expanded: bool,
        emit: EmitBehaviour,
    },
    Infix {
        loc: Location,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        op: InfixOperator,
    },
    Cast {
        loc: Location,
        into: Typed,
        expr: Box<Expression>,
    },
    UnaryPost {
        loc: Location,
        op: PostfixOperator,
        expr: Box<Expression>,
    },
    UnaryPre {
        loc: Location,
        op: PrefixOperator,
        expr: Box<Expression>,
    },
    StructInit {
        loc: Location,
        typed: Typed,
        fields: Vec<(String, Box<Expression>)>,
    },
    ArrayInit {
        loc: Location,
        fields: Vec<Box<Expression>>,
    },
    MacroCall {
        loc: Location,
        name: Name,
        args: Vec<Box<Expression>>,
    },
    Unsafe {
        loc: Location,
        into: Typed,
        expr: Box<Expression>,
    },
    Cpp {
        loc:    Location,
        expr:   Box<Expression>,
    },
}

impl Expression {
    pub fn loc(&self) -> &Location {
        match self {
            Expression::Name(name) => &name.loc,
            Expression::MemberAccess { loc, .. } => loc,
            Expression::ArrayAccess { loc, .. } => loc,
            Expression::Literal { loc, .. } => loc,
            Expression::LiteralString { loc, .. } => loc,
            Expression::LiteralChar { loc, .. } => loc,
            Expression::Call { loc, .. } => loc,
            Expression::Infix { loc, .. } => loc,
            Expression::Cast { loc, .. } => loc,
            Expression::UnaryPost { loc, .. } => loc,
            Expression::UnaryPre { loc, .. } => loc,
            Expression::StructInit { loc, .. } => loc,
            Expression::ArrayInit { loc, .. } => loc,
            Expression::MacroCall { loc, .. } => loc,
            Expression::Unsafe { loc, .. } => loc,
            Expression::Cpp { loc, .. } => loc,
        }
    }
}

#[serde(rename_all = "snake_case")]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Statement {
    Mark {
        lhs: Expression,
        loc: Location,
        key: String,
        value: String,
    },
    Label {
        loc: Location,
        label: String,
    },
    Assign {
        loc: Location,
        lhs: Expression,
        op: AssignOperator,
        rhs: Expression,
    },
    Expr {
        loc: Location,
        expr: Expression,
    },
    Switch {
        loc: Location,
        expr: Expression,
        cases: Vec<(Vec<Expression>, Block)>,
        default: Option<Block>,
    },
    Continue {
        loc: Location,
    },
    Break {
        loc:    Location,
        label:  Option<String>,
    },
    Return {
        loc: Location,
        expr: Option<Expression>,
    },
    Var {
        loc: Location,
        typed: Typed,
        tags: Tags,
        name: String,
        array: Option<Option<Expression>>,
        assign: Option<Expression>,
    },
    While {
        expr: Expression,
        body: Block,
    },
    For {
        label:  Option<String>,
        e1:     Vec<Box<Statement>>,
        e2:     Option<Expression>,
        e3:     Vec<Box<Statement>>,
        body:   Block,
    },
    If {
        branches: Vec<(Location, Option<Expression>, Block)>,
    },
    Block(Box<Block>),
    Unsafe(Box<Block>),
    CBlock {
        loc: Location,
        lit: String,
    },
    MacroCall {
        loc: Location,
        name: Name,
        args: Vec<Box<Expression>>,
    },
}

#[serde(rename_all = "snake_case")]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ConditionalBlock {
    pub branches: Vec<(Location, Option<Expression>, Block)>,
}

#[serde(rename_all = "snake_case")]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Block {
    pub end: Location,
    pub statements: Vec<Box<Statement>>,
    pub expanded: bool,
}

impl Tags {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, k: &str) -> Option<&HashMap<String, Location>> {
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
    pub fn insert(&mut self, key: String, value: String, loc: Location) {
        self.0
            .entry(key)
            .or_insert(HashMap::new())
            .insert(value, loc);
    }
    pub fn contains_key(&self, s: &str) -> bool {
        self.0.contains_key(s)
    }
    pub fn contains(&self, s: &str) -> bool {
        self.0.contains_key(s)
    }
}

use lazy_static::lazy_static;
use sha2::{Digest, Sha256};
use std::sync::{Arc, Mutex};

lazy_static! {
    static ref SOURCES: Arc<Mutex<HashMap<String, (&'static str, String)>>> =
        Arc::new(Mutex::new(HashMap::new()));
}

pub fn read_source(path: String) -> (&'static str, String /*sha*/) {
    if path == "" {
        return ("<builtin>", "".to_string());
    }
    if let Some(v) = SOURCES.lock().unwrap().get(&path) {
        return v.clone();
    }

    let s = Box::leak(Box::new(
        std::fs::read_to_string(&path).expect(&format!("read {:?}", path)),
    ));

    let mut hasher = Sha256::new();
    hasher.input(s.as_bytes());
    let hash = format!("{:x}", hasher.result());

    SOURCES.lock().unwrap().insert(path.clone(), (s, hash));

    SOURCES.lock().unwrap().get(&path).unwrap().clone()
}

pub fn generated_source(from: &str, source: String) -> (String, &'static str) {
    let mut hasher = Sha256::new();
    hasher.input(source.as_bytes());
    let hash = format!("{:x}", hasher.result());
    let path = format!("generated<{}> from {}", hash, from);

    SOURCES
        .lock()
        .unwrap()
        .insert(path.clone(), (Box::leak(Box::new(source)), hash));
    return (path.clone(), SOURCES.lock().unwrap().get(&path).unwrap().0);
}
