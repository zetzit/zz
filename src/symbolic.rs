use crate::flatten;
use crate::ast;
use crate::name::Name;
use std::collections::HashMap;
use super::parser::{emit_error, emit_warn, emit_debug};
use ast::Tags;
use std::io::Write;


pub enum Error {
    Error {
        message:    String,
        details:    Vec<(ast::Location, String)>,
    },
}

impl Error {
    pub fn new(message: String, details:    Vec<(ast::Location, String)>) -> Self {
        Self::Error {
            message,
            details,
        }
    }
}


type Symbol = usize;

#[derive(Clone)]
enum Value{
    Void,
    Uninitialized,
    Untrackable(String),
    Literal(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Void              => write!(f, "void"),
            Value::Uninitialized     => write!(f, "uninitialized"),
            Value::Literal(s)        => write!(f, "literal({})", s),
            Value::Untrackable(s)    => write!(f, "untrackable: {}", s),
        }
    }
}


#[derive(Clone)]
struct Storage {
    name:           Name,
    typed:          ast::Typed,
    declared:       ast::Location,
    value:          Value,
    tags:           ast::Tags,
    temporal:       usize,
}

struct Scope{
    name:   String,
    locals: HashMap<Name, usize>,
}

pub enum Sequence {
    Declare(Symbol),
    Copy {
        lhs:    (Symbol, usize),
        rhs:    (Symbol, usize),
    },
    Literal {
        lhs:    (Symbol, usize),
        val:    String,
    }
}

pub struct Symbolic {
    stack:      Vec<Scope>,
    symbols:    Vec<Storage>,
    ssa:        Vec<Sequence>,
}





impl Symbolic {
    fn execute_module(&mut self, module: &flatten::Module) -> Result<(), Error> {
        for (d,_,_) in &module.d {
            match &d.def {
                ast::Def::Function{args, body, ..} => {
                    self.execute_function(&d.name, args, body)?;
                },
                _ => {
                }
            }
        }
        Ok(())
    }

    fn execute_function(&mut self, name: &String , args: &Vec<ast::NamedArg>, body: &ast::Block) -> Result<(), Error> {
        for stm in &body.statements {
            match stm {
                ast::Statement::Var{loc, typed, tags, name, array, assign} => {
                    let sym = self.declare(Name::from(name), typed.clone(), loc.clone(), tags.clone())?;
                    if let Some(assign) = assign {
                        let sym2 = self.execute_expr(&assign)?;
                        self.copy(sym, sym2);
                    };
                },
                _ => {
                }
            }
        }
        Ok(())
    }

    fn execute_expr(&mut self, expr: &ast::Expression) -> Result<Symbol, Error> {
        match expr {
            ast::Expression::Name(name) => {
                self.name(&name.name, &name.loc)
            },
            ast::Expression::MemberAccess {loc, lhs, rhs, op, ..} => {
                unimplemented!();
            },
            ast::Expression::ArrayAccess {lhs, rhs, ..} => {
                unimplemented!();
            },
            ast::Expression::Literal { loc, v } => {
                let sym = self.declare(
                    Name::from(&format!("literal {}", self.symbols.len())),
                    ast::Typed{
                        name:   Name::from("literal"),
                        ptr:    Vec::new(),
                        loc:    loc.clone(),
                        tail:   ast::Tail::None,
                    },
                    loc.clone(),
                    Tags::new(),
                )?;
                self.symbols[sym].value = Value::Literal(v.clone());
                self.ssa.push(Sequence::Literal{lhs: (sym, 0), val: v.clone()});
                Ok(sym)
            }
            ast::Expression::Call { name, args: callargs, loc: callloc, .. } => {
                unimplemented!();
            }
            ast::Expression::Infix { lhs, rhs, loc, op} => {
                unimplemented!();
            }
            ast::Expression::Cast { expr, .. } => {
                unimplemented!();
            }
            ast::Expression::UnaryPost {expr, op, ..}=> {
                unimplemented!();
            }
            ast::Expression::UnaryPre {expr, op, loc }=> {
                unimplemented!();
            }
            ast::Expression::StructInit {loc, typed, fields} => {
                unimplemented!();
            }
            ast::Expression::ArrayInit {fields, ..} => {
                unimplemented!();
            }
        }
    }


    fn declare(&mut self, name: Name, typed: ast::Typed, loc: ast::Location, tags: ast::Tags) -> Result<Symbol, Error> {
        if let Some(_) = self.cur().locals.get(&name) {
            return Err(Error::new(format!("redeclation of local name '{}'", name), vec![
                (loc.clone(), "this declaration would shadow a previous name".to_string())
            ]));
        }

        let symbol = self.symbols.len();
        self.symbols.push(Storage{
            typed,
            name: name.clone(),
            declared:   loc.clone(),
            value:      Value::Uninitialized,
            tags,
            temporal:   0,
        });
        self.cur().locals.insert(name, symbol);
        self.ssa.push(Sequence::Declare(symbol));
        Ok(symbol)
    }

    fn copy(&mut self, lhs: Symbol, rhs: Symbol) {
        self.symbols[lhs].temporal += 1;
        self.symbols[lhs].value = self.symbols[rhs].value.clone();
        self.ssa.push(Sequence::Copy{
            lhs:    (lhs, self.symbols[lhs].temporal),
            rhs:    (rhs, self.symbols[rhs].temporal),
        });
    }

    fn name(&mut self, name: &Name, used_here: &ast::Location) -> Result<Symbol, Error> {
        for scope in self.stack.iter().rev() {
            if let Some(v) = scope.locals.get(name) {
                return Ok(*v);
            }
        }
        return Err(Error::new(format!("undefined name '{}'", name), vec![
            (used_here.clone(), format!("'{}' is not defined in this scope", name))
        ]));
    }

    fn new() -> Self {
        Symbolic {
            stack:  vec![
                Scope {
                    name: "global".to_string(),
                    locals: Default::default(),
                }
            ],
            symbols: Default::default(),
            ssa:    Vec::new(),
        }
    }

    fn push(&mut self, name: String) {
        debug!("  scope {}", name);
        self.stack.push(Scope{
            name,
            locals: HashMap::new(),
        });
    }

    fn cur(&mut self) -> &mut Scope {
        self.stack.last_mut().unwrap()
    }

}


pub fn execute(module: &flatten::Module) -> Symbolic{
    let mut sym = Symbolic::new();
    match sym.execute_module(module) {
        Err(Error::Error{message, details}) => {
            emit_error(message, &details);
            std::process::exit(9);
        },
        Ok(()) => {
        }
    }


    let outdir = format!("target/ssa/");
    std::fs::remove_dir_all(&outdir).ok();
    std::fs::create_dir_all(&outdir).unwrap();

    let mut f = std::fs::File::create(format!("{}/{}.stm2", outdir, module.name)).unwrap();
    write!(f, "(set-logic QF_BVFP)\n").unwrap();
    write!(f, "(set-option :produce-models true)\n").unwrap();

    for s in &sym.ssa {
        match s {
            Sequence::Declare(s) => {
                write!(f, ";{}\n(declare-fun local{} (Int) (_ BitVec 32))\n", sym.symbols[*s].name, s).unwrap();
            }
            Sequence::Copy {lhs, rhs} => {
                write!(f, "(assert (= (local{} {}) (local{} {})))\n", lhs.0, lhs.1, rhs.0, rhs.1).unwrap();
            }
            Sequence::Literal{lhs, val} => {
                write!(f, "(assert (= (local{} {}) (_ bv{} 32)))\n", lhs.0, lhs.1, val).unwrap();
            }
        }
    }
    write!(f, "(check-sat)\n").unwrap();
    write!(f, "(get-model)\n").unwrap();
    write!(f, "(exit)\n").unwrap();

    sym
}
