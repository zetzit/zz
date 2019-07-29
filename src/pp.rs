use pest;
use super::parser::Rule;
use super::ast;
use super::name::Name;
use std::path::{Path, PathBuf};
use super::parser;

pub struct PP {
    decl:   pest::iterators::Pairs<'static, Rule>,
    n:      PathBuf,
    stack:  Vec<bool>,
}

#[derive(Debug)]
pub enum Value {
    Bool(bool),
    String(String),
}


impl PP {
    pub fn new(n: &Path, decl: pest::iterators::Pairs<'static, Rule>) -> PP {
        PP {
            decl,
            n: n.into(),
            stack: Vec::new(),
        }
    }

    pub fn eval(&self, termish: pest::iterators::Pair<'static, Rule>)  -> Value {
        let loc = ast::Location{
            file: self.n.to_string_lossy().into(),
            span: termish.as_span(),
        };
        let expr = termish.into_inner().next().unwrap();
        match expr.as_rule() {
            Rule::number_literal => {
                if expr.as_str() == "0" {
                    Value::Bool(false)
                } else {
                    Value::Bool(true)
                }
            },
            Rule::string_literal => {
                let mut s = expr.as_str().to_string();
                s.remove(0);
                s.remove(s.len() -1);
                Value::String(s)

            },
            Rule::call => {
                let mut expr = expr.into_inner();
                let name = Name::from(expr.next().unwrap().as_str());
                let args = match expr.next() {
                    Some(args) => {
                        args.into_inner().into_iter().map(|arg|{
                            self.eval(arg)
                        }).collect()
                    },
                    None => {
                        Vec::new()
                    }
                };

                match name.0.join("::").as_str() {
                    "def" => {
                        if args.len() != 1 {
                            error!("wrong number of arguments to cfg. expected 1 \n{}", 
                                   parser::make_error(&loc, "called here"),
                                   );
                            std::process::exit(9);
                        }

                        match &args[0] {
                            Value::String(s) if s == "debug" => Value::Bool(true),
                            _ => {
                                warn!("undefined def defaults to false\n{}",
                                       parser::make_error(&loc, "avoid this warning by defining it explicitly"),
                                );
                                Value::Bool(false)
                            }
                        }
                    },
                    n => {
                        error!("function '{}' not available in preprocessor directive \n{}", 
                               n,
                               parser::make_error(&loc, "used here"),
                        );
                        std::process::exit(9);
                    }
                }


            },
            _ => {
                error!("{:?} expression cannot (yet) be used in preprocessor directive \n{}",
                       expr.as_rule(),
                       parser::make_error(&loc, "used here"),
                       );
                std::process::exit(9);
            }
        }
    }

    fn push(&mut self, loc: ast::Location, v: Value) {
        match v {
            Value::Bool(false) => {
                self.stack.push(false);
            }
            Value::Bool(true) => {
                self.stack.push(true);
            },
            _ => {
                error!("preprocessor directive must evaluate to boolean\n{}",
                       parser::make_error(&loc, format!("this expression = '{:?}'", v)),
                       );
                std::process::exit(9);
            }

        }
    }

    fn pop(&mut self, loc: &ast::Location) -> bool {
        if self.stack.len() < 1 {
            error!("missing preceeding #if directive\n{}",
                   parser::make_error(&loc, "here"),
                   );
            std::process::exit(9);
        }
        self.stack.pop().unwrap()
    }
}

impl Iterator for PP {
    type Item = pest::iterators::Pair<'static, Rule>;
    fn next(&mut self) -> Option<Self::Item> {
        let decl = match self.decl.next() {
            None => return None,
            Some(v) => v,
        };

        if let Rule::pp = decl.as_rule() {
            let decl = decl.into_inner().next().unwrap();
            let loc = ast::Location{
                file: self.n.to_string_lossy().into(),
                span: decl.as_span(),
            };
            match decl.as_rule() {
                Rule::ppelif  => {
                    let previous = self.pop(&loc);
                    if previous {
                        self.push(loc, Value::Bool(previous));
                    } else {
                        let expr = decl.into_inner().next().unwrap();
                        let loc = ast::Location{
                            file: self.n.to_string_lossy().into(),
                            span: expr.as_span(),
                        };
                        self.push(loc, self.eval(expr));
                    }
                },
                Rule::ppif => {
                    let expr = decl.into_inner().next().unwrap();
                    let loc = ast::Location{
                        file: self.n.to_string_lossy().into(),
                        span: expr.as_span(),
                    };
                    self.push(loc, self.eval(expr));
                },
                Rule::ppelse => {
                    let invert = Value::Bool(!self.pop(&loc));
                    self.push(loc, invert);
                },
                Rule::ppendif=> {
                    self.pop(&loc);
                },
                _ => panic!("unexpected rule {:?} in preprocessor", decl.as_rule()),
            }
            return self.next();
        }

        if let Some(false) = self.stack.last() {
            return self.next();
        } else {
            return Some(decl);
        }
    }
}
