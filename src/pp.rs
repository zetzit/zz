use pest;
use super::parser::{Rule, emit_warn, emit_error};
use super::ast;
use super::name::Name;
use std::path::{Path, PathBuf};
use std::collections::HashMap;

pub struct PP {
    decl:       pest::iterators::Pairs<'static, Rule>,
    n:          PathBuf,
    stack:      Vec<bool>,
    features:   HashMap<String,bool>,
}

#[derive(Debug)]
pub enum Value {
    Bool(bool),
    String(String),
}


impl PP {
    pub fn new(n: &Path, features: HashMap<String,bool>, decl: pest::iterators::Pairs<'static, Rule>) -> PP {
        PP {
            features,
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
                    "feature" => {
                        if args.len() != 1 {
                            emit_error("wrong number of arguments to feature. expected 1", &[
                                   (loc, "called here"),
                            ]);
                            std::process::exit(9);
                        }

                        let s = match &args[0] {
                            Value::String(s) => s,
                            _ => {
                                emit_error("argument to feature must be a string", &[
                                           (loc, "called here"),
                                ]);
                                std::process::exit(9);
                            },
                        };

                        match self.features.get(s) {
                            None => {
                                emit_warn("undefined feature defaults to false", &[
                                          (loc, "avoid this warning by defining it explicitly in zz.toml"),
                                ]);
                                Value::Bool(false)
                            },
                            Some(v) => Value::Bool(*v),
                        }

                    },
                    "def" => {
                        if args.len() != 1 {
                            emit_error("wrong number of arguments to def. expected 1", &[
                                   (loc, "called here"),
                            ]);
                            std::process::exit(9);
                        }

                        match &args[0] {
                            Value::String(s) if s == "debug" => Value::Bool(true),
                            _ => {
                                emit_warn("undefined def defaults to false", &[
                                       (loc, "avoid this warning by defining it explicitly"),
                                ]);
                                Value::Bool(false)
                            }
                        }
                    },
                    "target" => {
                        if args.len() != 2 {
                            emit_error("wrong number of arguments to def. expected 2", &[
                                   (loc, "called here"),
                            ]);
                            std::process::exit(9);
                        }

                        match &args[0] {
                            Value::String(s) if s == "endian" => {
                                match &args[1] {
                                    Value::String(s) if s == "big"          => Value::Bool(false),
                                    Value::String(s) if s == "little"       => Value::Bool(true),
                                    _ => {
                                        emit_warn("invalid attribute value defaults to false", &[
                                                  (loc, "target attribute needs to be a string"),
                                        ]);
                                        Value::Bool(false)
                                    }
                                }
                            },
                            Value::String(s)  => {
                                emit_warn("undefined target attribute defaults to false", &[
                                          (loc, format!("{} is not a known target attribute", s)),
                                ]);
                                Value::Bool(false)
                            },
                            _ => {
                                emit_warn("invalid target attribute defaults to false", &[
                                          (loc, "target attribute needs to be a string"),
                                ]);
                                Value::Bool(false)
                            }
                        }
                    },
                    n => {
                        emit_error(format!("function '{}' not available in preprocessor directive",n),  &[
                               (loc, "used here"),
                        ]);
                        std::process::exit(9);
                    }
                }


            },
            _ => {
                emit_error(format!("{:?} expression cannot (yet) be used in preprocessor directive", expr.as_rule()), &[
                       (loc, "used here"),
                ]);
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
                emit_error("preprocessor directive must evaluate to boolean", &[
                       (loc, format!("this expression = '{:?}'", v)),
                ]);
                std::process::exit(9);
            }

        }
    }

    fn pop(&mut self, loc: &ast::Location) -> bool {
        if self.stack.len() < 1 {
            emit_error("missing preceeding #if directive", &[
                (loc.clone(), "here"),
            ]);
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
                Rule::ppendif => {
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
