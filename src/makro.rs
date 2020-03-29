use super::Name;
use super::Error;
use crate::ast;
use std::collections::HashMap;
use std::process::{Command, Stdio};
use super::parser;
use pest::Parser;
use std::io::Write;
use std::io::Read;

use serde::{Serialize};

#[derive(Serialize)]
struct MacroStdin {
    args: Vec<Box<ast::Expression>>,
}

pub fn expr(name: &Name, loc: &ast::Location, args: &Vec<Box<ast::Expression>>) -> Result<ast::Expression, Error>
{
    let mp = format!("target/macro/{}", name.0[1..].join("_"));
    let mp = std::path::Path::new(&mp);
    if !mp.exists() {
        return Err(Error::new(format!("macro {} is unavailable", name), vec![
            (loc.clone(), "macro not available here. it may be compiled later".to_string())
        ]));
    }

    let input = MacroStdin {
        args: args.clone(),
    };

    let mut cmd = Command::new(mp)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect(&format!("failed to execute macro {}", name));

    let stdin = std::mem::replace(&mut cmd.stdin, None).unwrap();
    serde_json::ser::to_writer(stdin, &input).unwrap();

    if !cmd.wait().unwrap().success() {
        eprintln!("failed to execute macro {}", name);
        std::process::exit(9);
    }

    let mut n = String::new();
    cmd.stdout.as_mut().unwrap().read_to_string(&mut n).unwrap();
    let n = Box::leak(Box::new(n));

    let mut pp = match parser::ZZParser::parse(parser::Rule::expr, n ) {
        Ok(v) => v,
        Err(e) => {
            return Err(Error::new(format!("syntax error in proc macro return: {}", e), vec![
                (loc.clone(), "in this macro invocation".to_string())
            ]));
        }
    };

    let pp = match pp.next() {
        None => return Ok(ast::Expression::LiteralString{
            loc: ast::Location::builtin(),
            v: "".into(),
        }),
        Some(v) => v,
    };

    let expr = parser::parse_expr((n, &mp), pp);
    Ok(expr)
}


pub fn sieve(md: &ast::Module) -> Vec<ast::Module>
{
    let mut newmods = Vec::new();
    for local in &md.locals {
        match &local.def {
            ast::Def::Macro{args, body} => {
                let mut body = body.clone();
                body.statements.push(Box::new(ast::Statement::Return{
                    loc: ast::Location::builtin(),
                    expr: Some(ast::Expression::Literal{
                        loc: ast::Location::builtin(),
                        v: "0".to_string(),
                    })}));
                let mut nl = local.clone();
                nl.def = ast::Def::Function{
                    nameloc:     local.loc.clone(),
                    ret:         Some(ast::AnonArg{typed:ast::Typed{
                        t:      ast::Type::Int,
                        loc:    local.loc.clone(),
                        ptr:    Vec::new(),
                        tail:   ast::Tail::None,
                    }}),
                    args:        Vec::new(),
                    derives:     Vec::new(),
                    attr:        HashMap::new(),
                    body:        body,
                    vararg:      false,
                    callassert:  Vec::new(),
                    calleffect:  Vec::new(),
                    callattests: Vec::new(),
                };
                nl.name = "main".to_string();

                let mut numod = md.clone();
                numod.name = Name::from(&format!("{}::{}", numod.name, local.name));
                numod.locals = vec![nl];
                newmods.push(numod);
            }
            _ => (),
        }
    }
    return newmods;
}
