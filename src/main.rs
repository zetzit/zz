#[macro_use] extern crate pest_derive;
extern crate clap;
#[macro_use] extern crate log;
extern crate env_logger;
extern crate pbr;
extern crate rayon;

mod ast;
mod parser;
mod project;
mod make;
mod loader;
mod flatten;
mod emitter;
mod abs;
mod name;
mod pp;
mod lifetimes;

use std::path::Path;
use clap::{App, Arg, SubCommand};
use std::process::Command;
use name::Name;
use std::collections::HashSet;
use std::collections::HashMap;
use std::sync::atomic::{Ordering};
use zz;

fn main() {
    if let Err(_) = std::env::var("RUST_LOG") {
        std::env::set_var("RUST_LOG", "info");
    }
    env_logger::builder()
        //.default_format_module_path(false)
        .default_format_timestamp(false)
        .default_format_module_path(false)
        .init();

    let matches = App::new("Drunk Octopus")
        .version(clap::crate_version!())
        .setting(clap::AppSettings::UnifiedHelpMessage)
        .subcommand(SubCommand::with_name("check").about("check the current project"))
        .subcommand(SubCommand::with_name("build").about("build the current project")
            .arg(Arg::with_name("variant").takes_value(true).required(false).long("variant").short("s"))
        )
        .subcommand(SubCommand::with_name("clean").about("remove the target directory"))
        .subcommand(SubCommand::with_name("test").about("execute tests/*.zz")
                    .arg(Arg::with_name("testname").takes_value(true).required(false).index(1)),
                    )
        .subcommand(SubCommand::with_name("init").about("init zz project in current directory"))
        .subcommand(
            SubCommand::with_name("run").about("build and run")
            .arg(Arg::with_name("variant").takes_value(true).required(false).long("variant").short("s"))
            .arg(Arg::with_name("args").takes_value(true).multiple(true).required(false).index(1))
        )
        .get_matches();

    match matches.subcommand() {
        ("init", Some(_submatches)) => {
            project::init();
        },
        ("clean", Some(_submatches)) => {
            let (root, _) = project::load_cwd();
            std::env::set_current_dir(root).unwrap();
            if std::path::Path::new("./target").exists() {
                std::fs::remove_dir_all("target").unwrap();
            }
        },
        ("test", Some(submatches)) => {
            let variant = submatches.value_of("variant").unwrap_or("default");
            zz::build(true, false, variant);
            let (root, mut project) = project::load_cwd();
            std::env::set_current_dir(root).unwrap();

            for artifact in std::mem::replace(&mut project.artifacts, None).expect("no artifacts") {
                if let project::ArtifactType::Test = artifact.typ {
                    if let Some(testname) = submatches.value_of("testname") {
                        if testname != artifact.name {
                            if format!("tests::{}", testname) != artifact.name {
                                continue;
                            }
                        }
                    }
                    println!("running \"./target/{}/{}\"\n", variant, artifact.name);
                    let status = Command::new(format!("./target/{}/{}", variant, artifact.name))
                        .status()
                        .expect("failed to execute process");
                    if let Some(0) = status.code()  {
                        info!("PASS {}", artifact.name);
                    } else {
                        error!("FAIL {} {:?}", artifact.name, status);
                        std::process::exit(10);
                    }
                }
            }

        }
        ("run", Some(submatches)) => {
            let variant = submatches.value_of("variant").unwrap_or("default");
            zz::build(false, false, variant);
            let (root, mut project) = project::load_cwd();
            std::env::set_current_dir(root).unwrap();

            let mut exes = Vec::new();
            for artifact in std::mem::replace(&mut project.artifacts, None).expect("no artifacts") {
                if let project::ArtifactType::Exe = artifact.typ {
                    exes.push(artifact);
                }
            }
            if exes.len() < 1 {
                error!("no exe artifact to run");
                std::process::exit(9);
            }
            if exes.len() > 1 {
                error!("multiple exe artifacts");
                std::process::exit(9);
            }

            println!("running \"./target/{}/{}\"\n", variant, exes[0].name);
            let status = Command::new(format!("./target/{}/{}", variant, exes[0].name))
                .args(submatches.values_of("args").unwrap_or_default())
                .status()
                .expect("failed to execute process");
            std::process::exit(status.code().expect("failed to execute process"));
        },
        ("check", Some(submatches)) => {
            parser::ERRORS_AS_JSON.store(true, Ordering::SeqCst);
            zz::build(false, true, submatches.value_of("variant").unwrap_or("default"))
        },
        ("build", Some(submatches)) => {
            zz::build(false, false, submatches.value_of("variant").unwrap_or("default"))
        },
        ("", None) => {
            zz::build(false, false, "default");
        },
        _ => unreachable!(),
    }
}

