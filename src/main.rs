extern crate clap;
#[macro_use]
extern crate log;
extern crate env_logger;
extern crate pbr;
extern crate rayon;
extern crate tempdir;

use clap::{App, Arg, SubCommand};
use std::io::{Read, Write};
use std::process::Command;
use std::sync::atomic::Ordering;
use std::time::{Duration, Instant};
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
        .arg(
            Arg::with_name("smt-timeout")
                .takes_value(true)
                .required(false)
                .long("smt-timeout"),
        )
        .subcommand(
            SubCommand::with_name("check")
                .about("check the current project")
                .arg(
                    Arg::with_name("slow")
                        .takes_value(false)
                        .required(false)
                        .long("slow")
                        .short("0"),
                )
                .arg(
                    Arg::with_name("variant")
                        .takes_value(true)
                        .required(false)
                        .long("variant")
                        .short("s"),
                )
                .arg(
                    Arg::with_name("release")
                        .takes_value(false)
                        .required(false)
                        .long("release"),
                )
                .arg(
                    Arg::with_name("debug")
                        .takes_value(false)
                        .required(false)
                        .long("debug"),
                ),
        )
        .subcommand(
            SubCommand::with_name("build")
                .about("build the current project")
                .arg(
                    Arg::with_name("slow")
                        .takes_value(false)
                        .required(false)
                        .long("slow")
                        .short("0"),
                )
                .arg(
                    Arg::with_name("variant")
                        .takes_value(true)
                        .required(false)
                        .long("variant")
                        .short("s"),
                )
                .arg(
                    Arg::with_name("release")
                        .takes_value(false)
                        .required(false)
                        .long("release"),
                )
                .arg(
                    Arg::with_name("debug")
                        .takes_value(false)
                        .required(false)
                        .long("debug"),
                )
                .arg(
                    Arg::with_name("artifact")
                        .takes_value(true)
                        .required(false)
                        .index(1),
                )
                .arg(
                    Arg::with_name("export")
                        .takes_value(false)
                        .required(false)
                        .long("export"),
                ),
        )
        .subcommand(SubCommand::with_name("clean").about("remove the target directory"))
        .subcommand(
            SubCommand::with_name("bench")
                .about("benchmark tests/*.zz")
                .arg(
                    Arg::with_name("testname")
                        .takes_value(true)
                        .required(false)
                        .index(1),
                ),
        )
        .subcommand(
            SubCommand::with_name("test")
                .about("execute tests/*.zz")
                .arg(
                    Arg::with_name("testname")
                        .takes_value(true)
                        .required(false)
                        .index(1),
                ),
        )
        .subcommand(SubCommand::with_name("init").about("init zz project in current directory"))
        .subcommand(
            SubCommand::with_name("run")
                .about("build and run")
                .arg(
                    Arg::with_name("release")
                        .takes_value(false)
                        .required(false)
                        .long("release"),
                )
                .arg(
                    Arg::with_name("debug")
                        .takes_value(false)
                        .required(false)
                        .long("debug"),
                )
                .arg(
                    Arg::with_name("variant")
                        .takes_value(true)
                        .required(false)
                        .long("variant")
                        .short("s"),
                )
                .arg(
                    Arg::with_name("args")
                        .takes_value(true)
                        .multiple(true)
                        .required(false)
                        .index(1),
                ),
        )
        .subcommand(
            SubCommand::with_name("fuzz")
                .about("execute tests/*.zz with afl fuzzer")
                .arg(
                    Arg::with_name("testname")
                        .takes_value(true)
                        .required(false)
                        .index(1),
                ),
        )
        .get_matches();

    if let Some(t) = matches.value_of("smt-timeout") {
        zz::smt::TIMEOUT.store(t.parse().unwrap(), Ordering::Relaxed);
    }

    match matches.subcommand() {
        ("init", Some(_submatches)) => {
            zz::project::init();
        }
        ("clean", Some(_submatches)) => {
            let (root, _) = zz::project::load_cwd();
            if root.join("target").exists() {
                std::fs::remove_dir_all(root.join("target")).unwrap();
            }
        }
        ("test", Some(submatches)) | ("bench", Some(submatches)) => {
            let bench = matches.subcommand().0 == "bench";

            let variant = submatches.value_of("variant").unwrap_or("default");
            let stage = zz::make::Stage::test();
            zz::build(zz::BuildSet::Tests, variant, stage.clone(), false);
            let (root, mut project) = zz::project::load_cwd();

            for artifact in std::mem::replace(&mut project.artifacts, None).expect("no artifacts") {
                if let zz::project::ArtifactType::Test = artifact.typ {
                    if let Some(testname) = submatches.value_of("testname") {
                        if testname != artifact.name {
                            if format!("tests_{}", testname) != artifact.name {
                                continue;
                            }
                        }
                    }

                    let casedir = root
                        .join("target")
                        .join(stage.to_string())
                        .join("testcases")
                        .join(format!("{}", artifact.main.replace("::", "_")));
                    let mut cases = Vec::new();
                    match std::fs::read_dir(casedir) {
                        Err(_) => (),
                        Ok(dir) => {
                            for entry in dir {
                                let entry = match entry {
                                    Ok(v) => v,
                                    Err(_) => continue,
                                };
                                let path = entry.path();
                                let mut stdin = None;
                                let mut stdout = None;
                                let mut exit = 0;
                                match std::fs::File::open(path.join("stdin")) {
                                    Ok(mut f) => {
                                        let mut v = Vec::new();
                                        f.read_to_end(&mut v).unwrap();
                                        stdin = Some(v);
                                    }
                                    Err(_) => {
                                        eprintln!(
                                            "stdin testfile not found {}",
                                            path.to_string_lossy()
                                        );
                                    }
                                }
                                match std::fs::File::open(path.join("stdout")) {
                                    Ok(mut f) => {
                                        let mut v = Vec::new();
                                        f.read_to_end(&mut v).unwrap();
                                        stdout = Some(v);
                                    }
                                    Err(_) => {
                                        eprintln!(
                                            "stdout testfile not found {}",
                                            path.to_string_lossy()
                                        );
                                    }
                                }

                                match std::fs::File::open(path.join("exit")) {
                                    Ok(mut f) => {
                                        let mut v = String::new();
                                        f.read_to_string(&mut v).unwrap();
                                        exit = v.parse().unwrap_or(0);
                                    }
                                    Err(_) => {}
                                }
                                cases.push((
                                    entry.file_name().to_string_lossy().to_string(),
                                    stdin,
                                    stdout,
                                    exit,
                                ));
                            }
                        }
                    }

                    if cases.is_empty() {
                        cases.push(("default".to_string(), None, None, 0));
                    }

                    for case in &cases {
                        let running = root
                            .join("target")
                            .join(stage.to_string())
                            .join("bin")
                            .join(&artifact.name);
                        println!("running \"{}\"\n", running.to_string_lossy());
                        let start = Instant::now();
                        let mut average = 0;
                        loop {
                            let istart = Instant::now();
                            let mut child = Command::new(&running)
                                .stdin(std::process::Stdio::piped())
                                .stdout(std::process::Stdio::piped())
                                .spawn()
                                .expect("failed to execute process");

                            if let Some(i) = &case.1 {
                                let stdin = child.stdin.as_mut().expect("Failed to open stdin");
                                stdin.write_all(&i).unwrap();
                            }
                            let output = child.wait_with_output().expect("Failed to read stdout");
                            average = (average + istart.elapsed().as_millis()) / 2;

                            match output.status.code() {
                                Some(c) => {
                                    if c != case.3 {
                                        error!(
                                            "FAIL {}::{} exit: {} instead of: {}",
                                            artifact.name, case.0, c, case.3
                                        );
                                        std::process::exit(10);
                                    }
                                }
                                _ => {
                                    #[cfg(unix)]
                                    {
                                        use std::os::unix::process::ExitStatusExt;
                                        error!(
                                            "FAIL {}::{} died by signal {}",
                                            artifact.name,
                                            case.0,
                                            output.status.signal().unwrap()
                                        );
                                    }
                                    #[cfg(not(unix))]
                                    {
                                        error!("FAIL {}::{} died by signal", artifact.name, case.0);
                                    }
                                    std::process::exit(10);
                                }
                            }
                            if let Some(expect_stdout) = &case.2 {
                                let mut output_stdout = output.stdout;
                                output_stdout.retain(|&i| i != b'\r');
                                if &output_stdout != expect_stdout {
                                    error!("FAIL {} {} \nstdout expected:\n<{}>({})\nbut got:\n<{}>({})\n",
                                           artifact.name,
                                           case.0,
                                           String::from_utf8_lossy(&expect_stdout),
                                           expect_stdout.len(),
                                           String::from_utf8_lossy(&output_stdout),
                                           output_stdout.len()
                                          );
                                    std::process::exit(10);
                                }
                            }
                            if bench {
                                if start.elapsed().as_secs() > 0 {
                                    info!("PASS {} {} {}ms/iter", artifact.name, case.0, average);
                                    break;
                                } else {
                                    continue;
                                }
                            } else {
                                info!("PASS {} {} in {}ms", artifact.name, case.0, average);
                                break;
                            }
                        }
                    }
                }
            }
        }
        ("run", Some(submatches)) => {
            let stage = if submatches.is_present("release") {
                zz::make::Stage::release()
            } else if submatches.is_present("debug") {
                zz::make::Stage::debug()
            } else {
                zz::make::Stage::test()
            };
            let variant = submatches.value_of("variant").unwrap_or("default");
            zz::build(zz::BuildSet::Run, variant, stage.clone(), false);
            let (root, mut project) = zz::project::load_cwd();

            let mut exes = Vec::new();
            for artifact in std::mem::replace(&mut project.artifacts, None).expect("no artifacts") {
                if let zz::project::ArtifactType::Exe = artifact.typ {
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

            let running = root
                .join("target")
                .join(stage.to_string())
                .join("bin")
                .join(&exes[0].name);
            println!("running \"{}\"\n", running.to_string_lossy());
            let status = Command::new(running)
                .args(submatches.values_of("args").unwrap_or_default())
                .status()
                .expect("failed to execute process");
            std::process::exit(status.code().expect("failed to execute process"));
        }
        ("fuzz", Some(submatches)) => {
            let variant = submatches.value_of("variant").unwrap_or("default");
            let stage = zz::make::Stage::fuzz();
            zz::build(zz::BuildSet::Tests, variant, stage.clone(), false);
            let (root, mut project) = zz::project::load_cwd();

            let mut exes = Vec::new();
            for artifact in std::mem::replace(&mut project.artifacts, None).expect("no artifacts") {
                if let zz::project::ArtifactType::Test = artifact.typ {
                    match submatches.value_of("testname") {
                        Some(v) if v == artifact.name => {
                            exes.push((artifact.name, artifact.main));
                        }
                        Some(_) => {}
                        None => {
                            exes.push((artifact.name, artifact.main));
                        }
                    }
                }
            }

            if exes.len() == 0 {
                if let Some(_) = submatches.value_of("testname") {
                    eprintln!("no such test name");
                } else {
                    eprintln!("no tests");
                }
                std::process::exit(1);
            }

            if exes.len() > 1 {
                eprintln!("specify which test to run:");
                for (exe, _) in exes {
                    eprintln!(" - {}", exe);
                }
                std::process::exit(1);
            }

            let indir = tempdir::TempDir::new("zzfuzz").unwrap();
            let casedir = root
                .join("target")
                .join(stage.to_string())
                .join("testcases")
                .join(format!("{}", exes[0].1.replace("::", "_")));
            let mut havesome = false;
            match std::fs::read_dir(casedir) {
                Err(_) => (),
                Ok(dir) => {
                    for entry in dir {
                        let entry = match entry {
                            Ok(v) => v,
                            Err(_) => continue,
                        };
                        let path = entry.path();
                        let stdin = path.join("stdin");
                        if stdin.exists() {
                            havesome = true;
                            std::fs::copy(&stdin, indir.path().join(path.file_name().unwrap()))
                                .unwrap();
                        }
                    }
                }
            }

            if !havesome {
                eprintln!("the test {} has no testcases with stdin", exes[0].0);
                std::process::exit(1);
            }

            let outdir = root.join("target").join(stage.to_string()).join(&exes[0].1);
            std::fs::create_dir_all(&outdir).unwrap();

            println!("fuzzer output in {}", outdir.to_string_lossy());

            let mut child = Command::new("afl-fuzz")
                .arg("-m30000")
                .arg("-i")
                .arg(indir.path())
                .arg("-o")
                .arg(&outdir)
                .arg(
                    root.join("target")
                        .join(stage.to_string())
                        .join("bin")
                        .join(&exes[0].0),
                )
                .spawn()
                .expect("failed to execute process");
            child.wait().unwrap();

            println!("\n\nfuzzer output in {}", outdir.to_string_lossy());
            return;
        }
        ("check", Some(submatches)) => {
            zz::parser::ERRORS_AS_JSON.store(true, Ordering::SeqCst);
            zz::build(
                zz::BuildSet::Check,
                submatches.value_of("variant").unwrap_or("default"),
                zz::make::Stage::test(),
                false,
            )
        }
        ("build", Some(submatches)) => {
            let stage = if submatches.is_present("release") {
                zz::make::Stage::release()
            } else if submatches.is_present("debug") {
                zz::make::Stage::debug()
            } else {
                zz::make::Stage::test()
            };

            let set = if submatches.is_present("export") {
                zz::BuildSet::Export
            } else if let Some(v) = submatches.value_of("artifact") {
                zz::BuildSet::Named(v.to_string())
            } else {
                zz::BuildSet::All
            };

            zz::build(
                set,
                submatches.value_of("variant").unwrap_or("default"),
                stage,
                submatches.is_present("slow"),
            )
        }
        ("", None) => {
            zz::build(zz::BuildSet::All, "default", zz::make::Stage::test(), false);
        }
        _ => unreachable!(),
    }
}
