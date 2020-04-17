use std::sync::{Arc, Mutex};
use super::loader;
use std::collections::HashSet;
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use super::Name;
use super::makro;
use rayon::prelude::*;
use super::project;
use super::make;
use super::emitter;
use super::abs;
use super::expand;
use super::ast;
use super::flatten;
use super::parser;
use super::symbolic;

static ABORT: AtomicBool = AtomicBool::new(false);

pub struct Pipeline
{
    modules:    HashMap<Name, loader::Module>,
    pb:         Arc<Mutex<pbr::ProgressBar<std::io::Stdout>>>,
    silent:     bool,
    project:    project::Config,
    stage:      make::Stage,
    variant:    String,

    ext:                abs::Ext,
    completed_abs:      HashSet<Name>,
    macros_available:   bool,
    working_on_these:   Arc<Mutex<HashSet<String>>>,
}

impl Pipeline {
    pub fn new(project: project::Config, stage: make::Stage, variant: String, modules: HashMap<Name, loader::Module>) -> Self
    {
        let pb =  Arc::new(Mutex::new(pbr::ProgressBar::new(modules.len() as u64)));
        pb.lock().unwrap().show_speed = false;

        let silent = parser::ERRORS_AS_JSON.load(Ordering::SeqCst);

        Self {
            variant,
            stage,
            project,
            pb,
            silent,
            modules,
            ext: abs::Ext::new(),
            completed_abs: HashSet::new(),
            macros_available:   false,
            working_on_these:   Arc::new(Mutex::new(HashSet::new())),
        }
    }

    pub fn build(mut self, buildset: super::BuildSet) {
        self.do_macros();
        for artifact in std::mem::replace(&mut self.project.artifacts, None).expect("no artifacts") {
            match (&artifact.typ, &buildset) {
                (project::ArtifactType::Test, super::BuildSet::Tests)  => (),
                (project::ArtifactType::Test, _)                => continue,
                (project::ArtifactType::Exe, _)                 => (),
                (_, super::BuildSet::Run)                       => continue,
                (_,_)                                           => (),
            };
            self.do_artifact(artifact, &buildset);
        }
    }

    fn do_macros(&mut self) {
        self.pb_reset();
        let more : Vec<ast::Module> = self.modules.par_iter().map(|(_,module)|{
            match module {
                loader::Module::C(_) => {
                    Vec::new()
                }
                loader::Module::ZZ(ast) => {
                    let hn = ast.name.human_name();
                    self.pb_doing("sieve", hn.clone());
                    let r = makro::sieve(ast);
                    self.pb_done("sieve", hn);
                    r
                }
            }
        }).flatten().collect();

        for macromod in more {
            let artifact = project::Artifact{
                name:       macromod.name.0[1..].join("_"),
                main:       format!("{}", macromod.name),
                typ:        project::ArtifactType::Macro,
                indexjs:    None,
            };
            self.modules.insert(macromod.name.clone(), loader::Module::ZZ(macromod));
            self.do_artifact(artifact, &super::BuildSet::Run);
        }

        self.macros_available = true;
    }

    fn do_abs(&mut self) {
        self.pb_reset();

        for name in self.modules.keys().cloned().collect::<Vec<Name>>() {
            if !self.completed_abs.insert(name.clone()) {
                continue;
            }
            let hn = name.human_name();
            self.pb_doing("abs", hn.clone());

            let mut md = self.modules.remove(&name).unwrap();
            match &mut md {
                loader::Module::C(_) => (),
                loader::Module::ZZ(ast) => {
                    if !abs::abs(ast, &self.modules, self.ext.clone(), self.macros_available) {
                        self.completed_abs.remove(&name);
                    }
                }
            }
            self.modules.insert(name.clone(), md);
            self.pb_done("abs", hn);
        }
    }

    fn do_emit(&self, ast: &mut ast::Module) -> Result<emitter::CFile, Option<super::Error>> {
        if let Some(v) = self.from_buildcache(&ast.name) {
            return Ok(v);
        }

        let mut module = flatten::flatten(ast, &self.modules, self.ext.clone());
        expand::expand(&mut module).map_err(|e|Some(e))?;
        let (ok, complete) = symbolic::execute(&mut module, false/*TODO*/);
        if !ok {
            return Err(None);
        }

        let header  = super::emitter::Emitter::new(&self.project.project, self.stage.clone(), module.clone(), true);
        header.emit();

        let rsbridge = super::emitter_rs::Emitter::new(&self.project.project, self.stage.clone(), module.clone());
        rsbridge.emit();

        let jsbridge = super::emitter_js::Emitter::new(&self.project.project, self.stage.clone(), module.clone());
        jsbridge.emit();

        let docs = super::emitter_docs::Emitter::new(&self.project.project, self.stage.clone(), module.clone());
        docs.emit();

        let em = super::emitter::Emitter::new(&self.project.project, self.stage.clone(), module, false);
        let cf = em.emit();

        if complete {
            self.to_buildcache(&cf);
        }

        Ok(cf)
    }

    fn do_artifact(&mut self, artifact: project::Artifact, buildset: &super::BuildSet) {
        self.do_abs();

        self.pb_reset();

        let cfiles = self.modules.keys().cloned().collect::<Vec<Name>>().into_par_iter().filter_map(|name|{
            let hn = name.human_name();
            let mut module = self.modules.get(&name).unwrap().clone();
            match &mut module {
                loader::Module::C(c) => {
                    self.pb_done("comp", hn);
                    Some((name.clone(), emitter::CFile{
                        name:       name,
                        filepath:   c.to_string_lossy().into(),
                        sources:    HashSet::new(),
                        deps:       HashSet::new(),
                        symbols:    HashSet::new(),
                    }))
                }
                loader::Module::ZZ(ast) => {
                    self.pb_doing("comp", hn.clone());
                    let r = self.do_emit(ast);
                    self.pb_done("comp", hn);
                    match r {
                        Err(None) => {
                            ABORT.store(true, Ordering::Relaxed);
                            None
                        }
                        Err(Some(e)) => {
                            parser::emit_error(e.message.clone(), &e.details);
                            ABORT.store(true, Ordering::Relaxed);
                            None
                        }
                        Ok(v) => {
                            Some((v.name.clone(), v))
                        }
                    }
                }
            }
        }).collect::<HashMap<Name, emitter::CFile>>();

        if ABORT.load(Ordering::Relaxed) {
            std::process::exit(1);
        }

        let mut main = Name::from(&artifact.main);
        if !main.is_absolute() {
            main.0.insert(0,String::new());
        }
        let mut need = vec![main];
        let mut used : HashSet<Name> = HashSet::new();
        let mut symbols : HashSet<Name> = HashSet::new();

        let mut make = make::Make::new(self.project.clone(), &self.variant, self.stage.clone(), artifact.clone());

        while need.len() > 0 {
            for n in std::mem::replace(&mut need, Vec::new()) {
                if !used.insert(n.clone()) {
                    continue
                }
                let n = cfiles.get(&n).expect(&format!("ICE: dependency {} module doesnt exist", n));
                for d in &n.deps {
                    need.push(d.clone());
                }
                symbols.extend(n.symbols.clone());
                make.build(n);
            }
        }

        for entry in std::fs::read_dir("./src").unwrap() {
            let entry = entry.unwrap();
            let path  = entry.path();
            if path.is_file() {
                if let Some("c") = path.extension().map(|v|v.to_str().expect("invalid file name")) {
                    make.cobject(&path);
                }
            }
        }

        make.build(&emitter::builtin(&self.project.project, &self.stage, &artifact, symbols));


        if buildset != &super::BuildSet::Check {
            make.link();
        }
    }


    fn to_buildcache(&self, cf: &emitter::CFile)
    {
        let (_, outname) = emitter::outname(&self.project.project, &self.stage, &cf.name, false);
        let cachename = format!("{}.buildcache", outname);

        let cachefile = std::fs::File::create(&cachename).expect(&format!("cannot create {}", cachename));
        serde_cbor::ser::to_writer(cachefile, &cf).expect(&format!("cannot write {}", cachename));
    }

    fn from_buildcache(&self, module: &Name) -> Option<emitter::CFile> {
        let (_, outname) = emitter::outname(&self.project.project, &self.stage, module, false);

        let cachename = format!("{}.buildcache", outname);
        let cached : Option<emitter::CFile> = match std::fs::File::open(&cachename) {
            Ok(f) => {
                match serde_cbor::from_reader(&f) {
                    Ok(cf) => Some(cf),
                    Err(_) => {
                        std::fs::remove_file(&cachename).expect(&format!("cannot remove {}", cachename));
                        None
                    }
                }

            },
            Err(_) => None,
        };

        if let Some(cached) = cached {
            if !cached.is_newer_than(&outname) && !cached.is_newer_than(&cachename) {
                return Some(cached)
            }
        }
        None
    }

    fn pb_reset(&self) {
        let mut pb =  pbr::ProgressBar::new(self.modules.len() as u64);
        pb.show_speed = false;
        std::mem::replace(&mut *self.pb.lock().unwrap(), pb);
    }

    fn pb_doing(&self, action: &str, on: String) {
        self.working_on_these.lock().unwrap().insert(on);
        self.pb_tick(action);
    }

    fn pb_done(&self, action: &str, on: String) {
        self.working_on_these.lock().unwrap().remove(&on);
        self.pb.lock().unwrap().inc();
        self.pb_tick(action);
    }

    fn pb_tick(&self, action: &str) {
        if self.silent {
            return;
        }
        let mut indic = String::new();
        for working_on in  self.working_on_these.lock().unwrap().iter() {
            if !indic.is_empty() {
                indic.push_str(", ");
            }
            if indic.len() > 30 {
                indic = format!("{}.. ", indic);
                break;
            }
            indic = format!("{}{} ", indic, working_on);
        }
        indic = format!("{} [ {}]  ", action, indic);
        self.pb.lock().unwrap().message(&indic);
        self.pb.lock().unwrap().tick();
    }


}
