#![allow(unused)]

use super::ast;
use super::flatten;
use super::make;
use super::name::Name;
use super::parser::{self, emit_error};
use super::project::Project;
use std::collections::HashSet;
use std::collections::HashMap;
use super::project;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use super::emitter_common;

pub struct CFile {
    pub name: Name,
    pub filepath: String,
    pub sources: HashSet<PathBuf>,
    pub deps: HashSet<Name>,
}

pub struct Emitter {
    p: String,
    project_name: String,
    f: fs::File,
    module: flatten::Module,
    cur_loc: Option<ast::Location>,
    register_structs: Vec<String>,
    register_fns: HashMap<String, String>,
}

pub fn make_npm_module(make: &super::make::Make) {
    let td      = project::target_dir();
    let pdir_   = td.join("npm").join(&make.artifact.name);
    let pdir    = std::path::Path::new(&pdir_);
    std::fs::create_dir_all(&pdir).unwrap();

    let p = pdir.join("package.json");
    let mut f = fs::File::create(&p).expect(&format!("cannot create {:?}", p));
    write!(
        f,
        r#"
{{
  "name": "{}",
  "version": "1.0.0",
  "description": "",
  "main": "index.js",
  "scripts": {{
    "test": "echo \"Error: no test specified\" && exit 1",
    "install": "node-gyp rebuild"
  }},
  "author": "",
  "license": "ISC",
  "gypfile": true,
  "dependencies": {{
    "bindings": "~1.2.1"
  }}
}}
"#,
        make.artifact.name
    )
    .unwrap();

    let p = pdir.join("index.js");
    if let Some(indexjs) = &make.artifact.indexjs {
        std::fs::copy(indexjs, &p).expect(&format!("cannot copy {} to {:?}", indexjs, p));
    } else {
        let mut f = fs::File::create(&p).expect(&format!("cannot create {:?}", p));
        write!(
            f,
            "module.exports = require('bindings')('{}');\n",
            make.artifact.name
        )
        .unwrap();
    }

    let mut register_modules = Vec::new();

    let p = pdir.join("binding.gyp");
    let mut f = fs::File::create(&p).expect(&format!("cannot create {:?}", p));
    write!(
        f,
        r#"
{{
  "targets": [
    {{
      "target_name": "{n}",
      "sources": [
"#,
        n = make.artifact.name
    )
    .unwrap();

    for step in &make.steps {
        write!(f,"      \"{}\", \n",
            emitter_common::path_rel(&pdir, &step.source).to_string_lossy().to_string()
        ).unwrap();

        // for every linked zz file, add the js bridge
        if step.source.parent().unwrap().file_name().unwrap() == "zz" {
            let mut mn = step
                .source
                .file_stem()
                .unwrap()
                .to_string_lossy()
                .to_string();
            write!(f,"      \"../{}.c\",\n",
                step.source.file_stem().unwrap().to_string_lossy()
            )
            .unwrap();
            register_modules.push(step.source.file_stem().unwrap().to_string_lossy());
        }
    }

    write!(
        f,
        r#"
        "js.c"
      ],
      "conditions": [
          ['OS!="win"', {{
            'cflags': [
                '-Wno-attributes',
                '-Wno-old-style-declaration',
                '-Wno-unused-function',
                '-Wno-unused-variable',
                '-Wno-gnu-binary-literal',
                '-Wno-zero-length-array',
                '-Wno-duplicate-decl-specifier',
"#
    )
    .unwrap();
    for flag in &make.cflags {
        write!(f, r#"              "{}","#, flag).unwrap();
    }

    write!(
        f,
        r#"
            ],
          }}],
      ],
      "include_dirs": [
          "include", "../../include",
      ],
    }}
  ]
}}
"#,
    )
    .unwrap();

    let p = pdir.join("js.c");
    let mut f = fs::File::create(&p).expect(&format!("cannot create {:?}", p));
    write!(
        f,
        r#"
#include <node_api.h>
#include <assert.h>
#include <string.h>
"#
    )
    .unwrap();

    for m in &register_modules {
        write!(
            f,
            "napi_value js_{}_Init(napi_env env, napi_value exports);\n",
            m
        )
        .unwrap();
    }

    write!(
        f,
        "\nnapi_value Init(napi_env env, napi_value exports)\n{{\n"
    )
    .unwrap();

    write!(
        f,
        r#"
        napi_status status;
        napi_value  m;
    "#
    )
    .unwrap();

    for m in &register_modules {
        write!(
            f,
            r#"
    status = napi_create_object(env, &m);
    assert(status == napi_ok);
    m = js_{}_Init(env, m);
    status = napi_set_named_property(env, exports, "{}", m);
    assert(status == napi_ok);
"#,
            m, m
        )
        .unwrap();
    }

    write!(f, "    return exports;\n").unwrap();
    write!(f, "}}\n").unwrap();
    write!(f, "NAPI_MODULE({}, Init)\n", make.artifact.name).unwrap();
}

pub fn outname(_project: &Project, stage: &make::Stage, module: &flatten::Module) -> String {
    let td = project::target_dir().join("npm");
    std::fs::create_dir_all(&td);
    td.join(format!("{}.c", module.name.0[1..].join("_"))).to_string_lossy().to_string()
}

impl Emitter {
    pub fn new(project: &Project, stage: make::Stage, module: flatten::Module) -> Self {

        let p = outname(project, &stage, &module);
        let f = fs::File::create(&p).expect(&format!("cannot create {}", p));

        Emitter {
            p,
            project_name: project.name.clone(),
            f,
            module,
            cur_loc: None,
            register_structs: Vec::new(),
            register_fns: HashMap::new(),
        }
    }

    fn emit_loc(&mut self, loc: &ast::Location) {
        if let Some(cur_loc) = &self.cur_loc {
            if cur_loc.file == loc.file && cur_loc.line == loc.line {
                return;
            }
        }
        self.cur_loc = Some(loc.clone());
        //write!(self.f, "// line {} \"{}\"\n", loc.line(), loc.file).unwrap();
    }

    fn to_local_typed_name(&self, name: &ast::Typed) -> String {
        match name.t {
            ast::Type::U8 => "uint8_t".to_string(),
            ast::Type::U16 => "uint16_t".to_string(),
            ast::Type::U32 => "uint32_t".to_string(),
            ast::Type::U64 => "uint64_t".to_string(),
            ast::Type::U128 => "uint128_t".to_string(),
            ast::Type::I8 => "int8_t".to_string(),
            ast::Type::I16 => "int16_t".to_string(),
            ast::Type::I32 => "int32_t".to_string(),
            ast::Type::I64 => "int64_t".to_string(),
            ast::Type::I128 => "int128_t".to_string(),
            ast::Type::Int => "int".to_string(),
            ast::Type::UInt => "unsigned int".to_string(),
            ast::Type::ISize => "intptr_t".to_string(),
            ast::Type::USize => "uintptr_t".to_string(),
            ast::Type::Bool => "bool".to_string(),
            ast::Type::F32 => "float".to_string(),
            ast::Type::F64 => "double".to_string(),
            ast::Type::Other(ref n) => {
                let mut s = self.to_local_name(&n);
                match &name.tail {
                    ast::Tail::Dynamic(_) | ast::Tail::None | ast::Tail::Bind(_, _) => {}
                    ast::Tail::Static(v, _) => {
                        s = format!("{}_{}", s, v);
                    }
                }
                s
            }
            ast::Type::ILiteral | ast::Type::ULiteral | ast::Type::Elided | ast::Type::New => {
                parser::emit_error(
                    "ICE: untyped literal ended up in emitter",
                    &[(
                        name.loc.clone(),
                        format!("this should have been resolved earlier"),
                    )],
                );
                std::process::exit(9);
            }
        }
    }

    fn create_js_value(&mut self, localfrom: &str, localto: &str, typed: &ast::Typed) {
        if typed.ptr.len() == 1 {
            if let ast::Type::Other(n) = &typed.t {
                if n.0.last().unwrap() == "char" {
                    write!(self.f, "    status = napi_create_string_utf8(env, {from}, strlen({from}), &{to});\n",
                        from=localfrom, to=localto).unwrap();
                    return;
                }
            }
        }
        if typed.ptr.len() > 0 {
            //TODO
            return;
        }
        match typed.t {
            ast::Type::U8
            | ast::Type::U16
            | ast::Type::U32
            | ast::Type::UInt
            | ast::Type::USize => {
                write!(
                    self.f,
                    "    status = napi_create_uint32(env, {}, &{});\n",
                    localfrom, localto
                )
                .unwrap();
            }
            ast::Type::U64 | ast::Type::U128 => {
                write!(
                    self.f,
                    "    status = napi_create_bigint_uint64(env, {}, &{});\n",
                    localfrom, localto
                )
                .unwrap();
            }
            ast::Type::I8 | ast::Type::I16 | ast::Type::I32 | ast::Type::Int | ast::Type::ISize => {
                write!(
                    self.f,
                    "    status = napi_create_int32(env, {}, &{});\n",
                    localfrom, localto
                )
                .unwrap();
            }
            ast::Type::I64 | ast::Type::I128 => {
                write!(
                    self.f,
                    "    status = napi_create_int64(env, {}, &{});\n",
                    localfrom, localto
                )
                .unwrap();
            }
            ast::Type::Bool => {
                write!(
                    self.f,
                    "    status = napi_create_uint32(env, {}, &{});\n",
                    localfrom, localto
                )
                .unwrap();
            }

            ast::Type::F32 | ast::Type::F64 => {
                write!(
                    self.f,
                    "    status = napi_create_double(env, {}, &{});\n",
                    localfrom, localto
                )
                .unwrap();
            }
            ast::Type::Other(ref n) => {
                //TODO
            }
            ast::Type::ILiteral | ast::Type::ULiteral | ast::Type::Elided | ast::Type::New => {
                parser::emit_error(
                    "ICE: untyped literal ended up in emitter",
                    &[(
                        typed.loc.clone(),
                        format!("this should have been resolved earlier"),
                    )],
                );
                std::process::exit(9);
            }
        }
    }

    fn from_js_value(&mut self, localfrom: &str, localto: &str, typed: &ast::Typed) {
        if typed.ptr.len() == 1 {
            if let ast::Type::Other(n) = &typed.t {
                if n.0.last().unwrap() == "char" {
                    write!(
                        self.f,
                        r#"
                        {{
                                static char buf[10000];
                                size_t rs = 0;
                                status = napi_get_value_string_utf8(env, {}, buf, 10000, &rs);
                                buf[rs] = 0;
                                {} = buf;
                        }}
                        "#,
                        localfrom, localto
                    )
                    .unwrap();
                    return;
                }
            }


            let localto_clean = localto.replace(|c: char| !c.is_ascii_alphanumeric(), "_");

            write!(
                self.f,
                r#"
    void * tttt_{localto_clean} = 0;
    size_t {localto_clean}_tail = 0;
    status = napi_unwrap(env, {localfrom}, &tttt_{localto_clean});
    if (tttt_{localto_clean} == 0 || status != napi_ok) {{
        {localto} = 0;
    }} else {{
        {localto_clean}_tail = *((size_t*)tttt_{localto_clean});
        {localto} = tttt_{localto_clean} + sizeof(size_t*);
    }}
    "#,
                localto = localto,
                localfrom = localfrom,
                localto_clean = localto_clean,
            )
            .unwrap();
            return;
        }
        match typed.t {
            ast::Type::U8
            | ast::Type::U16
            | ast::Type::U32
            | ast::Type::UInt
            | ast::Type::USize => {
                write!(
                    self.f,
                    "    status = napi_get_value_uint32(env, {}, (uint32_t*)&{});\n",
                    localfrom, localto
                )
                .unwrap();
            }
            ast::Type::U64 | ast::Type::U128 => {
                write!(
                    self.f,
                    "    status = napi_get_value_bigint_uint64(env, {}, (uint64_t*)&{}, 0);\n",
                    localfrom, localto
                )
                .unwrap();
            }
            ast::Type::I8 | ast::Type::I16 | ast::Type::I32 | ast::Type::Int | ast::Type::ISize => {
                write!(
                    self.f,
                    "    status = napi_get_value_int32(env, {}, (int32_t*)&{});\n",
                    localfrom, localto
                )
                .unwrap();
            }
            ast::Type::I64 | ast::Type::I128 => {
                write!(
                    self.f,
                    "    status = napi_get_value_int64(env, {}, (int64_t*)&{});\n",
                    localfrom, localto
                )
                .unwrap();
            }
            ast::Type::Bool => {
                write!(
                    self.f,
                    "    status = napi_get_value_uint32(env, {}, (uint32_t*)&{});\n",
                    localfrom, localto
                )
                .unwrap();
            }

            ast::Type::F32 | ast::Type::F64 => {
                write!(
                    self.f,
                    "    status = napi_get_value_double(env, {}, &{});\n",
                    localfrom, localto
                )
                .unwrap();
            }
            ast::Type::Other(ref n) => {
                //TODO
            }
            ast::Type::ILiteral | ast::Type::ULiteral | ast::Type::Elided | ast::Type::New => {
                parser::emit_error(
                    "ICE: untyped literal ended up in emitter",
                    &[(
                        typed.loc.clone(),
                        format!("this should have been resolved earlier"),
                    )],
                );
                std::process::exit(9);
            }
        }
    }

    fn to_local_name(&self, s: &Name) -> String {
        if !s.is_absolute() {
            return s.0.join("_");
        }

        assert!(s.is_absolute(), "ICE not abs: '{}'", s);
        if let Some(an) = self.module.aliases.get(&s) {
            return an.clone();
        }

        if s.0[1] == "ext" {
            return s.0.last().unwrap().clone();
        }

        let mut s = s.clone();
        s.0.remove(0);
        return s.0.join("_");
    }

    pub fn emit(mut self) {
        let module = self.module.clone();
        debug!("emitting js {}", module.name);

        write!(
            self.f,
            r#"
#include <assert.h>
#include <node_api.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "zz/{}.h"

"#,
            self.module.name.0[1..].join("_")
        )
        .unwrap();

        for (d, complete) in &module.d {
            if d.vis != ast::Visibility::Export {
                continue;
            }
            match d.def {
                ast::Def::Function { .. } => {
                    self.emit_fndecl(&d);
                }
                _ => (),
            }
        }
        for (d, complete) in &module.d {
            if complete != &flatten::TypeComplete::Complete {
                continue;
            }

            if d.vis != ast::Visibility::Export {
                continue;
            }

            let mut dmodname = Name::from(&d.name);
            dmodname.pop();
            if dmodname != module.name {
                continue;
            }

            self.emit_loc(&d.loc);
            match d.def {
                ast::Def::Flags { .. } => {}
                ast::Def::Macro { .. } => {}
                ast::Def::Const { .. } => self.emit_const(&d),
                ast::Def::Static { .. } => self.emit_static(&d),
                ast::Def::Struct { .. } => {
                    self.emit_struct(&d, None);
                    if let Some(vs) = module.typevariants.get(&Name::from(&d.name)) {
                        for (v, _) in vs {
                            let mut d = d.clone();
                            d.name = format!("{}_{}", d.name, v);
                            self.emit_struct(&d, Some(*v));
                        }
                    }
                }
                ast::Def::Enum { .. } => self.emit_enum(&d),
                ast::Def::Function { .. } => {
                    if !d.name.ends_with("::main") {
                        self.emit_fn(&d);
                    }
                }
                ast::Def::Closure { .. } => {
                    self.emit_closure(&d);
                }
                ast::Def::Symbol { .. } => {}
                ast::Def::Theory { .. } => {}
                ast::Def::Testcase { .. } => {}
                ast::Def::Include { .. } => {}
            }
            write!(self.f, "\n").unwrap();
        }

        write!(
            self.f,
            "\n\nnapi_value js_{}_Init(napi_env env, napi_value exports)\n{{\n",
            module.name.0[1..].join("_")
        )
        .unwrap();
        for f in self.register_structs {
            write!(self.f, "    {}(env, exports);\n", f).unwrap();
        }
        write!(self.f, "    napi_value ff;\n").unwrap();
        write!(self.f, "    napi_status status;\n").unwrap();
        for (longname, shortname) in self.register_fns {
            write!(
                self.f,
                "    status = napi_create_function(env, \"{}\", NAPI_AUTO_LENGTH, {}, 0, &ff);\n",
                shortname, longname
            )
            .unwrap();
            write!(self.f, "    assert(status == napi_ok);\n").unwrap();
            write!(
                self.f,
                "    status = napi_set_named_property(env, exports, \"{}\", ff);\n",
                shortname
            )
            .unwrap();
            write!(self.f, "    assert(status == napi_ok);\n").unwrap();
        }
        write!(self.f, "    return exports;\n").unwrap();
        write!(self.f, "}}\n").unwrap();
    }

    pub fn emit_static(&mut self, ast: &ast::Local) {
        self.emit_loc(&ast.loc);
        let (_typed, _expr, _tags, storage, _array) = match &ast.def {
            ast::Def::Static {
                typed,
                expr,
                tags,
                storage,
                array,
            } => (typed, expr, tags, storage, array),
            _ => unreachable!(),
        };

        match storage {
            ast::Storage::Atomic => {
                return;
            }
            ast::Storage::ThreadLocal => {
                return;
            }
            ast::Storage::Static => (),
        }
    }

    pub fn emit_const(&mut self, ast: &ast::Local) {
        self.emit_loc(&ast.loc);
        let (_typed, _expr) = match &ast.def {
            ast::Def::Const { typed, expr } => (typed, expr),
            _ => unreachable!(),
        };
    }

    pub fn emit_enum(&mut self, ast: &ast::Local) {
        self.emit_loc(&ast.loc);
        let names = match &ast.def {
            ast::Def::Enum { names } => (names),
            _ => unreachable!(),
        };
    }

    pub fn emit_struct(&mut self, ast: &ast::Local, tail_variant: Option<u64>) {


        let (fields, _packed, tail, _union, impls) = match &ast.def {
            ast::Def::Struct {
                fields,
                packed,
                tail,
                union,
                impls,
                ..
            } => (fields, packed, tail, union, impls),
            _ => unreachable!(),
        };

        let shortname = Name::from(&ast.name).0.last().unwrap().clone();
        let longname = self.to_local_name(&Name::from(&ast.name));

        // getters and setters
        for field in fields {
            if let ast::Array::None = field.array {
            } else {
                continue;
            }

            write!(
                self.f,
                r#"
napi_value jsGet_{s}_{f}(napi_env env, napi_callback_info info) {{
  napi_status status;

  napi_value jsthis;
  status = napi_get_cb_info(env, info, 0, 0, &jsthis, 0);
  assert(status == napi_ok);

  void *mem;
  status = napi_unwrap(env, jsthis, &mem);
  assert(status == napi_ok);
  mem += sizeof(size_t);
  {s} * obj = ({s}*)mem;

  napi_value value;
"#,
                f = field.name,
                s = longname,
            )
            .unwrap();

            self.create_js_value(&format!("obj->{}", field.name), "value", &field.typed);

            write!(
                self.f,
                r#"
  assert(status == napi_ok);
  return value;
}}

"#,
            )
            .unwrap();

            if field.tags.contains("mut") {
                write!(
                    self.f,
                    r#"
napi_value jsSet_{s}_{f}(napi_env env, napi_callback_info info) {{
  napi_status status;

  size_t argc = 1;
  napi_value value;
  napi_value jsthis;
  status = napi_get_cb_info(env, info, &argc, &value, &jsthis, 0);
  assert(status == napi_ok);

  void *mem;
  status = napi_unwrap(env, jsthis, &mem);
  assert(status == napi_ok);
  mem += sizeof(size_t);
  {s} * obj = ({s}*)mem;

"#,
                    f = field.name,
                    s = longname,
                )
                .unwrap();

                self.from_js_value("value", &format!("obj->{}", field.name), &field.typed);

                write!(
                    self.f,
                    r#"
  assert(status == napi_ok);
  return 0;
}}

"#,
                )
                .unwrap();
            }
        }

        write!(
            self.f,
            r#"
void js_delete_{}(napi_env env, void *obj, void*hint) {{
    free(obj);
}}
napi_value js_new_{n}(napi_env env, napi_callback_info info) {{
    napi_status status;

    napi_value target;
    status = napi_get_new_target(env, info, &target);
    assert(status == napi_ok);
    bool is_constructor = target != 0;

    assert (is_constructor);

    size_t argc = 1;
    napi_value args[1];
    napi_value jsthis;
    status = napi_get_cb_info(env, info, &argc, args, &jsthis, 0);
    assert(status == napi_ok);

    size_t tail = 0;
    napi_valuetype valuetype;
    status = napi_typeof(env, args[0], &valuetype);
    assert(status == napi_ok);

    if (valuetype != napi_undefined) {{
        status = napi_get_value_uint32(env, args[0], (uint32_t*)&tail);
        assert(status == napi_ok);
    }}
    "#,
            n = longname,
        )
        .unwrap();

        if tail == &ast::Tail::None || tail_variant.is_some() {
            write!(
                self.f,
                r#"
    void *obj = malloc(sizeof(size_t) + sizeof_{n});
    memset(obj, 0, sizeof(size_t) + sizeof_{n});
"#,
                n = longname,
            )
            .unwrap();
        } else {
            write!(
                self.f,
                r#"
    void *obj = malloc(sizeof(size_t) + sizeof_{n}(tail));
    memset(obj, 0, sizeof(size_t) + sizeof_{n}(tail));
"#,
                n = longname,
            )
            .unwrap();
        }

        write!(
            self.f,
            r#"
    *((size_t *)obj) = tail;

    status = napi_wrap(env,
            jsthis,
            obj,
            js_delete_{n}, // destructor
            0,  // finalize_hint
            0
    );
    assert(status == napi_ok);
    return jsthis;

}}

"#,
            n = longname,
        )
        .unwrap();

        write!(
            self.f,
            "void js_register_{} (napi_env env, napi_value exports) {{\n",
            longname
        )
        .unwrap();

        let mut proplen = 0;
        write!(self.f, "    napi_property_descriptor properties[] = {{\n").unwrap();
        for (field, (fnname, _)) in impls {
            let longname = self.to_local_name(fnname);
            if !self.register_fns.contains_key(&longname) {
                continue;
            }
            write!(
                self.f,
                "        {{ \"{}\", 0, js_{}, 0, 0, 0, napi_default, 0 }},\n",
                field,
                longname,
            )
            .unwrap();
            proplen += 1;
        }
        for field in fields {
            if let ast::Array::None = field.array {
            } else {
                continue;
            }
            if field.tags.contains("mut") {
                write!(self.f, "        {{ \"{f}\", 0, 0, jsGet_{s}_{f}, jsSet_{s}_{f}, 0, napi_default, 0}},\n",
                       f = field.name,
                       s = longname,

                ).unwrap();
            } else {
                write!(
                    self.f,
                    "        {{ \"{f}\", 0, 0, jsGet_{s}_{f}, 0, 0, napi_default, 0}},\n",
                    f = field.name,
                    s = longname,
                )
                .unwrap();
            }
            proplen += 1;
        }

        write!(self.f, "    }};\n").unwrap();
        write!(self.f, "    napi_value cc;\n").unwrap();
        write!(self.f, "    napi_status status = napi_define_class(env, \"{}\", NAPI_AUTO_LENGTH, js_new_{}, 0, {}, properties, &cc);\n",
               shortname,
               longname,
               proplen,
        ).unwrap();

        write!(self.f, "    assert(status == napi_ok);\n").unwrap();
        write!(
            self.f,
            "    status = napi_set_named_property(env, exports, \"{}\", cc);\n",
            shortname
        )
        .unwrap();
        write!(self.f, "    assert(status == napi_ok);\n").unwrap();
        write!(self.f, "}}\n").unwrap();

        self.register_structs
            .push(format!("js_register_{}", longname));
    }

    pub fn emit_closure(&mut self, ast: &ast::Local) {
        let (_ret, _args, _attr) = match &ast.def {
            ast::Def::Closure {
                ret,
                args,
                attr,
                ..
            } => (ret, args, attr),
            _ => unreachable!(),
        };
        self.emit_loc(&ast.loc);
    }

    pub fn emit_fndecl(&mut self, ast: &ast::Local) {
        let (ret, args, body, _vararg, _attr) = match &ast.def {
            ast::Def::Function {
                ret,
                args,
                body,
                vararg,
                attr,
                ..
            } => (ret, args, body, *vararg, attr),
            _ => unreachable!(),
        };

        let mut has_default = false;
        for (loc, expr, body) in &body.branches {
            if let Some(expr) = &expr {
            } else {
                has_default = true;
            }
        }

        if !has_default {
            return;
        }

        write!(
            self.f,
            "napi_value js_{}(napi_env env, napi_callback_info info);\n",
            self.to_local_name(&Name::from(&ast.name))
        )
        .unwrap();

        let shortname = Name::from(&ast.name).0.last().unwrap().clone();
        let longname = self.to_local_name(&Name::from(&ast.name));
        self.register_fns.insert(longname.clone(), format!("js_{}", shortname));
    }

    pub fn emit_fn(&mut self, ast: &ast::Local) {

        let longname = self.to_local_name(&Name::from(&ast.name));
        if !self.register_fns.contains_key(&longname) {
            return;
        }

        let (ret, args, _body, _vararg, _attr) = match &ast.def {
            ast::Def::Function {
                ret,
                args,
                body,
                vararg,
                attr,
                ..
            } => (ret, args, body, *vararg, attr),
            _ => unreachable!(),
        };

        let shortname = Name::from(&ast.name).0.last().unwrap().clone();
        let longname = self.to_local_name(&Name::from(&ast.name));

        write!(
            self.f,
            r#"

napi_value js_{}(napi_env env, napi_callback_info info) {{
    napi_status status;

    size_t argc = 16;
    napi_value argv[16];
    napi_value jsthis;
    status = napi_get_cb_info(env, info, &argc, argv, &jsthis, 0);
    assert(argc < 16);
    assert(status == napi_ok);


"#,
            longname
        )
        .unwrap();

        let mut jarg = 0;
        let mut cargs = String::new();
        for (i, arg) in args.iter().enumerate() {
            if i != 0 {
                cargs.push(',');
            }

            if let Some(_) = arg.tags.get("tail") {
                cargs = format!("{} local_{}_tail", cargs, i - 1);
            } else {
                if arg.name == "self" {
                    write!(
                        self.f,
                        r#"
                    void * thismem;
                    status = napi_unwrap(env, jsthis, &thismem);
                    assert(status == napi_ok);
                    size_t local_{}_tail = (*(size_t*)thismem);
                    void * local_{} = thismem + sizeof(size_t);

                "#,
                        i, i
                    )
                    .unwrap();
                } else {
                    write!(
                        self.f,
                        r#"
                    if ({} >= argc) {{
                        napi_throw_error(env, 0, "call argument count mismatch");
                        return 0;
                    }}
                "#,
                        jarg
                    )
                    .unwrap();
                    write!(
                        self.f,
                        "\n    {} {} local_{};\n",
                        self.to_local_typed_name(&arg.typed),
                        "*".repeat(arg.typed.ptr.len()),
                        i
                    )
                    .unwrap();
                    self.from_js_value(
                        &format!("argv[{}]", jarg),
                        &format!("local_{}", i),
                        &arg.typed,
                    );
                    write!(
                        self.f,
                        r#"
                    if (status != napi_ok) {{
                        napi_throw_type_error(env, 0, "{}'th arg requires type {}");
                        return 0;
                    }}
                "#,
                        jarg + 1,
                        arg.typed
                    )
                    .unwrap();

                    jarg += 1;
                }

                cargs = format!("{} local_{}", cargs, i);
            }
        }

        write!(self.f, "    napi_value jsreturn = 0;\n").unwrap();

        if let Some(ret) = ret {
            write!(
                self.f,
                "    {} {} frrr = {}({});\n",
                self.to_local_typed_name(&ret.typed),
                "*".repeat(ret.typed.ptr.len()),
                longname,
                cargs,
            )
            .unwrap();
        } else {
            write!(self.f, "    {}({});\n", longname, cargs).unwrap();
        }

        if let Some(ret) = ret {
            self.create_js_value("frrr", "jsreturn", &ret.typed);
            write!(self.f, "    assert(status == napi_ok);\n").unwrap();
        }
        write!(self.f, "    return jsreturn;\n").unwrap();
        write!(self.f, "}}").unwrap();
    }

    fn write_escaped_literal(&mut self, c: u8, isstr: bool) {
        let c = c as char;
        match c {
            '"' if isstr => {
                write!(self.f, "\\\"").unwrap();
            }
            '\'' if !isstr => {
                write!(self.f, "\\'").unwrap();
            }
            '\\' => {
                write!(self.f, "\\\\").unwrap();
            }
            '\t' => {
                write!(self.f, "\\t").unwrap();
            }
            '\r' => {
                write!(self.f, "\\r").unwrap();
            }
            '\n' => {
                write!(self.f, "\\n").unwrap();
            }
            _ if c.is_ascii() && !c.is_ascii_control() => {
                write!(self.f, "{}", c).unwrap();
            }
            _ => {
                if isstr {
                    write!(self.f, "\"\"\\x{:x}\"\"", c as u8).unwrap();
                } else {
                    write!(self.f, "\\x{:x}", c as u8).unwrap();
                }
            }
        }
    }
}

