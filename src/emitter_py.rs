#![allow(unused)]

use super::ast;
use super::flatten;
use super::make;
use super::name::Name;
use super::parser::{self, emit_error};
use super::project::Project;
use std::collections::HashSet;
use std::fs;
use std::io::Write;
use std::path::PathBuf;

pub struct Emitter {
    p: String,
    project_name: String,
    f: fs::File,
    module: flatten::Module,
    cur_loc: Option<ast::Location>,
    register_structs: Vec<(String /* long */, String /*short*/)>,
    register_fns: Vec<(
        String, /*fn*/
        String, /*name*/
        String, /*doc*/
        bool,   /*has args*/
    )>,
    closure_types:   HashSet<String>,
    enum_types:      HashSet<String>,
    struct_types:    HashSet<String>,
}

pub fn make_module(make: &super::make::Make) {
    let pdir_ = format!("target/python/{}/", make.artifact.name);
    let pdir = std::path::Path::new(&pdir_);
    std::fs::create_dir_all(&pdir).unwrap();

    let p = pdir.join("setup.py");
    let mut f = fs::File::create(&p).expect(&format!("cannot create {:?}", p));

    let mut register_modules = Vec::new();

    write!(
        f,
        r#"#!/usr/bin/env python3
# encoding: utf-8

import os.path
root = os.path.abspath(__file__ + '/../../../../')

from distutils.core import setup, Extension

module = Extension('{n}', sources = [
    root + '/target/python/{n}/mod.c',
"#,
        n = make.artifact.name
    )
    .unwrap();

    for step in &make.steps {
        write!(f, "   root + '/{}',\n", step.source.to_string_lossy()).unwrap();

        // for every linked zz file, add the py bridge
        let s = step.source.parent().unwrap();
        if s.file_name().unwrap() == "zz" {
            let s = step.source.file_name().unwrap();
            write!(f, "   root + '/target/python/{}',\n", s.to_string_lossy()).unwrap();
            register_modules.push(step.source.file_stem().unwrap().to_string_lossy());
        }
    }
    write!(
        f,
        r#"
],
    include_dirs = [root + '/target/{stage}/include'],
    extra_compile_args = ["{cflags}"],
)
setup(name='{name}', ext_modules=[module])
"#,
        stage = make.stage,
        name = make.artifact.name,
        cflags = make.cflags.join("\",\"")
    )
    .unwrap();

    let p = pdir.join("mod.c");
    let mut f = fs::File::create(&p).expect(&format!("cannot create {:?}", p));
    write!(
        f,
        r#"
#define PY_SSIZE_T_CLEAN
#include <Python.h>
"#
    )
    .unwrap();

    for m in &register_modules {
        write!(f, "PyObject* py_mod_{}_Init();\n", m).unwrap();
    }

    write!(f, "\nstatic struct PyModuleDef mod_definition = {{PyModuleDef_HEAD_INIT, \"{}\", \"{}\", -1, 0}};\n",
           make.artifact.name, make.artifact.name
          ).unwrap();

    write!(f, "\nPyMODINIT_FUNC PyInit_{}() {{\n", make.artifact.name).unwrap();
    write!(
        f,
        "    PyObject* exports = PyModule_Create(&mod_definition);\n"
    )
    .unwrap();
    for m in &register_modules {
        write!(
            f,
            "PyModule_AddObject(exports, \"{}\", py_mod_{}_Init());\n",
            m, m
        )
        .unwrap();
    }
    write!(f, "    return exports;\n").unwrap();
    write!(f, "}}\n").unwrap();
}

pub fn outname(_project: &Project, stage: &make::Stage, module: &flatten::Module) -> String {
    format!("target/python/{}.c", module.name.0[1..].join("_"))
}

impl Emitter {
    pub fn new(project: &Project, stage: make::Stage, module: flatten::Module) -> Self {
        std::fs::create_dir_all(format!("target/python/")).unwrap();
        let p = outname(project, &stage, &module);
        let f = fs::File::create(&p).expect(&format!("cannot create {}", p));

        Emitter {
            p,
            project_name: project.name.clone(),
            f,
            module,
            cur_loc: None,
            register_structs: Vec::new(),
            register_fns: Vec::new(),
            closure_types: HashSet::new(),
            enum_types: HashSet::new(),
            struct_types: HashSet::new(),
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
            ast::Type::Char => "char".to_string(),
            ast::Type::Void => "void".to_string(),
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
            ast::Type::ILiteral | ast::Type::ULiteral | ast::Type::Elided | ast::Type::New | ast::Type::Typeid => {
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

        write!(self.f, r#"
#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <assert.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "zz/{}/{}.h"

typedef struct
{{
    PyObject_HEAD
    void        *ptr;
    size_t      tail;
    bool        borrowed;
}} pyFATObject;

static inline void * pyFATGetPtr(PyObject * obj , char * expected_type) {{
    if (obj == 0 || strcmp(Py_TYPE(obj)->tp_name,  expected_type) != 0) {{
        PyErr_Format(PyExc_ValueError, "expected %s pointer, got %s", expected_type, Py_TYPE(obj)->tp_name);
        return 0;
    }}
    pyFATObject   * fat = (pyFATObject *)obj;
    return fat->ptr;
}}

"#,
    self.project_name,
    self.module.name.0[1..].join("_")).unwrap();





        for (d, complete) in &module.d {
            match d.def {
                ast::Def::Function { .. } => {
                    self.emit_fndecl(&d);

                }
                ast::Def::Struct { .. } => {
                    self.emit_struct_fwd(&d, None);
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
                }
                ast::Def::Enum { .. } => self.emit_enum(&d),
                ast::Def::Function { .. } => {}
                ast::Def::Closure { .. } => {
                    self.emit_closure(&d);
                }
                ast::Def::Symbol { .. } => {}
                ast::Def::Theory { .. } => {}
                ast::Def::Testcase { .. } => {}
                ast::Def::Include { .. } => {}
                ast::Def::Type { .. } => {}
            }
            write!(self.f, "\n").unwrap();
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

            match d.def {
                ast::Def::Function { .. } => {
                    self.emit_fn(&d);
                }
                _ => (),
            }
        }

        write!(self.f, "\n\nstatic PyMethodDef methods[] = {{\n").unwrap();
        for (n, f, doc, has_args) in self.register_fns {
            if has_args {
                write!(self.f, "{{\"{}\", {}, METH_VARARGS,\"{}\"}},\n", n, f, doc).unwrap();
            } else {
                write!(self.f, "{{\"{}\", {}, METH_NOARGS,\"{}\"}},\n", n, f, doc).unwrap();
            }
        }
        write!(self.f, "{{NULL, NULL, 0, NULL}}\n").unwrap();
        write!(self.f, "}};\n").unwrap();

        write!(self.f, "\nstatic struct PyModuleDef mod_definition = {{PyModuleDef_HEAD_INIT, \"{}\", \"{}\", -1, methods}};\n",
           module.name.0[1..].join("_"), module.name.0[1..].join("::")
    ).unwrap();

        write!(
            self.f,
            "\nPyObject*  py_mod_{}_Init()\n{{\n",
            module.name.0[1..].join("_")
        )
        .unwrap();
        write!(
            self.f,
            "    PyObject* exports = PyModule_Create(&mod_definition);\n"
        )
        .unwrap();
        for (ln, sn) in self.register_structs {
            write!(
                self.f,
                "    if (PyType_Ready(&py_Type_{ln}) < 0) {{ return NULL; }} \n",
                ln = ln
            )
            .unwrap();
            write!(
                self.f,
                "    PyModule_AddObject(exports, \"{sn}\", (PyObject *)&py_Type_{ln});\n",
                sn = sn,
                ln = ln
            )
            .unwrap();
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
            ast::Def::Enum { names, .. } => (names),
            _ => unreachable!(),
        };

        let longname = self.to_local_name(&Name::from(&ast.name));
        self.enum_types.insert(longname);
    }

    pub fn emit_struct_fwd(&mut self, ast: &ast::Local, tail_variant: Option<u64>) {
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

        write!(self.f, "extern PyTypeObject py_Type_{ln};\n", ln = longname).unwrap();

        self.struct_types.insert(longname);
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

        self.register_structs
            .push((longname.clone(), shortname.clone()));

        let mut register_fields = Vec::new();
        for field in fields {

            // cannot access deep pointers
            if field.typed.ptr.len() > 1 {
                continue;
            }
            // TODO cannot access array
            if let ast::Array::None = &field.array {
            } else {
                continue;
            }


            register_fields.push(field.name.clone());

            //  getter
            write!(
                self.f,
                "static PyObject * py_get_{ln}_{fna}(PyObject *pyself, void *closure) {{\n
    {ln} * self = pyFATGetPtr(pyself, \"{ln}\");
    if (self == 0) {{ return 0; }}
        ",
                ln = longname,
                fna = field.name
            )
            .unwrap();


            let field_longname = self.to_local_typed_name(&field.typed);

            match field.typed.t {
                ast::Type::Bool => {
                    write!(
                        self.f,
                        "    return PyBool_FromLong(self->{});\n",
                        field.name
                    )
                    .unwrap();
                }
                ast::Type::U8
                | ast::Type::U16
                | ast::Type::U32
                | ast::Type::U64
                | ast::Type::UInt
                | ast::Type::USize
                | ast::Type::U128 => {
                    write!(
                        self.f,
                        "    return PyLong_FromUnsignedLongLong(self->{});\n",
                        field.name
                    )
                    .unwrap();
                }
                ast::Type::I8
                | ast::Type::I16
                | ast::Type::I32
                | ast::Type::I64
                | ast::Type::I128
                | ast::Type::Int
                | ast::Type::ISize => {
                    write!(
                        self.f,
                        "    return PyLong_FromLongLong(self->{});\n",
                        field.name
                    )
                    .unwrap();
                }
                ast::Type::F32 | ast::Type::F64 => {
                    write!(
                        self.f,
                        "    return PyFloat_FromDouble(self->{});\n",
                        field.name
                    )
                    .unwrap();
                }
                _ if is_cstring(&field.typed) => {
                    write!(
                        self.f,
                        "    if (self->{}== 0) {{ return 0; }} else {{ return PyUnicode_FromString(self->{}); }}\n",
                        field.name, field.name
                    )
                    .unwrap();
                }
                _ if self.closure_types.contains(&field_longname) => {
                    if field.typed.ptr.len() == 0 {
                        write!(
                            self.f,
                            "    return (PyObject*)self->{}.ctx;\n",
                            field.name
                        )
                    } else {
                        write!(
                            self.f,
                            "    return (PyObject*)self->{}->ctx;\n",
                            field.name
                        )
                    }
                    .unwrap();
                }
                _ if self.enum_types.contains(&field_longname) => {
                    write!(
                        self.f,
                        "    return PyLong_FromLongLong(self->{});\n",
                        field.name
                    )
                    .unwrap();
                }
                ast::Type::Other(ref n) if self.struct_types.contains(&field_longname) => {

                    if let ast::Tail::Static(t, _) = field.typed.tail  {
                        let mut baset = field.typed.clone();
                        baset.tail = ast::Tail::Dynamic(None);
                        write!(self.f, "pyFATObject * fat = (pyFATObject *)PyType_GenericAlloc(&py_Type_{ln}, 0);",
                            ln = self.to_local_typed_name(&baset)).unwrap();
                        write!(self.f, "fat->tail = {}\n", t).unwrap();
                    } else {
                        write!(self.f, "pyFATObject * fat = (pyFATObject *)PyType_GenericAlloc(&py_Type_{ln}, 0);",
                            ln = self.to_local_typed_name(&field.typed)).unwrap();
                    }

                    if field.typed.ptr.len() == 0 {
                        //TODO this is scary, there's so many ways this can go wrong
                        write!(self.f, "fat->ptr = &self->{n};\n",n = field.name).unwrap();
                    } else {
                        write!(self.f, "fat->ptr = self->{n};\n", n = field.name).unwrap();
                    }

                    write!(self.f, "    fat->borrowed = true;\n").unwrap();
                    write!(self.f, "    return (PyObject*)fat;\n").unwrap();


                }
                _  => {
                    write!(self.f, "    return NULL;\n").unwrap();
                }
            }
            write!(self.f, "}}\n").unwrap();

            //  setter

            write!(self.f, "static int py_set_{ln}_{fna}(PyObject *pyself, PyObject *value, void *closure) {{\n
    {ln} * self = pyFATGetPtr(pyself, \"{ln}\");
    if (self == 0) {{ return 0; }}
        ",
        ln = longname,
        fna = field.name).unwrap();

            match field.typed.t {
                ast::Type::Bool => {
                    write!(self.f, "    self->{} = PyBool_Check(value);\n", field.name).unwrap();
                }
                ast::Type::U8
                | ast::Type::U16
                | ast::Type::U32
                | ast::Type::U64
                | ast::Type::UInt
                | ast::Type::USize
                | ast::Type::U128 => {
                    write!(
                        self.f,
                        "    self->{} = PyLong_AsUnsignedLongLong(value);\n",
                        field.name
                    )
                    .unwrap();
                }
                ast::Type::I8
                | ast::Type::I16
                | ast::Type::I32
                | ast::Type::I64
                | ast::Type::I128
                | ast::Type::Int
                | ast::Type::ISize => {
                    write!(
                        self.f,
                        "    self->{} = PyLong_AsLongLong(value);\n",
                        field.name
                    )
                    .unwrap();
                }
                ast::Type::F32 | ast::Type::F64 => {
                    write!(
                        self.f,
                        "    self->{} = PyFloat_AsDouble(value);\n",
                        field.name
                    )
                    .unwrap();
                }
                _ if is_cstring(&field.typed) => {

                    //TODO this does an incref, which makes it global lifetime.
                    // it should probably be connected to the lifetime of the self python wrapper
                    // or zetz should have lifetime annotations
                    write!(
                        self.f,
                        "    if(!PyUnicode_Check(value)) {{ return -1; }} Py_INCREF(value); self->{} = PyUnicode_AsUTF8(value);\n",
                        field.name
                    )
                    .unwrap();
                }
                _ if self.closure_types.contains(&self.to_local_typed_name(&field.typed)) => {
                    write!(
                        self.f,
                        "    self->{f} = ({ln}){{ fn: py_CLOSURE_{ln}, ctx: value }};\n",
                        f  = field.name,
                        ln = self.to_local_typed_name(&field.typed)
                    ).unwrap();
                }
                _ if self.enum_types.contains(&self.to_local_typed_name(&field.typed)) => {
                    write!(
                        self.f,
                        "    self->{} = PyLong_AsLongLong(value);\n",
                        field.name
                    )
                    .unwrap();
                }
                _ => {
                    write!(self.f, "    return -1;\n").unwrap();
                }
            }
            write!(self.f, "    return 0;\n").unwrap();
            write!(self.f, "}}\n").unwrap();
        } // for field in fields

        // free

        write!(
            self.f,
            r#"
static void py_free_{ln}(PyObject *pyself)
{{
    {ln} * self = pyFATGetPtr(pyself, "{ln}");
    if (self != 0) {{
        pyFATObject * fat = (pyFATObject *)pyself;
        if (!fat->borrowed) {{
            PyMem_Free(self);
        }}
    }}
    PyMem_Free(pyself);
}}
"#,
            ln = longname
        )
        .unwrap();

        // alloc

        if let ast::Tail::Dynamic(_) = tail {
            write!(
                self.f,
                r#"
static PyObject* py_new_{ln}(PyTypeObject *type, PyObject *args, PyObject *kwds) {{
    int tail_len = 0;
    if (!PyArg_ParseTuple(args, "i", &tail_len)) {{ return NULL; }};

    void *mem = ({ln} *)PyMem_Calloc(1, sizeof_{ln}(tail_len));
    if (mem == 0) {{
        PyErr_SetString(PyExc_ValueError, "calloc failed");
        return 0;
    }}

    pyFATObject * fat = (pyFATObject *)type->tp_alloc(type, 0);
    fat->ptr    = mem;
    fat->tail   = tail_len;
    return (PyObject*)fat;
}}
"#,
                ln = longname
            )
            .unwrap();
        } else {
            write!(
                self.f,
                r#"
static PyObject* py_new_{ln}(PyTypeObject *type, PyObject *args, PyObject *kwds) {{
    void *mem = PyMem_Calloc(1, sizeof_{ln}());
    if (mem == 0) {{
        PyErr_SetString(PyExc_ValueError, "calloc failed");
        return 0;
    }}

    pyFATObject *fat = (pyFATObject *)type->tp_alloc(type, 0);
    fat->ptr    = mem;
    fat->tail   = 0;
    return (PyObject*)fat;
}}
"#,
                ln = longname
            )
            .unwrap();
        }

        // python type declaration

        write!(
            self.f,
            "static PyGetSetDef py_getset_{ln}[]  = {{\n",
            ln = longname
        )
        .unwrap();
        for field in register_fields {
            write!(
                self.f,
                "{{\"{n}\", py_get_{ln}_{n}, py_set_{ln}_{n},NULL,NULL}},\n",
                ln = longname,
                n = field
            )
            .unwrap();
        }
        write!(self.f, "{{NULL, NULL, NULL,NULL,NULL}}\n").unwrap();
        write!(self.f, "}};\n").unwrap();

        write!(
            self.f,
            r#"
PyTypeObject py_Type_{ln}  = {{
    PyVarObject_HEAD_INIT(NULL,0)
    .tp_name        = "{ln}",
    .tp_doc         = "{doc}",
    .tp_basicsize   = sizeof(pyFATObject),
    .tp_flags       = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,
    .tp_new         = py_new_{ln},
    .tp_getset      = py_getset_{ln},
    .tp_dealloc     = py_free_{ln},
}};
"#,
            ln = longname,
            doc = ast.doc.trim().replace("\n", "\\n").to_string(),
        )
        .unwrap();
    }

    pub fn emit_closure(&mut self, ast: &ast::Local) {
        let (ret, args, _attr) = match &ast.def {
            ast::Def::Closure {
                ret,
                args,
                attr,
                ..
            } => (ret, args, attr),
            _ => unreachable!(),
        };
        self.emit_loc(&ast.loc);

        // do not emit functions which return copy or deep pointers
        if let Some(arg) = ret {
            if let ast::Type::Other(_) = &arg.typed.t {
                if arg.typed.ptr.len() != 1 {
                    return;
                }
            }
        }
        // do not emit functions which take copy values or deep pointers
        for arg in args.iter() {
            if let ast::Type::Other(_) = &arg.typed.t {
                if arg.typed.ptr.len() != 1 {
                    return;
                }
            }
        }

        let longname = self.to_local_name(&Name::from(&ast.name));
        self.closure_types.insert(longname.clone());

        match &ret {
            None => write!(self.f, "static void ").unwrap(),
            Some(a) => {
                write!(self.f, "static {} ", self.to_c_typed_name(&a.typed)).unwrap();
                self.emit_pointer(&a.typed.ptr);
            }
        };


        write!(self.f, " py_CLOSURE_{} (", longname).unwrap();

        let mut first = true;
        for (i, arg) in args.iter().enumerate() {
            if first {
                first = false;
            } else {
                write!(self.f, ", ").unwrap();
            }

            write!(self.f, "{}", self.to_local_typed_name(&arg.typed)).unwrap();

            self.emit_pointer(&arg.typed.ptr);
            if !arg.tags.contains_key("mut") {
                write!(self.f, " const ").unwrap();
            }

            write!(self.f, " arg{}", i).unwrap();
        }
        if !first {
            write!(self.f, ", ").unwrap();
        }
        write!(self.f, "void * _ctx").unwrap();
        write!(self.f, ") {{\n").unwrap();


        write!(self.f, "    PyObject *callobject = (PyObject *)_ctx;\n").unwrap();

        let mut pycallfmt   = Vec::new();
        let mut pycallargs  = Vec::new();

        for (i, arg) in args.iter().enumerate() {
            if let Some(_) = arg.tags.get("tail") {
                write!(self.f, "fat{}->tail = arg{};\n", i - 1, i).unwrap();
                continue;
            }
            if arg.typed.ptr.len() == 0 {
                match arg.typed.t {
                    ast::Type::Bool => {
                        write!(self.f, "    int pass_arg{} = arg{};\n", i, i).unwrap();
                        pycallfmt.push("p");
                        pycallargs.push(format!("pass_arg{}", i));
                    }
                    ast::Type::U8 => {
                        write!(self.f, "    uint8_t pass_arg{} = arg{};\n", i, i).unwrap();
                        pycallfmt.push("b");
                        pycallargs.push(format!("pass_arg{}", i));
                    }
                    ast::Type::I8
                    | ast::Type::U16
                    | ast::Type::U32
                    | ast::Type::U64
                    | ast::Type::U128
                    | ast::Type::I16
                    | ast::Type::I32
                    | ast::Type::I64
                    | ast::Type::I128
                    | ast::Type::Int
                    | ast::Type::UInt
                    | ast::Type::ISize
                    | ast::Type::Void
                    | ast::Type::USize => {
                        write!(self.f, "    long long int pass_arg{} = arg{};\n", i, i).unwrap();
                        pycallfmt.push("l");
                        pycallargs.push(format!("pass_arg{}", i));
                    }
                    ast::Type::F32 | ast::Type::F64 => {
                        write!(self.f, "    double pass_arg{} = arg{};\n", i, i).unwrap();
                        pycallfmt.push("d");
                        pycallargs.push(format!("pass_arg{}", i));
                    }
                    ast::Type::Char => {
                        write!(self.f, "    char pass_arg{} = arg{};\n", i,i).unwrap();
                        pycallfmt.push("c");
                        pycallargs.push(format!("pass_arg{}", i));
                    }
                    ast::Type::Other(ref n) => {
                        unreachable!();
                    }
                    ast::Type::ILiteral
                    | ast::Type::ULiteral
                    | ast::Type::Elided
                    | ast::Type::Typeid
                    | ast::Type::New => {
                        parser::emit_error(
                            "ICE: untyped literal ended up in emitter",
                            &[(
                                arg.loc.clone(),
                                format!("this should have been resolved earlier"),
                            )],
                        );
                        std::process::exit(9);
                    }
                }
            } else {
                // pointer arg
                match arg.typed.t {
                    ast::Type::U8 => {
                        write!(self.f, "    int pass_arg{} = arg{};\n", i, i).unwrap();
                        pycallfmt.push("i");
                        pycallargs.push(format!("pass_arg{}", i));
                    }
                    ast::Type::Other(ref n) => {
                        if n.0[1] == "ext"{
                            write!(self.f, "    int pass_arg{} = 0;\n", i).unwrap();
                            pycallfmt.push("i");
                            pycallargs.push(format!("pass_arg{}", i));
                        } else {
                            write!(self.f, "
    pyFATObject * fat{i} = (pyFATObject *)PyType_GenericAlloc(&py_Type_{ln}, 0);
    fat{i}->borrowed = true;
    fat{i}->ptr = arg{i};
                            ", i = i, ln = self.to_local_typed_name(&arg.typed)
                            ).unwrap();
                            pycallfmt.push("O");
                            pycallargs.push(format!("fat{}", i));
                        }
                    }
                    _ => {
                        //TODO
                        write!(self.f, "    int pass_arg{} = 0;\n", i).unwrap();
                        pycallfmt.push("i");
                        pycallargs.push(format!("pass_arg{}", i));
                    }
                }
            }
        }

        write!(self.f, "    PyObject *rrrr =  PyObject_CallFunction(callobject, \"{}\", {}); \n",
            pycallfmt.join(""),
            pycallargs.join(",")
        ).unwrap();

        write!(self.f, "    if (PyErr_Occurred()) {{PyErr_WriteUnraisable(callobject);}} \n",).unwrap();

        match ret {
            None => {}
            Some(arg) => match arg.typed.t {
                ast::Type::Bool => {
                    write!(self.f, "    if(rrrr == 0) {{ return false; }} bool return_val = PyBool_Check(rrrr);\n").unwrap();
                }
                ast::Type::U8
                | ast::Type::U16
                | ast::Type::U32
                | ast::Type::U64
                | ast::Type::UInt
                | ast::Type::USize
                | ast::Type::U128 => {
                    write!(
                            self.f,
                            "    if(rrrr == 0) {{ return 0; }} unsigned long long return_val = PyLong_AsUnsignedLongLong(rrrr);\n",
                            )
                            .unwrap();
                    }
                ast::Type::I8
                | ast::Type::I16
                | ast::Type::I32
                | ast::Type::I64
                | ast::Type::I128
                | ast::Type::Int
                | ast::Type::ISize => {
                        write!(
                            self.f,
                            "    if(rrrr == 0) {{ return 0; }} long long return_val = PyLong_AsLongLong(rrrr);\n",
                            )
                            .unwrap();
                    }
                ast::Type::F32 | ast::Type::F64 => {
                    write!(
                        self.f,
                        "    if(rrrr == 0) {{ return 0; }} double return_val = PyFloat_AsDouble(rrrr);\n",
                        )
                        .unwrap();
                }
                _ => {
                    write!(self.f, "    int return_val = 0;\n").unwrap();
                }
            },
        }
         write!(self.f, "    Py_DECREF(rrrr);\n").unwrap();
        if ret.is_some() {
            write!(self.f, "    return return_val;\n").unwrap();
        }

        write!(self.f, "}} \n").unwrap();
    }

    pub fn emit_fndecl(&mut self, ast: &ast::Local) {
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
    }

    pub fn emit_fn(&mut self, ast: &ast::Local) {
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

        // do not emit functions which return copy or deep pointers
        if let Some(arg) = ret {
            if let ast::Type::Other(ref n) = &arg.typed.t {
                let tname = n.0.last().unwrap();
                if tname != "char" && arg.typed.ptr.len() != 1 {
                    return;
                }
            }
        }
        // do not emit functions which take copy values or deep pointers
        for (i, arg) in args.iter().enumerate() {
            if let ast::Type::Other(ref n) = &arg.typed.t {
                let tname = n.0.last().unwrap();
                if tname != "char" && arg.typed.ptr.len() != 1 {
                    if !self.closure_types.contains(&self.to_local_typed_name(&arg.typed)) {
                        return;
                    }
                }
            }
        }

        let shortname = Name::from(&ast.name).0.last().unwrap().clone();
        let longname = self.to_local_name(&Name::from(&ast.name));
        self.register_fns.push((
            shortname.clone(),
            format!("py_{}", longname),
            ast.doc.trim().replace("\n", "\\n").to_string(),
            args.len() > 0,
        ));

        write!(
            self.f,
            r#"
static PyObject* py_{}(PyObject *pyself, PyObject *args) {{
"#,
            longname
        )
        .unwrap();

        let mut parse_tuple_fmt = Vec::new();
        let mut parse_tuple_args = Vec::new();
        let mut pass_args = Vec::new();

        for (i, arg) in args.iter().enumerate() {
            if let Some(_) = arg.tags.get("tail") {
                pass_args.push(format!("((pyFATObject *)arg{})->tail", i - 1));
                continue;
            }

            write!(self.f, "    //{}\n", arg.name).unwrap();

            if arg.typed.ptr.len() == 0 {
                match arg.typed.t {
                    ast::Type::Bool => {
                        write!(self.f, "    int arg{} = 0;\n", i).unwrap();
                        parse_tuple_fmt.push("p");
                        parse_tuple_args.push(format!("&arg{}", i));
                        pass_args.push(format!("arg{}", i));
                    }
                    ast::Type::U8 => {
                        write!(self.f, "    uint8_t arg{} = 0;\n", i).unwrap();
                        parse_tuple_fmt.push("b");
                        parse_tuple_args.push(format!("&arg{}", i));
                        pass_args.push(format!("arg{}", i));
                    }
                    ast::Type::I8
                    | ast::Type::U16
                    | ast::Type::U32
                    | ast::Type::U64
                    | ast::Type::U128
                    | ast::Type::I16
                    | ast::Type::I32
                    | ast::Type::I64
                    | ast::Type::I128
                    | ast::Type::Int
                    | ast::Type::UInt
                    | ast::Type::ISize
                    | ast::Type::USize
                    | ast::Type::Void => {
                        write!(self.f, "    long long int arg{} = 0;\n", i).unwrap();
                        parse_tuple_fmt.push("l");
                        parse_tuple_args.push(format!("&arg{}", i));
                        pass_args.push(format!("arg{}", i));
                    }
                    ast::Type::F32 | ast::Type::F64 => {
                        write!(self.f, "    double arg{} = 0;\n", i).unwrap();
                        parse_tuple_fmt.push("d");
                        parse_tuple_args.push(format!("&arg{}", i));
                        pass_args.push(format!("arg{}", i));
                    }
                    ast::Type::Char => {
                        write!(self.f, "    char arg{} = 0;\n", i).unwrap();
                        parse_tuple_fmt.push("c");
                        parse_tuple_args.push(format!("&arg{}", i));
                        pass_args.push(format!("arg{}", i));
                    }
                    ast::Type::Other(ref n) => {
                        let tname = n.0.last().unwrap();
                        if self.closure_types.contains(&self.to_local_typed_name(&arg.typed)) {
                            write!(self.f, "    PyObject * arg{} = 0;\n", i).unwrap();
                            parse_tuple_fmt.push("O");
                            parse_tuple_args.push(format!("&arg{}", i));
                            pass_args.push(format!("({ln}){{ fn: py_CLOSURE_{ln}, ctx: arg{i} }} ",
                                ln = self.to_local_typed_name(&arg.typed), i = i));
                        } else {
                            unreachable!();
                        }
                    }
                    ast::Type::ILiteral
                    | ast::Type::ULiteral
                    | ast::Type::Elided
                    | ast::Type::Typeid
                    | ast::Type::New => {
                        parser::emit_error(
                            "ICE: untyped literal ended up in emitter",
                            &[(
                                arg.loc.clone(),
                                format!("this should have been resolved earlier"),
                            )],
                        );
                        std::process::exit(9);
                    }
                }
            } else {
                // pointer arg
                match arg.typed.t {
                    ast::Type::U8 => {
                        write!(self.f, "    uint8_t * arg{} = 0;\n", i).unwrap();
                        write!(self.f, "    Py_ssize_t arg{}_len = 0;\n", i).unwrap();
                        if arg.tags.contains("unsafe") {
                            parse_tuple_fmt.push("z#");
                        } else {
                            parse_tuple_fmt.push("s#");
                        }
                        parse_tuple_args.push(format!("&arg{}", i));
                        parse_tuple_args.push(format!("&arg{}_len", i));
                        pass_args.push(format!("arg{}", i));
                    }
                    ast::Type::Other(ref n) => {
                        let tname = n.0.last().unwrap();
                        if tname == "char" && arg.typed.ptr.len() == 1 {
                            write!(self.f, "    char * arg{} = 0;\n", i).unwrap();
                            write!(self.f, "    Py_ssize_t arg{}_len = 0;\n", i).unwrap();
                            if arg.tags.contains("unsafe") {
                                parse_tuple_fmt.push("z#");
                            } else {
                                parse_tuple_fmt.push("s#");
                            }
                            parse_tuple_args.push(format!("&arg{}", i));
                            parse_tuple_args.push(format!("&arg{}_len", i));
                            pass_args.push(format!("arg{}", i));
                        } else {
                            write!(self.f, "    PyObject * arg{} = 0;\n", i).unwrap();
                            parse_tuple_fmt.push("O");
                            parse_tuple_args.push(format!("&arg{}", i));
                            pass_args.push(format!(
                                "pyFATGetPtr(arg{}, \"{}\")",
                                i,
                                self.to_local_typed_name(&arg.typed)
                            ));
                        }
                    }
                    _ => {
                        //TODO list?
                        write!(self.f, "    PyObject * arg{} = 0;\n", i).unwrap();
                        parse_tuple_fmt.push("O");
                        parse_tuple_args.push(format!("&arg{}", i));
                        pass_args.push(format!(
                            "pyFATGetPtr(arg{}, \"{}\")",
                            i,
                            self.to_local_typed_name(&arg.typed)
                        ));
                    }
                }
            }
        }

        if parse_tuple_args.len() > 0 {
            write!(
                self.f,
                "    if (!PyArg_ParseTuple(args, \"{}\", {})) {{ return NULL; }};\n",
                parse_tuple_fmt.join(""),
                parse_tuple_args.join(",")
            )
            .unwrap();
        }

        let thecall = format!("{}(\n        {})", longname, pass_args.join(",\n        "));

        match ret {
            None => {
                write!(self.f, "    {};\n", thecall).unwrap();
                write!(self.f, "    Py_RETURN_NONE;\n").unwrap();
            }
            Some(arg) => match arg.typed.t {
                ast::Type::Bool => {
                    write!(
                        self.f,
                        "    long long rarg = (long long int)({});\n",
                        thecall,
                    )
                    .unwrap();
                    write!(self.f, "    return PyBool_FromLong(rarg);\n").unwrap();
                }
                ast::Type::U8
                | ast::Type::I8
                | ast::Type::U16
                | ast::Type::U32
                | ast::Type::U64
                | ast::Type::U128
                | ast::Type::I16
                | ast::Type::I32
                | ast::Type::I64
                | ast::Type::I128
                | ast::Type::Int
                | ast::Type::UInt
                | ast::Type::ISize
                | ast::Type::USize => {
                    write!(
                        self.f,
                        "    long long int rarg = (long long int)({});\n",
                        thecall,
                    )
                    .unwrap();
                    write!(self.f, "    return PyLong_FromLong(rarg);\n").unwrap();
                }
                ast::Type::F32 | ast::Type::F64 => {
                    write!(self.f, "    double rarg = (double)({});\n", thecall,).unwrap();
                    write!(self.f, "    return PyLong_FromDouble(rarg);\n").unwrap();
                }
                ast::Type::Char => {
                    if arg.typed.ptr.len() == 0 {
                        write!(
                            self.f,
                            "    long long int rarg = (long long int)({});\n",
                            thecall,
                        )
                        .unwrap();
                        write!(self.f, "    return PyLong_FromLong(rarg);\n").unwrap();
                    } else if arg.typed.ptr.len() == 1 {
                        write!(self.f, "    const char * rarg = {};\n", thecall,).unwrap();
                        write!(self.f, "    return PyUnicode_FromString(rarg);\n").unwrap();
                    }
                }
                ast::Type::Void => {
                    write!(self.f, "    void * rarg = (void*)({});\n", thecall,).unwrap();
                    write!(
                        self.f,
                        "    return PyCapsule_New(rarg, \"{}\", 0);\n",
                        self.to_local_typed_name(&arg.typed)
                        )
                    .unwrap();
                }
                ast::Type::Other(ref n) => {
                    let tname = n.0.last().unwrap();
                    if n == &Name::from("::ext::<Python.h>::PyObject"){
                        write!(self.f, "    return ({});\n", thecall,).unwrap();
                    } else {
                        write!(self.f, "    void * rarg = (void*)({});\n", thecall,).unwrap();
                        write!(
                            self.f,
                            "    return PyCapsule_New(rarg, \"{}\", 0);\n",
                            self.to_local_typed_name(&arg.typed)
                        )
                        .unwrap();
                    }
                }
                ast::Type::ILiteral | ast::Type::ULiteral | ast::Type::Elided | ast::Type::New | ast::Type::Typeid => {
                    parser::emit_error(
                        "ICE: untyped literal ended up in emitter",
                        &[(
                            arg.typed.loc.clone(),
                            format!("this should have been resolved earlier"),
                        )],
                    );
                    std::process::exit(9);
                }
            },
        }
        write!(self.f, "}}\n").unwrap();
    }

    pub fn to_c_typed_name(&self, name: &ast::Typed) -> String {
        match name.t {
            ast::Type::Void => "void".to_string(),
            ast::Type::Char => "char".to_string(),
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
            ast::Type::ILiteral | ast::Type::ULiteral | ast::Type::Elided | ast::Type::New | ast::Type::Typeid => {
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

    pub fn emit_pointer(&mut self, v: &Vec<ast::Pointer>) {
        for ptr in v {
            if !ptr.tags.contains_key("mut") && !ptr.tags.contains_key("mut") {
                write!(self.f, " const ").unwrap();
            }
            write!(self.f, "* ").unwrap();
        }
    }

}

pub fn is_cstring(typed: &ast::Typed) -> bool {
    if let ast::Type::Other(ref n) = &typed.t {
        if let ast::Type::Other(ref n) = &typed.t {
            let tname = n.0.last().unwrap();
            if tname == "char" && typed.ptr.len() == 1 {
                return true;
            }
        }
    }
    return false;
}

