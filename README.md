![logo](logo.png?raw=true)

Drunk Octopus - It compiles, ship it
====================================

ZZ (drunk octopus) is a dialect of C, inspired by rust

It's main use case is embedded systems, where we still program C out of desperation, because nothing else actually works.
You can also use it to build cross platform libraries. The build and linking concept is biased towards exporting a clean api.

[![Build Status](https://travis-ci.org/aep/zz.svg?branch=master)](https://travis-ci.org/aep/zz)

### too long didn't try

```bash
cd examples/hello
cargo run run
```


### features

 - make C less painful
 - automatic headerfile generation
 - automatic declaration ordering
 - outputs to standard C with useful symbol names
 - namespaces
 - building
 - testing
 - compile time assertions

### maybe-later-features

 - advanced type system
 - procedural macros

### never-features

 - stdlib: ZZ is a C dialect, so libc works fine. If in doubt use musl. For additional functionality, we have modules.
 - smart pointers and runtime checks: ZZ is C. C is terrible but we would not be here if anyone knew how to make smart pointers free.
 - emit binary directly: lots of work for no gain, reduces portability and ignores all the work that went into optimizing clang/gcc/etc.


### no headers

it'll generate headers to make sure its still C and works with C code, but you'll never have to write headers again.

### modules/namespaces

alot of ZZ is inspired by rust, so it just copies the exact same namespacing concept from rust,
except all symbols are C standard, without mangling.

an export fn foo in module bar will be exported as C symbol project_bar_foo.

### default build system

all build systems for C are garbage, because there are no conventions, and people want weird features.
ZZ will never do things like find local libraries from your linux distro.
If you want something like that, you're probably not working on embedded systems anyway and are better served just using a higher level language.


### how it looks


```C++
using errors;
using libc::stdint::{uint32_t as u32};

struct Beep {
    u32 a;
}

fn some_helper(errors::error* err, uint32_t mut* bob) -> uint32_t {
    printf("lol\n");
    if (bob) {
        *bob = add(horst(), foo);
        printf("bob %d\n", *bob);
    }
    return 32;
}

export const fn horst() -> uint32_t {
    return 3;
}

```
By default, arguments are const, unless declared mutable.

note how horst() has been declared later than its use. Declaration order does no longer matter.

### language reference

#### top level declarations: fn, struct

fn delares a function.
struct declares a struct.
nothing fancy here.

#### macro

macro definitions are almost like C's #define but slightly less ugly

```C
macro CHECK(e) {
    if ((e) != 0) {
        printf("oh noes!\n");
    }
}
```

any C syntax is allowed within the body, without having to do the ugly newline escape.
But unlike C, a macro must close all brackets it opens.
this intentionally disables some rather insane use cases.

#### storage: const, static, atomic and thread_local

const and static work exactly like in rust, but with C syntax.

```C
export const uint32_t foo = 3;
static mutable float blarg = 2.0/0.3;
thread_local mutable bool bob = true;
atomic mutable int marvin = 0;
```

const is inlined in each module and therefor points to different memory in each module.
static has a global storage location, but is private to the current module.

in effect, there is no way to have declare a shared global writable variable.
ZZ has no borrowchecker, and the restriction has nothing to do with preventing multithread races.
Instead the declarations are selected so that the resulting exported binary interface can be mapped to any other language.

if you need to export a global writeable memory location (which is still a bad idea, because threads),
you can define a function that returns a pointer to the local static.

thread_local and atomic are mapped directly to the C11 keywords.
ZZ can use nicer keywords because there are no user defined names at the top level.

#### visibility: pub, export

by default all declarations are private to a module

"export" can be used to make sure the declaration ends in the final result. that is in the binary and the export header.

"pub" marks a declaration as local to the project. it is usable in other zz modules, but not exported into the resulting binary


#### mutability: const, mut

by default, everything is const. this is the opposite of C. the mut keyword is used to make a global variable, or function argument mutable.


#### struct initialization


identical to C with the minor difference that you have to explicitly state the type, like rust.
To prepare for type elision, all expressions have to have a known type.

```C
struct A {
    int a;
    int b;
}

fn main() {
    A a = A{
        .a = 2,
    };
}
```

#### conditional compilation / preprocessor

Like in rust, the prepro is not a string processor, but rather executed on the AST  **after** parsing.
This makes it behave very different than C, even if the syntax is the same as C.

The right hand side of #if is evaluated immediately and can only access preprocessor scope.

```C
struct A {
    int a;
#if def("TEST")
    uint proc;
#elif def("MAYBE")
    int proc;
#else
    void* proc;
#endif
}
```

Every branch of an #if / #else must contain a completed statement,
and can only appear where a statment would be valid,
so this is not possible:

```C
pub fn foo(
#if os("unix")
)
#endif
```

note that even code that is disabled by conditions must still be valid syntax. It can however not be type checked,

#### a note on west-const vs east-const

ZZ enforces east-const. C is not a formally correct language, so in order to make ZZ formally correct, we have to make some syntax illegal.
In this case we sacrifice west-const, which is incosistent and difficult to comprehend anyway.

west-const with left aligned star reads as if the pointer is part of the type, and mutability is a property of the pointer (which it is).

```C++
    int mut* foo;
    foo = 0; // compile error
    *foo = 0 // valid
```

unless you want to apply mutability to the local storage named foo

```C++
    void * mut foo;
    foo = 0; // valid
    *foo = 0 // compile error
```

Coincidentally this is roughly equivalent to Rust, so rust devs should feel right at home.
Even if not, you will quickly learn how pointer tags works by following the compiler errors.


#### annotations

ZZ has type annotations.
They are key-value pairs (with default empty value) that are attached to types and local names used throughout compilation and type checking,
but they will never be emitted into the C interface.

examples of built-in predicares are "mutable", "initialized" and "safe"

A variable that is not marked as "initialized", cannot be used in most contexts, except as left hand side in an assignment.

A pointer that is not safe, cannot be dereferenced.

Because automatic annotation is still in early stage, you will do manual annotation alot using the 'is' keyword.

```C
fn bla(A mut* a) {
    if (a != 0) {
        a is safe;
        a->a = 3;
    }
}
```

#### typestate

we can use annotations to define states for types, which neatly lets you define which calls are legal on which
type at a given time in the program without ANY runtime code.


```C++
fn open(int set<open> mut* a) {
    *a = 1;
}

fn read(int require<open> mut* a) -> int {
    return *a;
}

fn close(int unset<open> mut* a) {
    *a = 0;
}
```

the above example defines a type state transition that is legal: open -> read -> close
any other combination will lead to a compile error, such as read before open.
Also not calling close while dropping a will lead to the lifetime checker emitting a warning.

