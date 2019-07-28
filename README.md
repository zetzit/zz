![logo](logo.png?raw=true)

Drunk Octopus
==============


ZZ (drunk octopus) is a dialect of C, inspired by rust

It's main use case is embedded systems, where we still program C out of desperation, because nothing else actually works.
You can also use it to build cross platform libraries. The build and linking concept is tailed to export a clean api with a namespace.


### too long didn't try

```bash
cd example
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

## maybe-later-features

 - advanced type system
 - compile time assertions
 - procedural macros
 - monads (sounds worse than it is, trust me)

### never-features

 - stdlib: ZZ is a C dialect, so libc works fine. If in doubt use musl. For additional functionality, we have modules.
 - smart pointers and runtime checks: ZZ is C. C is terrible but we would not be here if anyone knew how to make smart pointers free.
 - borrowchecker: no idea how to implement without changing the language.
 - emit binary directly: lots of work for no gain, reduces portability and ignores all the work that went into optimizing clang/gcc/etc.


### minimally invasive.

ZZ is C, and it only deviates from C when useful. sure, C syntax is shit,
but the solution is not to make up another shit syntax that you'd have to learn for no reason.

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


```C
using errors;
using libc::stdint::{uint32_t as u32};

struct Beep {
    u32 a;
}

fn some_helper(mut errors::error* err, mut uint32_t* bob) -> uint32_t {
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
By default, arguments are const, unless declared mutable. You can declare them const, but it does nothing.

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

#### a note on pointer syntax

in ZZ, the star "\*" is always on the left side, i.e. attached to the type. There cannot be a space between type and star.

```C
    void* foo;
```

the reason is parser complexity. If you hate this, feel free to submit a PR that makes the parser tolerate different styles.


