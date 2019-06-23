![logo](logo.png?raw=true)

Drunk Octopus
==============


ZZ (drunk octopus) is a precompiler for C that deals with some of the pain that is C.

It's main use case is embedded systems, where we still program C out of desperation, because nothing else actually works.

You can also use it to build cross platform libraries. The build and linking concept is tailed to export a clean api with a namespace.


### too long didn't try

```bash
cd example
cargo run && ./target/exe
```


### minimally invasive.


ZZ is C, and it only deviates from C when useful. sure, C syntax is shit,
but the solution is not to make up another shit syntax that you'd have to learn for no reason.

### no headers

it'll generate headers to make sure its still C and works with C code, but you'll never have to write headers again.
basically a ZZ file is ZZ syntax at the top level and just plain C in the function bodies.
the compiler does all the dull work with the function definitions, and lets you write plain C otherwise.

### modules/namespaces

alot of ZZ is inspired by rust, so it just copies the exact same namespacing concept from rust.

a function named "bob" in the file "foo.zz" in the project "yo" will be exported as C symbol "yo_foo_bob",
but an import "yo::foo::bob" will bring it back in scope as just "bob"

### default build system

all build systems for C are garbage, because there are no conventions, and people want weird features.
ZZ doesn't have any features. It builds a binary for a target from ZZ source files.
If you want something else, you're probably not working on embedded, so why are you here?


### how it looks


```C
import errors::error;
import math::add;

fn some_helper(mutable error* err, mutable uint32_t* bob) -> uint32_t {
    printf("lol\n");
    if (bob) {
        *bob = add(horst(), foo);
        printf("bob %d\n", *bob);
    }
    return 32;
}


export fn horst() -> uint32_t {
    return 3;
}

```

it's inspired by rust, but keeps C's argument declaration order, because it would be confusing if the body still uses C style. 

By default, arguments are const, unless declared mutable. You can declare them const, but it does nothing.

note how horst() has been declared later than its use. Declaration order does no longer matter.

### language reference

#### top level declarations: fn, struct

fn delares a function.
the body is passed verbatim to the C compiler.

struct does the same for structs.

#### macro

macro definitions are almost like C's #define but slightly more stable

```C
macro check_noise_error(e) {
    if ((e) != 0) {
        printf("oh noes!\n");
    }
}
```

the macro can be multiple lines but is resctricted within the body {  }.
this intentionally disables some insane use cases.

#### storage: const, static, atomic and thread_local

const and static work exactly like in rust, but with C syntax.

```
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

#### visibility: private, export

by default all declarations are visible to the entire module, but not exported in the final binary.

"export" can be used to make sure the declaration ends in the final result. that is in the binary and the export header.

"private" markes a declaration as local to the current module/file. It cannot be used inside other modules and
as such the compiler may optimize it.
In most cases this is equivalent to the C 'static' keyword, but can be added to any declaration.


#### mutability: const, mutable

by default, everything is const. this is the opposite of C. the mutable keyword is used to make a global variable, or function argument mutable.










