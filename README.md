![logo](logo.png?raw=true)

Drunk Octopus
==============


ZZ (drunk octopus) is a precompiler for C that deals with some of the pain that is C.

It's main use case is embedded systems, where we still program C out of desperation, because nothing else actually works.


### too long didn't try

```bash
cd example
cargo run
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
        *bob = add(1, 2);
        printf("bob %d\n", *bob);
    }
    return 32;
}

```

it's inspired by rust, but keeps C's argument declaration order, because it would be confusing if the body still uses C style. 

By default, arguments are const, unless declared mutable. You can declare them const, but it does nothing.

note how bob is declared later. declaration order doesn't matter.






