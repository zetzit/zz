Drunk Octopus
==============



ZZ (drunk octopus) is a precompiler for C that deals with some of the pain that is C.
It's main use case is embedded, where we still program C out of desperation, because nothing else actually works.

features:

1. minimally invasive.

ZZ is C, and it only deviates from C when useful. sure, C syntax is shit,
but the solution is not to make up another shit syntax that you'd have to learn for no reason.

2. it'll compile on avr8

that's the lowest spec piece of garbage MCU that i can think of. It'll work there.

3. no headers

it'll generate headers to make sure its still C and works with C code, but you'll never have to write headers again.
basically a ZZ file is ZZ syntax at the top level and just plain C in the function bodies.
the compiler does all the dull work with the function definitions, and lets you write plain C otherwise.

4. modules/namespaces

alot of ZZ is inspired by rust, so it just copies the exact same namespacing concept from rust.

5. default build system

all build systems for C are garbage, because there are no conventions, and people want weird features.
ZZ doesn't have any features. It builds a binary for a target from ZZ source files.
If you want something else, you're probably not working on embedded, so why are you here?


here's how ZZ looks:


```C
fn get_stuffs(mutable path::path_builder* pb, const char* bob) -> int {
    // just plain C here
    bob();
}

fn bob() {
}

```

it's inspired by rust, but keeps C's argument declaration order, because it would be confusing if the body still uses C style.
By default, arguments are const, unless declared mutable. You can declare them const, but it does nothing.
note how bob is declared later. declaration order doesn't matter.




WHY was it changed
=========================


1. function declaration

int bob() was changed for fn bob() -> int

it serves as a reminder that this is a ZZ defintion which has stricter rules.
It also makes parsing easier, because parsing C is hard, and i can't be bothered to write a real C parser.


