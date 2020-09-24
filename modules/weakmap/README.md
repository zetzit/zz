weakmap
=======

> A higher level `Map` abstraction for borrowed references

## Installation

Add this to your `zz.toml` file:

```toml
[dependencies]
weakmap = "1"
```

## Usage

```c++
using weakmap
```

## API

### `WeakMap`

A higher level `Map` abstraction for borrowed references.

### `new[..., +tail] wm = weakmap::make()`

Creates a new `WeakMap` with tail with support for `map::key` and
`map::val` theories aliased to `weakmap::key` and `weakmap::val`
respectively.

### `new[...] wm = weakmap::make_with_pool()`

Creates a new `WeakMap` from a pool with support for `map::key` and
`map::val` theories aliased to `weakmap::key` and `weakmap::val`
respectively.

#### `wm.insert(void *key, void *value) -> bool`

Insert a _borrowed_ `value` for a _borrowed_ `key` into the `WeakMap`.
Both the `key` and `value` need to exist for the lifetime of the
`WeakMap`. This function returns `true` upon success, otherwise `false`
if there is not enough memory to store the key (hash).

#### `wm.set(void *key, void *value) -> bool`

An alias to `wm.insert(key, value)`

#### `wm.get(void *key) -> void mut *`

Return a _borrowed_ reference for `key`. A call site attestation may be
needed to use the return value as it may be unsafe.

#### `wm.remove(void *key) -> bool`

Remove a value entry for a given `key`. Returns `true` upon success,
otherwise `false` if it does not exist.

#### `wm.clear() -> bool`

Clear all references for all keys in the `WeakMap`. Returns `true` upon
success, otherwise `false`.

#### `wm.has(void *key) -> bool`

Returns `true` if there is a value entry for a given `key`, otherwise
`false`.

#### `wm.count() -> usize`

Returns the number of borrowed items the `WeakMap` references.

#### `wm.keys(void *key) -> map::Keys`

Returns an _iterator_ for all keys in the `WeakMap`.

```c++
for let mut it = wm.keys(); it.next(); {
  let key = it.key.mem;
  let value = wm.get(key);
}
```
