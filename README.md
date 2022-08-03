
# beardbolt

An experimental fork of [rmsbolt](https://gitlab.com/jgkamat/rmsbolt),
itself a supercharged implementation of [godbolt
compiler-explorer](https://github.com/mattgodbolt/compiler-explorer)
for Emacs.

beardbolt tries to make it easy to see what your compiler is doing.
It does this by showing you the assembly output of a given source code
file.  It also highlights which source code a given assembly block
corresponds to, and vice versa.

### Why rmsbolt over beardbolt

- Supports more languages/compilers. beardbolt only C++/C clang/gcc.
- Has good documentation and a proper API.
- Supports more Emacs versions.  beardbolt only 28+
- Support compile-commands.json

### Why beardbolt over rmsbolt

- Doesn't require file to be saved.
- Faster (2x) and more responsive (TODO: show benchmarks)
- Less buggy (TODO: show actual rmsbolt problems)
- Has rainbows.
- Simpler code (half the LOC)

### Installation

```sh
cd /path/to/beardbolt-clone
make
```

```lisp
(add-to-list 'load-path "/path/to/beardbolt-clone")
(require 'beardbolt)
```

```
M-x beardbolt-starter
```
