# debugger-hs

A Haskell library for creating/metaprogramming GDB scripts.

## Why?

GDB has Python integration, and while it is powerful, it has some problems:

- Python code can end up interleaved in the GDB code, making it complicated
  really quickly.
- No problem accessing variables between GDB and Python.

Besides that, also some things in GDB don't work as expected, such as commands
that behave differently inside a user-defined command ("define"), which can lead
to unexpected and buggy results.

If we use Haskell however, we get the following nice things:

- A typesystem,
- Can use full Haskell ecosystem for metaprogramming,
- The generated output will be 100% GDB script only:
  - easier to use afterwards,
  - inspectable
  - no need for this package once the script has been generated.


## How?

Partial evaluation/staged programming. You write a Haskell file that makes use
of the DSL this package provides, and render it to a script that can be
passed in to GDB like you would normally.

```haskell
TODO: example haskell script + setup commands
```

```bash
# Replace $PROGRAM with the program you want to debug.
$ gdb $PROGRAM <<< $(< /path/to/stack-script)
# or if your shell doesn't support the previous command:
$ /path/to/stack-script > ./script.gdb
$ gdb $PROGRAM -ex "source ./script.gdb"
```


## TODO

- [ ] Create core AST datatype
- [ ] Create builder monad for easily constructing GDB scripts using Haskell do-syntax.
- [ ] Write function for compiling AST -> GDB script
- [ ] Create CLI application (stack/nix?) that takes the DSL and outputs GDB to stdout.
- [ ] Add helper functions for easily adding breakpoints (using tools like grep)
- [ ] Add LLDB support also?

