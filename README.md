# debugger-hs

A Haskell library for creating/metaprogramming GDB scripts.

## Why?

GDB has Python integration, and while it is powerful, it has some problems:

- Python code can end up interleaved in the GDB code, making it complicated
  really quickly.
- Accessing variables between GDB and Python can get complicated too.

Besides that, also some things in GDB don't work as expected, such as commands
that behave differently inside a user-defined command ("define"), which can lead
to unexpected and buggy results.

If we use Haskell however, we get the following nice things:

- A typesystem,
- Can use full Haskell ecosystem for metaprogramming,
- The generated output will be 100% GDB script only:
  - easier to use afterwards,
  - inspectable,
  - no need for this package once the script has been generated.

## How?

Partial evaluation/staged programming. You write a Haskell file that makes use
of the DSL this package provides, and render it to a script that can be
passed in to GDB like you would normally.

Here's an example of how you can use this library to generate a GDB script:

```haskell
module Main where

import qualified Debugger.Builder as D
import qualified Debugger.Render as D
import qualified Debugger.Statement as D

-- First we build up a Haskell value that represents our GDB scripts.
script :: D.Builder ()
script = do
  bp <- D.break (D.Function "main")
  D.command bp $ do
    D.print "42"
    D.continue

-- And then we can render the GDB script to a file:
main :: IO ()
main = do
  let gdbScript = D.runBuilder script
  D.renderIO gdbScript "./script.gdb"
```

This will render the following GDB scripts:

```gdb
break main
set $var0 = $bpnum
command $var0
  print "42"
  continue
end
```

```bash
# Replace $PROGRAM with the program you want to debug.
$ gdb $PROGRAM <<< $(< /path/to/stack-script)
# or if your shell doesn't support the previous command:
$ /path/to/stack-script > ./script.gdb
$ gdb $PROGRAM -ex "source ./script.gdb"
```

## TODO

- [x] Create core AST datatype
- [x] Create builder monad for easily constructing GDB scripts using Haskell do-syntax.
- [x] Write function for compiling AST -> GDB script
- [ ] Create CLI application (stack/nix?) that takes the DSL and outputs GDB to stdout.
- [ ] Add helper functions for easily adding breakpoints (using tools like grep)
- [ ] Extend core AST datatype to support more functionality
- [ ] Add LLDB support also?
- [ ] DSL (to support GDB and LLDB at same time)?
