# A compiler of the Tiger programming language implemented in Common Lisp

The Tiger programming language is from the book: *Modern Compiler Implementation in ML*.

# Supported platforms

x86-64 architecture on Windows, Linux and Mac.

# Things implemented

- Straight-line program interpreter (Chapter 1 of the book): see `straight-line.lisp`.
- Lexical analysis and parsing: see `parse.lisp`,
  I combine both lexical analysis and parsing using [esrap](https://github.com/scymtym/esrap)
  and use [a trick](https://github.com/scymtym/esrap/issues/10) of the author to ignore
  whitespace where appropriate.
- Translate into AST during parsing: see `parse.lisp` and `ast.lisp`.
- Type checking: see `type-check.lisp`.
- Frame/Activation record abstraction and its implementation on x86-64:
  see `frame.lisp` and `frame-x86-64.lisp`.
- Translate into IR: see `translate.lisp` and `ir.lisp`.
- Normalize/Canonize IRs: see `normalize.lisp`.
- Split IRs into basic blocks: see `normalize.lisp`.
- Trace schedule basic blocks: see `normalize.lisp`.
- Abstraction of machine instruction: see `instr.lisp`.
- Instruction selection interface and its implementation on x86-64:
  see `instr-select.lisp` and `instr-select-x86-64.lisp`.
- Flow graph construction: see `flow-graph.lisp`.
- Liveness analysis: see `liveness.lisp`.
- Register allocation with spilling and coalescing: see `reg-alloc.lisp`.
- A C runtime: see `runtime.c`.
- Assemble the generated ASM labels, definitions and instructions etc into
  a valid assembly source file, combine the assembly source file with the runtime C source
  to generate a CMake project, then user can use CMake to generate a native
  project and compile the native project to an executable: see `build.lisp` and `build-x86-64.lisp`.

# TODOs

- Garbage Collection

# How to compile and build a tiger source

We compile a tiger source to an assembly file `tiger.asm/tiger.s`,
then generate a CMake project including the assembly file `tiger.asm/tiger.s`
and the runtime C source `runtime.c`, then, user can use CMake to generate a native
project and compile the native project to an executable.

First, we compile a tiger source and generate a CMake project:

```common-lisp
(ql:quickload :cl-tiger)
(cl-tiger:compile-tiger
  #p"path/to/tiger-source.tig"
  #p"path/to/output-project-dir" ; Parent directories will be created as needed.
  ;; Choose one of platforms.
  (cl-tiger/target:target cl-tiger/target:arch-x86-64 cl-tiger/target:os-windows)
  ;; (cl-tiger/target:target cl-tiger/target:arch-x86-64 cl-tiger/target:os-linux)
  ;; (cl-tiger/target:target cl-tiger/target:arch-x86-64 cl-tiger/target:os-mac)
  )
```

**Note:** Right now, the name of the output project directory (the
placeholder `output-project-dir` in the above) should not include spaces,
because we simply use the output project directory name as the name of CMake target,
something like "my-project", "my_project", "MyProject" are fine.

Next, build the generated CMake project.
Open a terminal/cmd, then execute the following commands:

```sh
cd path/to/output-project-dir
mkdir build
cd build
cmake -S .. -B .
cmake --build .
```

After `cmake --build .`,
the compiled executable should be generated,
you should be able to run it and see its output.

# How to test the system

At the first time, we haven't download the dependencies of `cl-tiger`
and `cl-tiger-test` systems, we use `quicklisp` to do that:

```common-lisp
(ql:quickload :cl-tiger-test)
;; We don't need (ql:quickload :cl-tiger) since cl-tiger-test depends on cl-tiger
```

Then, we can test the system using `asdf:test-system`:

```common-lisp
(asdf:test-system :cl-tiger)
```

Next time, we can use `asdf:test-system` directly without
`ql:quickload` since we already download the dependencies of
`cl-tiger` and `cl-tiger-test` systems.

# ABI references

- For Windows, see [x64 calling convention of Microsoft](https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention).
- For Linux and Mac, see [System V AMD64 ABI](https://en.wikipedia.org/wiki/X86_calling_conventions#System_V_AMD64_ABI).
