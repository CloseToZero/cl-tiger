# A compiler of the Tiger programming language implemented in Common Lisp

The Tiger programming language is from the book: *Modern Compiler Implementation in ML*.

# Support platforms

x86-64 architecture on Windows, Linux and Mac.

# How to compile and build a tiger program

We compile a tiger source to an assembly file ``tiger.asm/tiger.s``,
then generate a CMake project including the assembly file ``tiger.asm/tiger.s``
and the runtime C source `runtime.c`, then, user can use CMake to generate a native
project and compile the native project to an executable.

First, we compile a tiger source and generate a CMake project:

```common-lisp
(ql:quickload :cl-tiger)
(cl-tiger:compile-tiger
  "path/to/tiger-source.tig"
  "path/to/output-project-dir" ; Parent directories will be created as needed.
  ;; Choose one of platforms.
  (cl-tiger/target:target cl-tiger/target:arch-x86-64 cl-tiger/target:os-windows)
  ;; (cl-tiger/target:target cl-tiger/target:arch-x86-64 cl-tiger/target:os-linux)
  ;; (cl-tiger/target:target cl-tiger/target:arch-x86-64 cl-tiger/target:os-mac)
  )
```

Next, build the generated CMake project.
Open a terminal/cmd, then execute the following commands:

```sh
cd path/to/output-project-dir
mkdir build
cd build
cmake -S .. -B .
cmake --build .
```

After ``cmake --build .``,
the compiled executable should be generated,
you should be able to run it and see its output.
