# A compiler of the Tiger programming language implemented in Common Lisp

The Tiger programming language is from the book: *Modern Compiler Implementation in ML*.

# Support platforms

Right now, only supports x86-64 architecture on Windows platform,
in the future, we should also Linux and Mac platforms (only x86-64 architecture).

Things need to be fixed/done to support Linux and Mac platforms:

- We only support [x64 calling convention of Microsoft](https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention).
  Is this calling convention also applied to other platforms (mandated by x86-64 architecture)?
  Or what calling conventions other platforms use.
- We only generate assembly instructions in MASM syntax which doesn't supported on other platforms.

# How to compile and build a tiger program

First, generate CMake project:

```common-lisp
(ql:quickload :cl-tiger)
(cl-tiger:compile-tiger
  "path/to/tiger-source.tig"
  "path/to/output-project-dir" ; Parent directories will be created if needed.
  (cl-tiger/target:target cl-tiger/target:arch-x86-64 cl-tiger/target:os-windows))
```

Next, build the generated CMake project:

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
