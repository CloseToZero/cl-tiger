# A compiler of the Tiger programming language implemented in Common Lisp

The Tiger programming language is from the book: *Modern Compiler Implementation in ML*.

# Supported platforms

x86-64 architecture on Windows, Linux and Mac.

# How to compile and build a tiger program

We compile a tiger source to an assembly file `tiger.asm/tiger.s`,
then generate a CMake project including the assembly file `tiger.asm/tiger.s`
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

In the first time, we haven't download the dependencies of `cl-tiger`
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
