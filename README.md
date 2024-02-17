# A compiler of the Tiger programming language implemented in Common Lisp

The Tiger programming language is from the book: *Modern Compiler Implementation in ML*.

# Support platforms

Right now, only supports x86-64 architecture on Windows platform,
in the future, we should also Linux and Mac platforms (only x86-64 architecture).

Things need to be fixed/done to support Linux and Mac platforms:

- We only support [x64 calling convention of Microsoft][https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention].
  Is this calling convention also applied to other platforms (mandated by x86-64 architecture)?
  Or what calling conventions other platforms use.
- We only generate assembly instructions in MASM syntax which doesn't supported on other platforms.
