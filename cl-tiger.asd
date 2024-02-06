(defsystem "cl-tiger"
  :description "A compiler of the the Tiger Programming Language (from Modern Compiler Implementation in ML) implemented in Common Lisp."
  :version "0.0.1"
  :author "Zhexuan Chen <2915234902@qq.com>"
  :licence "MIT"
  :depends-on ("trivial-package-local-nicknames"
               "alexandria"
               "serapeum"
               "trivia"
               "esrap"
               "cl-data-structures")
  :serial t
  :components ((:file "straight-line")
               (:file "symbol")
               (:file "utils")
               (:file "temp")
               (:file "target")
               (:file "ir")
               (:file "frame")
               (:file "x86-64-frame")
               (:file "ast")
               (:file "parse")
               (:file "types")
               (:file "type-check")
               (:file "translate")
               (:file "normalize")))
