(defsystem "cl-tiger"
  :description "The Tiger Programming Language (from Modern Compiler Implementation in ML) implemented in Common Lisp."
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
               (:file "ast")
               (:file "parse")
               (:file "types")
               (:file "env")
               (:file "type-check")
               (:file "temp")))
