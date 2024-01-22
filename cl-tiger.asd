(defsystem "cl-tiger"
  :description "The Tiger Programming Language (from Modern Compiler Implementation in ML) implemented in Common Lisp."
  :version "0.0.1"
  :author "Zhexuan Chen <2915234902@qq.com>"
  :licence "MIT"
  :depends-on ("trivia" "esrap" "alexandria")
  :components ((:file "straight-line")
               (:file  "ast")
               (:file "parser" :depends-on ("ast"))))
