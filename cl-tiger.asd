(defsystem "cl-tiger"
  :description "A compiler of the the Tiger Programming Language (from Modern Compiler Implementation in ML) implemented in Common Lisp."
  :version "0.0.1"
  :author "Zhexuan Chen <2915234902@qq.com>"
  :licence "MIT"
  :depends-on ("cl-ppcre"
               "trivial-package-local-nicknames"
               "alexandria"
               "serapeum"
               "trivia"
               "esrap"
               "fset"
               "uiop"
               "trivial-utf-8"
               "queues.simple-queue"
               "queues.priority-queue")
  :serial t
  :components ((:module "src"
                :components ((:file "straight-line")
                             (:file "symbol")
                             (:file "utils")
                             (:file "temp")
                             (:file "target")
                             (:file "ir")
                             (:file "instr")
                             (:file "frame")
                             (:file "frame-x86-64")
                             (:file "ast")
                             (:file "parse")
                             (:file "types")
                             (:file "type-check")
                             (:file "translate")
                             (:file "normalize")
                             (:file "instr-select")
                             (:file "instr-select-x86-64")
                             (:file "graph")
                             (:file "flow-graph")
                             (:file "liveness")
                             (:file "reg-alloc")
                             (:file "runtime")
                             (:file "build")
                             (:file "build-x86-64")
                             (:file "cl-tiger"))))
  :in-order-to ((asdf:test-op (asdf:test-op "cl-tiger-test"))))
