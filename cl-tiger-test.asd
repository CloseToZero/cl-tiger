(defsystem "cl-tiger-test"
  :description "Tests of cl-tiger."
  :author "Zhexuan Chen <2915234902@qq.com>"
  :licence "MIT"
  :depends-on ("cl-tiger"
               "trivial-package-local-nicknames"
               "uiop"
               "alexandria"
               "cl-ppcre"
               "parachute"
               "trivial-features")
  :serial t
  :components ((:module "test"
                :components ((:file "test"))))
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :cl-tiger/test)))
