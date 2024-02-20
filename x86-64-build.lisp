(cl:defpackage :cl-tiger/x86-64-build
  (:use :cl)
  (:local-nicknames
   (:utils :cl-tiger/utils)
   (:temp :cl-tiger/temp)
   (:types :cl-tiger/types)
   (:target :cl-tiger/target)
   (:frame :cl-tiger/frame)
   (:instr :cl-tiger/instr)
   (:runtime :cl-tiger/runtime)
   (:build :cl-tiger/build)))

(cl:in-package :cl-tiger/x86-64-build)

(defmethod build:build% (frag-strs frag-funs dst-dir
                         target
                         (target-arch target:arch-x86-64) target-os)
  (let* ((dst-dir-pathname (uiop:ensure-pathname
                            (if (stringp dst-dir)
                                (parse-namestring dst-dir)
                                dst-dir)
                            :want-pathname t
                            :ensure-directory t
                            :ensure-directories-exist t))
         (project-name (first (last (pathname-directory dst-dir-pathname)))))
    (create-cmake-project
     dst-dir-pathname project-name frag-strs frag-funs
     target (target:target-arch target) (target:target-os target))))

(defgeneric create-cmake-project (dst-dir-pathname project-name frag-strs frag-funs
                                  target target-arch target-os))

(defmethod create-cmake-project (dst-dir-pathname project-name frag-strs frag-funs
                                 target
                                 (target-arch target:arch-x86-64)
                                 (target-os target:os-windows))
  (let* ((cmakelists-pathname (uiop:merge-pathnames* "CMakeLists.txt" dst-dir-pathname))
         (asm-filename "tiger.asm")
         (asm-pathname (uiop:merge-pathnames* asm-filename dst-dir-pathname))
         (runtime-filename "runtime.c")
         (in-runtime-pathname (asdf:system-relative-pathname  "cl-tiger" runtime-filename))
         (out-runtime-pathname (uiop:merge-pathnames* runtime-filename dst-dir-pathname)))
    (with-open-file (out cmakelists-pathname
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (format out
              "cmake_minimum_required(VERSION 3.14)

project(~A C ASM_MASM)

set(C_STANDARD 99)
set(C_STANDARD_REQUIRED TRUE)

add_executable(~A
  ~A
  ~A
)
"
              project-name
              project-name
              asm-filename
              runtime-filename))
    (uiop:copy-file in-runtime-pathname out-runtime-pathname)
    (with-open-file (out asm-pathname
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)

      (format out ".data~%~%")
      (dolist (frag-str frag-strs)
        (format out "~A~%" (build:frag-str-def frag-str)))
      (format out "~%")

      (format out ".code~%~%")

      (format out "public tiger_main~%")
      (dolist (runtime-function runtime:*runtime-functions*)
        (format out "extern ~A :proc~%" (frame:external-call-label-name runtime-function target)))
      (dolist (binding types:*built-in-function-bindings*)
        (format out "extern ~A :proc~%" (frame:external-call-label-name (first binding) target)))
      (format out "~%")

      (dolist (frag-fun frag-funs)
        (trivia:let-match1 (build:frag-fun prolog body-instrs epilog) frag-fun
          (dolist (instr prolog)
            (format out "~A~%" instr))
          (dolist (instr body-instrs)
            (format out "~A~%" (instr:format-instr instr)))
          (dolist (instr epilog)
            (format out "~A~%" instr)))
        (format out "~%"))

      (format out "end"))))

(defmethod create-cmake-project (dst-dir-pathname project-name frag-strs frag-funs
                                 target
                                 (target-arch target:arch-x86-64)
                                 target-os)
  (let* ((cmakelists-pathname (uiop:merge-pathnames* "CMakeLists.txt" dst-dir-pathname))
         (asm-filename "tiger.s")
         (asm-pathname (uiop:merge-pathnames* asm-filename dst-dir-pathname))
         (runtime-filename "runtime.c")
         (in-runtime-pathname (asdf:system-relative-pathname  "cl-tiger" runtime-filename))
         (out-runtime-pathname (uiop:merge-pathnames* runtime-filename dst-dir-pathname)))
    (with-open-file (out cmakelists-pathname
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (format out
              "cmake_minimum_required(VERSION 3.14)

project(~A C ASM)

set(C_STANDARD 99)
set(C_STANDARD_REQUIRED TRUE)

add_executable(~A
  ~A
  ~A
)
"
              project-name
              project-name
              asm-filename
              runtime-filename))
    (uiop:copy-file in-runtime-pathname out-runtime-pathname)
    (with-open-file (out asm-pathname
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)

      (format out ".data~%~%")
      (dolist (frag-str frag-strs)
        (format out "_~A~%" (build:frag-str-def frag-str)))
      (format out "~%")

      (format out ".text~%~%")

      (format out ".global _tiger_main~%")
      (dolist (runtime-function runtime:*runtime-functions*)
        (format out ".global _~A~%" (frame:external-call-label-name runtime-function target)))
      (dolist (binding types:*built-in-function-bindings*)
        (format out ".global _~A~%" (frame:external-call-label-name (first binding) target)))
      (format out "~%")

      (dolist (frag-fun frag-funs)
        (trivia:let-match1 (build:frag-fun prolog body-instrs epilog) frag-fun
          (dolist (instr prolog)
            (format out "~A~%" instr))
          (dolist (instr body-instrs)
            (format out "~A~%" (instr:format-instr instr)))
          (dolist (instr epilog)
            (format out "~A~%" instr)))
        (format out "~%")))))
