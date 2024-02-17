(cl:defpackage :cl-tiger/x86-64-build
  (:use :cl)
  (:local-nicknames
   (:utils :cl-tiger/utils)
   (:temp :cl-tiger/temp)
   (:types :cl-tiger/types)
   (:target :cl-tiger/target)
   (:frame :cl-tiger/frame)
   (:asm :cl-tiger/asm)
   (:runtime :cl-tiger/runtime)
   (:build :cl-tiger/build)))

(cl:in-package :cl-tiger/x86-64-build)

(defmethod build:build% (frag-strs frag-funs dst-dir
                         target
                         (target-arch target:arch-x86-64) (target-os target:os-windows))
  (let* ((dst-dir-pathname (uiop:ensure-pathname
                            (if (stringp dst-dir)
                                (parse-namestring dst-dir)
                                dst-dir)
                            :want-pathname t
                            :ensure-directory t
                            :ensure-directories-exist t))
         (project-name (first (last (pathname-directory dst-dir-pathname)))))
    (create-cmake-project dst-dir-pathname project-name frag-strs frag-funs target)))

(defun create-cmake-project (dst-dir-pathname project-name frag-strs frag-funs target)
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
      (format out ".code~%~%")

      (format out "public tiger_main~%")
      (dolist (runtime-function runtime:*runtime-functions*)
        (format out "extern ~A :proc~%" (frame:external-call-label-name runtime-function target)))
      (dolist (binding types:*built-in-function-bindings*)
        (format out "extern ~A :proc~%" (frame:external-call-label-name (first binding) target)))
      (format out "~%")

      (dolist (frag-str frag-strs)
        (format out "~A~%" (build:frag-str-def frag-str)))
      (format out "~%")

      (dolist (frag-fun frag-funs)
        (trivia:let-match1 (build:frag-fun prolog body-instrs epilog) frag-fun
          (dolist (instr prolog)
            (format out "~A~%" instr))
          (dolist (instr body-instrs)
            (format out "~A~%" (asm:format-instr instr)))
          (dolist (instr epilog)
            (format out "~A~%" instr)))
        (format out "~%"))

      (format out "end"))))
