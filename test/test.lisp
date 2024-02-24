(cl:defpackage :cl-tiger/test
  (:use :cl)
  (:local-nicknames
   (:target :cl-tiger/target)
   (:straight-line :cl-tiger/straight-line)
   (:type-check :cl-tiger/type-check)))

(cl:in-package :cl-tiger/test)

(defvar *target-os*
  #+windows
  target:os-windows
  #+linux
  target:os-linux
  #+darwin
  target:os-mac)

(unless *target-os*
  (error "Unknown target OS."))

(defmacro expect-type-check-errors (errors exhaustive &body body)
  (let ((errors-var (gensym "errors"))
        (error-occurred-table-var (gensym "error-occurred-table"))
        (extra-errors-var (gensym "errors")))
    `(let ((,errors-var ,errors)
           (,error-occurred-table-var (make-hash-table))
           (,extra-errors-var nil))
       (handler-bind ((type-check:type-check-error
                        (lambda (condition)
                          ;; We don't need to generate variable name for the following introduced variable e,
                          ;; since it's not within the scope for user provided form.
                          (alexandria:if-let (e (find-if (lambda (e)
                                                           (typep condition e))
                                                         ,errors-var))
                            (setf (gethash e ,error-occurred-table-var) t)
                            (push condition ,extra-errors-var))
                          (invoke-restart 'type-check:continue-type-check))))
         (prog1 (progn ,@body)
           (unless (every (lambda (e)
                            (gethash e ,error-occurred-table-var))
                          ,errors-var)
             (error "Missing type check errors: ~{~S~^,~}"
                    (remove-if (lambda (e)
                                 (gethash e ,error-occurred-table-var))
                               ,errors-var)))
           (when (and ,exhaustive
                      (not (null ,extra-errors-var)))
             (error "Extra errors: ~{~S~^,~}" (nreverse ,extra-errors-var))))))))

(defun build-dir (project-dir-pathname)
  (uiop:ensure-directory-pathname (merge-pathnames "build" project-dir-pathname)))

(defun run-command (command args target-os &rest run-program-args)
  (let ((full-command
          (serapeum:match-of target:os target-os
            (target:os-windows
             (list "cmd" "/C"
                   (concatenate
                    'string "chcp 65001 &&"
                    command
                    " "
                    (format nil "~{~A~^  ~}" (mapcar #'uiop:escape-windows-token args)))))
            (_ (list* command args)))))
    (apply #'uiop:run-program full-command
           :external-format :utf-8
           run-program-args)))

(defun executable-path (project-dir-pathname target-os)
  (let ((output
          (run-command "cmake"
                       (list "--build" "." "--target" "get_exe_path")
                       target-os
                       :directory (build-dir project-dir-pathname)
                       :output :string
                       :error-output t)))
    (ppcre:register-groups-bind (path)
        ("target executable path = (.+)" output)
      (ppcre:regex-replace-all "[\\r\\n]" path ""))))

(defun build-and-run-project (project-dir-pathname target-os &key input-string)
  (let ((build-dir (build-dir project-dir-pathname)))
    (ensure-directories-exist build-dir)
    (run-command "cmake"
                 (list "-S" ".." "-B" ".")
                 target-os
                 :directory build-dir
                 :external-format :latin-1
                 :output t
                 :error-output t)
    (run-command "cmake"
                 (list "--build" ".")
                 target-os
                 :directory build-dir
                 :external-format :latin-1
                 :output t
                 :error-output t))
  (let ((output
          (with-input-from-string (in (or input-string ""))
            (uiop:run-program (executable-path project-dir-pathname target-os)
                              :external-format :latin-1
                              :input (when input-string in)
                              :output :string
                              :error-output t))))
    (ppcre:regex-replace-all "\\r" output "")))

(defun tiger-source-path (name)
  (uiop:merge-pathnames*
   name
   (uiop:ensure-directory-pathname
    (asdf:system-relative-pathname "cl-tiger-test" "tiger-program"))))


(defun tiger-book-test-source-path (name)
  (uiop:merge-pathnames*
   name
   (uiop:ensure-directory-pathname (tiger-source-path "book-test"))))

(defmacro with-temporary-directory ((dir-pathname-var dir-name &optional keep-dir) &body body)
  `(let ((,dir-pathname-var (uiop:ensure-pathname
                             (uiop:merge-pathnames*
                              ,dir-name
                              (uiop:ensure-directory-pathname
                               (uiop:merge-pathnames*
                                (uiop:with-temporary-file (:pathname pathname)
                                  (pathname-name pathname))
                                (uiop:temporary-directory))))
                             :want-pathname t
                             :ensure-directory t
                             :ensure-directories-exist t)))
     (unwind-protect
          (progn ,@body)
       (unless ,keep-dir
         (uiop:delete-directory-tree ,dir-pathname-var :validate t)))))

(defun read-test-file-string (filename)
  (ppcre:regex-replace-all
   "\\r"
   (uiop:read-file-string
    (uiop:merge-pathnames*
     filename
     (uiop:ensure-directory-pathname
      (asdf:system-relative-pathname "cl-tiger-test" "test"))))
   ""))

(parachute:define-test straight-line
  (parachute:is
   equal
   (list
    (concatenate
     'string
     "8 7"
     (string #\Newline)
     "80"
     (string #\Newline))
    '(("b" . 80) ("a" . 8)))
   (let ((result-env nil))
     (list
      (with-output-to-string (s)
        (let ((*standard-output* s))
          (setf result-env
                (straight-line:interpret-stm
                 (straight-line:stm-compound
                  (straight-line:stm-assign
                   "a"
                   (straight-line:expr-op
                    (straight-line:expr-int 5)
                    straight-line:op-plus
                    (straight-line:expr-int 3)))
                  (straight-line:stm-compound
                   (straight-line:stm-assign
                    "b"
                    (straight-line:expr-stm-then-expr
                     (straight-line:stm-print
                      (list (straight-line:expr-id "a")
                            (straight-line:expr-op
                             (straight-line:expr-id "a")
                             straight-line:op-minus
                             (straight-line:expr-int 1))))
                     (straight-line:expr-op
                      (straight-line:expr-int 10)
                      straight-line:op-times
                      (straight-line:expr-id "a"))))
                   (straight-line:stm-print
                    (list (straight-line:expr-id "b")))))
                 nil))))
      result-env))))

(parachute:define-test break-not-within-loop
  (parachute:fail
      (cl-tiger:compile-tiger
       (tiger-source-path "break-not-within-loop.tig")
       nil
       (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
       :build-args (list :generate-get-exe-path-target t))
      'type-check:break-not-within-loop))

(parachute:define-test circular-dep
  (parachute:fail
      (cl-tiger:compile-tiger
       (tiger-source-path "circular-dep.tig")
       nil
       (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
       :build-args (list :generate-get-exe-path-target t))
      'type-check:circular-dep))

(parachute:define-test queens
  (parachute:is
   string=
   (read-test-file-string "queens-output.txt")
   (with-temporary-directory (project-dir "queens")
     (cl-tiger:compile-tiger
      (tiger-source-path "queens.tig")
      project-dir
      (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
      :build-args (list :generate-get-exe-path-target t))
     (build-and-run-project project-dir *target-os*))))

(parachute:define-test merge
  (parachute:is
   string=
   (concatenate 'string
                "-7897 -45 -34 3 637 789 896 987 3748 3784 7632 7667 9767 7823743"
                (string #\Space)
                (string #\Newline))
   (with-temporary-directory (project-dir "merge")
     (cl-tiger:compile-tiger
      (tiger-source-path "merge.tig")
      project-dir
      (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
      :build-args (list :generate-get-exe-path-target t))
     (build-and-run-project
      project-dir *target-os*
      :input-string "-45 3 896 3748 3784 7823743; -7897 -34 637 789 987 7632 7667 9767;"))))

(parachute:define-test lots-of-args
  (parachute:is
   string=
   (read-test-file-string "lots-of-args-output.txt")
   (with-temporary-directory (project-dir "lots-of-args")
     (cl-tiger:compile-tiger
      (tiger-source-path "lots-of-args.tig")
      project-dir
      (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
      :build-args (list :generate-get-exe-path-target t))
     (build-and-run-project
      project-dir *target-os*))))

(parachute:define-test reference-nil-record
  (parachute:is
   string=
   (concatenate 'string
                "Reference a field of a nil record"
                (string #\Newline))
   (with-temporary-directory (project-dir "reference-nil-record")
     (cl-tiger:compile-tiger
      (tiger-source-path "reference-nil-record.tig")
      project-dir
      (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
      :build-args (list :generate-get-exe-path-target t))
     (handler-bind ((uiop:subprocess-error
                      (lambda (condition)
                        (when (= (uiop:subprocess-error-code condition) 1)
                          (continue)))))
       (build-and-run-project project-dir *target-os*)))))

(parachute:define-test array-index-out-of-range-1
  (parachute:is
   string=
   (concatenate 'string
                "Index -1 out of range [0, 8)"
                (string #\Newline))
   (with-temporary-directory (project-dir "array-index-out-of-range-1")
     (cl-tiger:compile-tiger
      (tiger-source-path "array-index-out-of-range-1.tig")
      project-dir
      (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
      :build-args (list :generate-get-exe-path-target t))
     (handler-bind ((uiop:subprocess-error
                      (lambda (condition)
                        (when (= (uiop:subprocess-error-code condition) 1)
                          (continue)))))
       (build-and-run-project project-dir *target-os*)))))

(parachute:define-test array-index-out-of-range-2
  (parachute:is
   string=
   (concatenate 'string
                "Index 8 out of range [0, 8)"
                (string #\Newline))
   (with-temporary-directory (project-dir "array-index-out-of-range-2")
     (cl-tiger:compile-tiger
      (tiger-source-path "array-index-out-of-range-2.tig")
      project-dir
      (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
      :build-args (list :generate-get-exe-path-target t))
     (handler-bind ((uiop:subprocess-error
                      (lambda (condition)
                        (when (= (uiop:subprocess-error-code condition) 1)
                          (continue)))))
       (build-and-run-project project-dir *target-os*)))))

(parachute:define-test book-test-01
  (parachute:finish
   (cl-tiger:compile-tiger
    (tiger-book-test-source-path "test-01.tig")
    nil
    (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
    :dont-generate-project t)))

(parachute:define-test book-test-02
  (parachute:finish
   (cl-tiger:compile-tiger
    (tiger-book-test-source-path "test-02.tig")
    nil
    (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
    :dont-generate-project t)))

(parachute:define-test book-test-03
  (parachute:is
   string=
   "Somebody 1000"
   (with-temporary-directory (project-dir "book-test-03")
     (cl-tiger:compile-tiger
      (tiger-book-test-source-path "test-03.tig")
      project-dir
      (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
      :build-args (list :generate-get-exe-path-target t))
     (handler-bind ((uiop:subprocess-error
                      (lambda (condition)
                        (when (= (uiop:subprocess-error-code condition) 1)
                          (continue)))))
       (build-and-run-project project-dir *target-os*)))))

(parachute:define-test book-test-04
  (parachute:is
   string=
   "3628800"
   (with-temporary-directory (project-dir "book-test-04")
     (cl-tiger:compile-tiger
      (tiger-book-test-source-path "test-04.tig")
      project-dir
      (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
      :build-args (list :generate-get-exe-path-target t))
     (handler-bind ((uiop:subprocess-error
                      (lambda (condition)
                        (when (= (uiop:subprocess-error-code condition) 1)
                          (continue)))))
       (build-and-run-project project-dir *target-os*)))))

(parachute:define-test book-test-05
  (parachute:finish
   (cl-tiger:compile-tiger
    (tiger-book-test-source-path "test-05.tig")
    nil
    (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
    :dont-generate-project t)))

(parachute:define-test book-test-06
  (parachute:finish
   (cl-tiger:compile-tiger
    (tiger-book-test-source-path "test-06.tig")
    nil
    (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
    :dont-generate-project t)))

(parachute:define-test book-test-07
  (parachute:finish
   (cl-tiger:compile-tiger
    (tiger-book-test-source-path "test-07.tig")
    nil
    (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
    :dont-generate-project t)))

(parachute:define-test book-test-08
  (parachute:finish
   (cl-tiger:compile-tiger
    (tiger-book-test-source-path "test-08.tig")
    nil
    (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
    :dont-generate-project t)))

(parachute:define-test book-test-09
  (parachute:fail
      (cl-tiger:compile-tiger
       (tiger-book-test-source-path "test-09.tig")
       nil
       (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
       :build-args (list :generate-get-exe-path-target t))
      'type-check:then-else-types-of-if-mismatch))

(parachute:define-test book-test-10
  (parachute:fail
      (cl-tiger:compile-tiger
       (tiger-book-test-source-path "test-10.tig")
       nil
       (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
       :build-args (list :generate-get-exe-path-target t))
      'type-check:body-of-while-not-unit))

(parachute:define-test book-test-11
  (parachute:finish
   (expect-type-check-errors
       '(type-check:for-high-not-int type-check:assign-index-var)
       t
     (cl-tiger:compile-tiger
      (tiger-book-test-source-path "test-11.tig")
      nil
      (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
      :dont-generate-project t))))

(parachute:define-test book-test-12
  (parachute:finish
   (cl-tiger:compile-tiger
    (tiger-book-test-source-path "test-12.tig")
    nil
    (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
    :dont-generate-project t)))

(parachute:define-test book-test-13
  (parachute:fail
      (cl-tiger:compile-tiger
       (tiger-book-test-source-path "test-13.tig")
       nil
       (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
       :dont-generate-project t)
      type-check:unsupport-operation))

(parachute:define-test book-test-14
  (parachute:fail
      (cl-tiger:compile-tiger
       (tiger-book-test-source-path "test-14.tig")
       nil
       (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
       :dont-generate-project t)
      'type-check:unsupport-operation))

(parachute:define-test book-test-15
  (parachute:fail
      (cl-tiger:compile-tiger
       (tiger-book-test-source-path "test-15.tig")
       nil
       (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
       :dont-generate-project t)
      'type-check:then-of-if-then-not-unit))

(parachute:define-test book-test-16
  (parachute:fail
      (cl-tiger:compile-tiger
       (tiger-book-test-source-path "test-16.tig")
       nil
       (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
       :build-args (list :generate-get-exe-path-target t))
      'type-check:circular-dep))

(parachute:define-test book-test-17
  (parachute:fail
      (cl-tiger:compile-tiger
       (tiger-book-test-source-path "test-17.tig")
       nil
       (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
       :build-args (list :generate-get-exe-path-target t))
      'type-check:undefined-field-type))

(parachute:define-test book-test-18
  (parachute:fail
      (cl-tiger:compile-tiger
       (tiger-book-test-source-path "test-18.tig")
       nil
       (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
       :build-args (list :generate-get-exe-path-target t))
      'type-check:undefined-fun))

(parachute:define-test book-test-19
  (parachute:fail
      (cl-tiger:compile-tiger
       (tiger-book-test-source-path "test-19.tig")
       nil
       (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
       :build-args (list :generate-get-exe-path-target t))
      'type-check:undefined-var))

(parachute:define-test book-test-20
  (parachute:fail
      (cl-tiger:compile-tiger
       (tiger-book-test-source-path "test-20.tig")
       nil
       (cl-tiger/target:target cl-tiger/target:arch-x86-64 *target-os*)
       :build-args (list :generate-get-exe-path-target t))
      'type-check:undefined-var))
