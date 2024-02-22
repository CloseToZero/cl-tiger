(cl:defpackage :cl-tiger/test
  (:use :cl)
  (:local-nicknames
   (:target :cl-tiger/target)))

(cl:in-package :cl-tiger/test)

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

(defun tiger-source-path (source-name)
  (uiop:merge-pathnames*
   source-name
   (uiop:ensure-directory-pathname
    (asdf:system-relative-pathname "cl-tiger" "tiger-programs"))))

(defmacro with-temporary-directory ((dir-pathname-var dir-name &optional keep-dir) &body body)
  `(let ((,dir-pathname-var (uiop:ensure-pathname
                             (uiop:merge-pathnames* ,dir-name (uiop:temporary-directory))
                             :want-pathname t
                             :ensure-directory t
                             :ensure-directories-exist t)))
     (unwind-protect
          (progn ,@body)
       (unless ,keep-dir
         (uiop:delete-directory-tree ,dir-pathname-var :validate t)))))

(parachute:define-test queens
  (parachute:is string=
      (with-temporary-directory (project-dir "queens")
        (cl-tiger:compile-tiger
         (tiger-source-path "queens.tig")
         project-dir
         (cl-tiger/target:target cl-tiger/target:arch-x86-64 cl-tiger/target:os-windows)
         :build-args (list :generate-get-exe-path-target t))
        (build-and-run-project project-dir cl-tiger/target:os-windows))
      " O . . . . . . .
 . . . . O . . .
 . . . . . . . O
 . . . . . O . .
 . . O . . . . .
 . . . . . . O .
 . O . . . . . .
 . . . O . . . .

 O . . . . . . .
 . . . . . O . .
 . . . . . . . O
 . . O . . . . .
 . . . . . . O .
 . . . O . . . .
 . O . . . . . .
 . . . . O . . .

 O . . . . . . .
 . . . . . . O .
 . . . O . . . .
 . . . . . O . .
 . . . . . . . O
 . O . . . . . .
 . . . . O . . .
 . . O . . . . .

 O . . . . . . .
 . . . . . . O .
 . . . . O . . .
 . . . . . . . O
 . O . . . . . .
 . . . O . . . .
 . . . . . O . .
 . . O . . . . .

 . O . . . . . .
 . . . O . . . .
 . . . . . O . .
 . . . . . . . O
 . . O . . . . .
 O . . . . . . .
 . . . . . . O .
 . . . . O . . .

 . O . . . . . .
 . . . . O . . .
 . . . . . . O .
 O . . . . . . .
 . . O . . . . .
 . . . . . . . O
 . . . . . O . .
 . . . O . . . .

 . O . . . . . .
 . . . . O . . .
 . . . . . . O .
 . . . O . . . .
 O . . . . . . .
 . . . . . . . O
 . . . . . O . .
 . . O . . . . .

 . O . . . . . .
 . . . . . O . .
 O . . . . . . .
 . . . . . . O .
 . . . O . . . .
 . . . . . . . O
 . . O . . . . .
 . . . . O . . .

 . O . . . . . .
 . . . . . O . .
 . . . . . . . O
 . . O . . . . .
 O . . . . . . .
 . . . O . . . .
 . . . . . . O .
 . . . . O . . .

 . O . . . . . .
 . . . . . . O .
 . . O . . . . .
 . . . . . O . .
 . . . . . . . O
 . . . . O . . .
 O . . . . . . .
 . . . O . . . .

 . O . . . . . .
 . . . . . . O .
 . . . . O . . .
 . . . . . . . O
 O . . . . . . .
 . . . O . . . .
 . . . . . O . .
 . . O . . . . .

 . O . . . . . .
 . . . . . . . O
 . . . . . O . .
 O . . . . . . .
 . . O . . . . .
 . . . . O . . .
 . . . . . . O .
 . . . O . . . .

 . . O . . . . .
 O . . . . . . .
 . . . . . . O .
 . . . . O . . .
 . . . . . . . O
 . O . . . . . .
 . . . O . . . .
 . . . . . O . .

 . . O . . . . .
 . . . . O . . .
 . O . . . . . .
 . . . . . . . O
 O . . . . . . .
 . . . . . . O .
 . . . O . . . .
 . . . . . O . .

 . . O . . . . .
 . . . . O . . .
 . O . . . . . .
 . . . . . . . O
 . . . . . O . .
 . . . O . . . .
 . . . . . . O .
 O . . . . . . .

 . . O . . . . .
 . . . . O . . .
 . . . . . . O .
 O . . . . . . .
 . . . O . . . .
 . O . . . . . .
 . . . . . . . O
 . . . . . O . .

 . . O . . . . .
 . . . . O . . .
 . . . . . . . O
 . . . O . . . .
 O . . . . . . .
 . . . . . . O .
 . O . . . . . .
 . . . . . O . .

 . . O . . . . .
 . . . . . O . .
 . O . . . . . .
 . . . . O . . .
 . . . . . . . O
 O . . . . . . .
 . . . . . . O .
 . . . O . . . .

 . . O . . . . .
 . . . . . O . .
 . O . . . . . .
 . . . . . . O .
 O . . . . . . .
 . . . O . . . .
 . . . . . . . O
 . . . . O . . .

 . . O . . . . .
 . . . . . O . .
 . O . . . . . .
 . . . . . . O .
 . . . . O . . .
 O . . . . . . .
 . . . . . . . O
 . . . O . . . .

 . . O . . . . .
 . . . . . O . .
 . . . O . . . .
 O . . . . . . .
 . . . . . . . O
 . . . . O . . .
 . . . . . . O .
 . O . . . . . .

 . . O . . . . .
 . . . . . O . .
 . . . O . . . .
 . O . . . . . .
 . . . . . . . O
 . . . . O . . .
 . . . . . . O .
 O . . . . . . .

 . . O . . . . .
 . . . . . O . .
 . . . . . . . O
 O . . . . . . .
 . . . O . . . .
 . . . . . . O .
 . . . . O . . .
 . O . . . . . .

 . . O . . . . .
 . . . . . O . .
 . . . . . . . O
 O . . . . . . .
 . . . . O . . .
 . . . . . . O .
 . O . . . . . .
 . . . O . . . .

 . . O . . . . .
 . . . . . O . .
 . . . . . . . O
 . O . . . . . .
 . . . O . . . .
 O . . . . . . .
 . . . . . . O .
 . . . . O . . .

 . . O . . . . .
 . . . . . . O .
 . O . . . . . .
 . . . . . . . O
 . . . . O . . .
 O . . . . . . .
 . . . O . . . .
 . . . . . O . .

 . . O . . . . .
 . . . . . . O .
 . O . . . . . .
 . . . . . . . O
 . . . . . O . .
 . . . O . . . .
 O . . . . . . .
 . . . . O . . .

 . . O . . . . .
 . . . . . . . O
 . . . O . . . .
 . . . . . . O .
 O . . . . . . .
 . . . . . O . .
 . O . . . . . .
 . . . . O . . .

 . . . O . . . .
 O . . . . . . .
 . . . . O . . .
 . . . . . . . O
 . O . . . . . .
 . . . . . . O .
 . . O . . . . .
 . . . . . O . .

 . . . O . . . .
 O . . . . . . .
 . . . . O . . .
 . . . . . . . O
 . . . . . O . .
 . . O . . . . .
 . . . . . . O .
 . O . . . . . .

 . . . O . . . .
 . O . . . . . .
 . . . . O . . .
 . . . . . . . O
 . . . . . O . .
 O . . . . . . .
 . . O . . . . .
 . . . . . . O .

 . . . O . . . .
 . O . . . . . .
 . . . . . . O .
 . . O . . . . .
 . . . . . O . .
 . . . . . . . O
 O . . . . . . .
 . . . . O . . .

 . . . O . . . .
 . O . . . . . .
 . . . . . . O .
 . . O . . . . .
 . . . . . O . .
 . . . . . . . O
 . . . . O . . .
 O . . . . . . .

 . . . O . . . .
 . O . . . . . .
 . . . . . . O .
 . . . . O . . .
 O . . . . . . .
 . . . . . . . O
 . . . . . O . .
 . . O . . . . .

 . . . O . . . .
 . O . . . . . .
 . . . . . . . O
 . . . . O . . .
 . . . . . . O .
 O . . . . . . .
 . . O . . . . .
 . . . . . O . .

 . . . O . . . .
 . O . . . . . .
 . . . . . . . O
 . . . . . O . .
 O . . . . . . .
 . . O . . . . .
 . . . . O . . .
 . . . . . . O .

 . . . O . . . .
 . . . . . O . .
 O . . . . . . .
 . . . . O . . .
 . O . . . . . .
 . . . . . . . O
 . . O . . . . .
 . . . . . . O .

 . . . O . . . .
 . . . . . O . .
 . . . . . . . O
 . O . . . . . .
 . . . . . . O .
 O . . . . . . .
 . . O . . . . .
 . . . . O . . .

 . . . O . . . .
 . . . . . O . .
 . . . . . . . O
 . . O . . . . .
 O . . . . . . .
 . . . . . . O .
 . . . . O . . .
 . O . . . . . .

 . . . O . . . .
 . . . . . . O .
 O . . . . . . .
 . . . . . . . O
 . . . . O . . .
 . O . . . . . .
 . . . . . O . .
 . . O . . . . .

 . . . O . . . .
 . . . . . . O .
 . . O . . . . .
 . . . . . . . O
 . O . . . . . .
 . . . . O . . .
 O . . . . . . .
 . . . . . O . .

 . . . O . . . .
 . . . . . . O .
 . . . . O . . .
 . O . . . . . .
 . . . . . O . .
 O . . . . . . .
 . . O . . . . .
 . . . . . . . O

 . . . O . . . .
 . . . . . . O .
 . . . . O . . .
 . . O . . . . .
 O . . . . . . .
 . . . . . O . .
 . . . . . . . O
 . O . . . . . .

 . . . O . . . .
 . . . . . . . O
 O . . . . . . .
 . . O . . . . .
 . . . . . O . .
 . O . . . . . .
 . . . . . . O .
 . . . . O . . .

 . . . O . . . .
 . . . . . . . O
 O . . . . . . .
 . . . . O . . .
 . . . . . . O .
 . O . . . . . .
 . . . . . O . .
 . . O . . . . .

 . . . O . . . .
 . . . . . . . O
 . . . . O . . .
 . . O . . . . .
 O . . . . . . .
 . . . . . . O .
 . O . . . . . .
 . . . . . O . .

 . . . . O . . .
 O . . . . . . .
 . . . O . . . .
 . . . . . O . .
 . . . . . . . O
 . O . . . . . .
 . . . . . . O .
 . . O . . . . .

 . . . . O . . .
 O . . . . . . .
 . . . . . . . O
 . . . O . . . .
 . O . . . . . .
 . . . . . . O .
 . . O . . . . .
 . . . . . O . .

 . . . . O . . .
 O . . . . . . .
 . . . . . . . O
 . . . . . O . .
 . . O . . . . .
 . . . . . . O .
 . O . . . . . .
 . . . O . . . .

 . . . . O . . .
 . O . . . . . .
 . . . O . . . .
 . . . . . O . .
 . . . . . . . O
 . . O . . . . .
 O . . . . . . .
 . . . . . . O .

 . . . . O . . .
 . O . . . . . .
 . . . O . . . .
 . . . . . . O .
 . . O . . . . .
 . . . . . . . O
 . . . . . O . .
 O . . . . . . .

 . . . . O . . .
 . O . . . . . .
 . . . . . O . .
 O . . . . . . .
 . . . . . . O .
 . . . O . . . .
 . . . . . . . O
 . . O . . . . .

 . . . . O . . .
 . O . . . . . .
 . . . . . . . O
 O . . . . . . .
 . . . O . . . .
 . . . . . . O .
 . . O . . . . .
 . . . . . O . .

 . . . . O . . .
 . . O . . . . .
 O . . . . . . .
 . . . . . O . .
 . . . . . . . O
 . O . . . . . .
 . . . O . . . .
 . . . . . . O .

 . . . . O . . .
 . . O . . . . .
 O . . . . . . .
 . . . . . . O .
 . O . . . . . .
 . . . . . . . O
 . . . . . O . .
 . . . O . . . .

 . . . . O . . .
 . . O . . . . .
 . . . . . . . O
 . . . O . . . .
 . . . . . . O .
 O . . . . . . .
 . . . . . O . .
 . O . . . . . .

 . . . . O . . .
 . . . . . . O .
 O . . . . . . .
 . . O . . . . .
 . . . . . . . O
 . . . . . O . .
 . . . O . . . .
 . O . . . . . .

 . . . . O . . .
 . . . . . . O .
 O . . . . . . .
 . . . O . . . .
 . O . . . . . .
 . . . . . . . O
 . . . . . O . .
 . . O . . . . .

 . . . . O . . .
 . . . . . . O .
 . O . . . . . .
 . . . O . . . .
 . . . . . . . O
 O . . . . . . .
 . . O . . . . .
 . . . . . O . .

 . . . . O . . .
 . . . . . . O .
 . O . . . . . .
 . . . . . O . .
 . . O . . . . .
 O . . . . . . .
 . . . O . . . .
 . . . . . . . O

 . . . . O . . .
 . . . . . . O .
 . O . . . . . .
 . . . . . O . .
 . . O . . . . .
 O . . . . . . .
 . . . . . . . O
 . . . O . . . .

 . . . . O . . .
 . . . . . . O .
 . . . O . . . .
 O . . . . . . .
 . . O . . . . .
 . . . . . . . O
 . . . . . O . .
 . O . . . . . .

 . . . . O . . .
 . . . . . . . O
 . . . O . . . .
 O . . . . . . .
 . . O . . . . .
 . . . . . O . .
 . O . . . . . .
 . . . . . . O .

 . . . . O . . .
 . . . . . . . O
 . . . O . . . .
 O . . . . . . .
 . . . . . . O .
 . O . . . . . .
 . . . . . O . .
 . . O . . . . .

 . . . . . O . .
 O . . . . . . .
 . . . . O . . .
 . O . . . . . .
 . . . . . . . O
 . . O . . . . .
 . . . . . . O .
 . . . O . . . .

 . . . . . O . .
 . O . . . . . .
 . . . . . . O .
 O . . . . . . .
 . . O . . . . .
 . . . . O . . .
 . . . . . . . O
 . . . O . . . .

 . . . . . O . .
 . O . . . . . .
 . . . . . . O .
 O . . . . . . .
 . . . O . . . .
 . . . . . . . O
 . . . . O . . .
 . . O . . . . .

 . . . . . O . .
 . . O . . . . .
 O . . . . . . .
 . . . . . . O .
 . . . . O . . .
 . . . . . . . O
 . O . . . . . .
 . . . O . . . .

 . . . . . O . .
 . . O . . . . .
 O . . . . . . .
 . . . . . . . O
 . . . O . . . .
 . O . . . . . .
 . . . . . . O .
 . . . . O . . .

 . . . . . O . .
 . . O . . . . .
 O . . . . . . .
 . . . . . . . O
 . . . . O . . .
 . O . . . . . .
 . . . O . . . .
 . . . . . . O .

 . . . . . O . .
 . . O . . . . .
 . . . . O . . .
 . . . . . . O .
 O . . . . . . .
 . . . O . . . .
 . O . . . . . .
 . . . . . . . O

 . . . . . O . .
 . . O . . . . .
 . . . . O . . .
 . . . . . . . O
 O . . . . . . .
 . . . O . . . .
 . O . . . . . .
 . . . . . . O .

 . . . . . O . .
 . . O . . . . .
 . . . . . . O .
 . O . . . . . .
 . . . O . . . .
 . . . . . . . O
 O . . . . . . .
 . . . . O . . .

 . . . . . O . .
 . . O . . . . .
 . . . . . . O .
 . O . . . . . .
 . . . . . . . O
 . . . . O . . .
 O . . . . . . .
 . . . O . . . .

 . . . . . O . .
 . . O . . . . .
 . . . . . . O .
 . . . O . . . .
 O . . . . . . .
 . . . . . . . O
 . O . . . . . .
 . . . . O . . .

 . . . . . O . .
 . . . O . . . .
 O . . . . . . .
 . . . . O . . .
 . . . . . . . O
 . O . . . . . .
 . . . . . . O .
 . . O . . . . .

 . . . . . O . .
 . . . O . . . .
 . O . . . . . .
 . . . . . . . O
 . . . . O . . .
 . . . . . . O .
 O . . . . . . .
 . . O . . . . .

 . . . . . O . .
 . . . O . . . .
 . . . . . . O .
 O . . . . . . .
 . . O . . . . .
 . . . . O . . .
 . O . . . . . .
 . . . . . . . O

 . . . . . O . .
 . . . O . . . .
 . . . . . . O .
 O . . . . . . .
 . . . . . . . O
 . O . . . . . .
 . . . . O . . .
 . . O . . . . .

 . . . . . O . .
 . . . . . . . O
 . O . . . . . .
 . . . O . . . .
 O . . . . . . .
 . . . . . . O .
 . . . . O . . .
 . . O . . . . .

 . . . . . . O .
 O . . . . . . .
 . . O . . . . .
 . . . . . . . O
 . . . . . O . .
 . . . O . . . .
 . O . . . . . .
 . . . . O . . .

 . . . . . . O .
 . O . . . . . .
 . . . O . . . .
 O . . . . . . .
 . . . . . . . O
 . . . . O . . .
 . . O . . . . .
 . . . . . O . .

 . . . . . . O .
 . O . . . . . .
 . . . . . O . .
 . . O . . . . .
 O . . . . . . .
 . . . O . . . .
 . . . . . . . O
 . . . . O . . .

 . . . . . . O .
 . . O . . . . .
 O . . . . . . .
 . . . . . O . .
 . . . . . . . O
 . . . . O . . .
 . O . . . . . .
 . . . O . . . .

 . . . . . . O .
 . . O . . . . .
 . . . . . . . O
 . O . . . . . .
 . . . . O . . .
 O . . . . . . .
 . . . . . O . .
 . . . O . . . .

 . . . . . . O .
 . . . O . . . .
 . O . . . . . .
 . . . . O . . .
 . . . . . . . O
 O . . . . . . .
 . . O . . . . .
 . . . . . O . .

 . . . . . . O .
 . . . O . . . .
 . O . . . . . .
 . . . . . . . O
 . . . . . O . .
 O . . . . . . .
 . . O . . . . .
 . . . . O . . .

 . . . . . . O .
 . . . . O . . .
 . . O . . . . .
 O . . . . . . .
 . . . . . O . .
 . . . . . . . O
 . O . . . . . .
 . . . O . . . .

 . . . . . . . O
 . O . . . . . .
 . . . O . . . .
 O . . . . . . .
 . . . . . . O .
 . . . . O . . .
 . . O . . . . .
 . . . . . O . .

 . . . . . . . O
 . O . . . . . .
 . . . . O . . .
 . . O . . . . .
 O . . . . . . .
 . . . . . . O .
 . . . O . . . .
 . . . . . O . .

 . . . . . . . O
 . . O . . . . .
 O . . . . . . .
 . . . . . O . .
 . O . . . . . .
 . . . . O . . .
 . . . . . . O .
 . . . O . . . .

 . . . . . . . O
 . . . O . . . .
 O . . . . . . .
 . . O . . . . .
 . . . . . O . .
 . O . . . . . .
 . . . . . . O .
 . . . . O . . .

"))
