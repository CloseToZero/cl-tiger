(cl:defpackage :cl-tiger/asm
  (:use :cl)
  (:local-nicknames
   (:temp :cl-tiger/temp))
  (:export
   #:maybe-jump
   #:is-jump
   #:not-jump
   #:instr
   #:op-instr
   #:op-instr-template
   #:op-instr-dsts
   #:op-instr-srcs
   #:op-instr-jumps
   #:label-instr
   #:label-instr-template
   #:label-instr-name
   #:move-instr
   #:move-instr-template
   #:move-instr-dst
   #:move-instr-src

   #:format-instr-with
   #:format-instr))

(cl:in-package :cl-tiger/asm)

;; Template string:
;; 's0, 's1 ... refer to src0, src1 ...
;; 'd0, 'd1 ... refer to dst0, dst1 ...
;; 'j0, 'j1 ... refer to jump0, jump1 ...
;; '' stands for the single quote character '

(serapeum:defunion maybe-jump
  (is-jump
   ;; A list of temp:label
   (targets list))
  not-jump)

(serapeum:defunion instr
  (op-instr
   (template string)
   ;; A list of temp:temp
   (dsts list)
   ;; A list of temp:temp
   (srcs list)
   (jumps maybe-jump))
  (label-instr
   (str string)
   (name temp:label))
  (move-instr
   (template string)
   (dst temp:temp)
   (src temp:temp)))

(defun format-instr-with (instr dsts srcs jumps)
  (labels ((check-index (template match list index name)
             (when (or (< index 0)
                       (>= index (length list)))
               (error "Formating the template ~S, it refers to the ~A ~A, but the index ~A is out of the range [~A, ~A)"
                      template name match index 0 (length list))))
           (format-core (template regex datas data->str name)
             (cl-ppcre:regex-replace-all
              regex
              template
              (lambda (match index-str)
                (let ((index (parse-integer index-str)))
                  (check-index template match datas index name)
                  (funcall data->str (nth index datas))))
              :simple-calls t)))
    (let* ((template? t)
           (result (serapeum:match-of instr instr
                     ((op-instr template _ _ _)
                      template)
                     ((label-instr str _)
                      (setf template? nil)
                      str)
                     ((move-instr template _ _)
                      template))))
      (when template?
        (setf result
              (format-core result "'s(\\d+)" srcs #'temp:temp-name "source"))
        (setf result
              (format-core result "'d(\\d+)" dsts #'temp:temp-name "destination"))
        (setf result
              (format-core result "'j(\\d+)" jumps #'temp:label-name "jump_target"))
        (cl-ppcre:regex-replace-all "''" result "'"))
      result)))

(defun format-instr (instr)
  (trivia:let-match1 (list dsts srcs jumps) 
      (serapeum:match-of instr instr
        ((op-instr _ dsts srcs jumps)
         (list dsts srcs (serapeum:match-of maybe-jump jumps
                           ((is-jump targets) targets)
                           (not-jump nil))))
        ((label-instr _ _)
         (list nil nil nil))
        ((move-instr _ dst src)
         (list (list dst) (list src) nil)))
    (format-instr-with instr dsts srcs jumps)))
