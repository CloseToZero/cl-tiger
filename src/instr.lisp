(cl:defpackage :cl-tiger/instr
  (:use :cl)
  (:local-nicknames
   (:temp :cl-tiger/temp))
  (:export
   #:maybe-jump
   #:is-jump
   #:not-jump
   #:instr
   #:instr-op
   #:instr-op-template
   #:instr-op-dsts
   #:instr-op-srcs
   #:instr-op-jumps
   #:instr-label
   #:instr-label-template
   #:instr-label-name
   #:instr-move
   #:instr-move-template
   #:instr-move-dst
   #:instr-move-src
   #:instr-stack-arg
   #:instr-stack-arg-template
   #:instr-stack-arg-dsts
   #:instr-stack-arg-srcs
   #:instr-stack-arg-reloc-fun
   #:instr-call
   #:instr-call-template
   #:instr-call-dsts
   #:instr-call-srcs
   #:instr-call-num-of-args

   #:format-instr-with
   #:format-instr))

(cl:in-package :cl-tiger/instr)

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
  (instr-op
   (template string)
   ;; A list of temp:temp
   (dsts list)
   ;; A list of temp:temp
   (srcs list)
   (jumps maybe-jump))
  (instr-label
   (str string)
   (name temp:label))
  (instr-move
   (template string)
   (dst temp:temp)
   (src temp:temp))
  (instr-stack-arg
   (template string)
   (dsts list)
   (srcs list)
   ;; Pass the instr itself and the final frame-size
   ;; return a new instr (can also return the passed instr)
   (reloc-fun (or null (function (instr fixnum) instr))))
  (instr-call
   (template string)
   (dsts list)
   (srcs list)
   (num-of-args fixnum)))

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
                     ((instr-op template _ _ _)
                      template)
                     ((instr-call template _ _ _)
                      template)
                     ((instr-stack-arg template _ _ _)
                      template)
                     ((instr-label str _)
                      (setf template? nil)
                      str)
                     ((instr-move template _ _)
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
        ((instr-op _ dsts srcs jumps)
         (list dsts srcs (serapeum:match-of maybe-jump jumps
                           ((is-jump targets) targets)
                           (not-jump nil))))
        ((instr-call _ dsts srcs _)
         (list dsts srcs nil))
        ((instr-stack-arg _ dsts srcs _)
         (list dsts srcs nil))
        ((instr-label _ _)
         (list nil nil nil))
        ((instr-move _ dst src)
         (list (list dst) (list src) nil)))
    (format-instr-with instr dsts srcs jumps)))
