(cl:defpackage :cl-tiger
  (:use :cl)
  (:local-nicknames
   (:utils :cl-tiger/utils)
   (:target :cl-tiger/target)
   (:asm :cl-tiger/asm)
   (:frame :cl-tiger/frame)
   (:parse :cl-tiger/parse)
   (:type-check :cl-tiger/type-check)
   (:translate :cl-tiger/translate)
   (:normalize :cl-tiger/normalize)
   (:instr-select :cl-tiger/instr-select))
  (:export
   #:compile-tiger-file))

(cl:in-package :cl-tiger)

(defun compile-tiger-file (file target &key (stream t) (string-literal-as-comment t))
  (let* ((text (uiop:read-file-string file))
         (line-map (utils:get-line-map text))
         (ast (parse:parse-program text)))
    (type-check:type-check-program ast line-map)
    (let ((frags (translate:translate-program ast target)))
      (trivia:let-match1 (list frag-strs frag-funs)
          (loop for frag in frags
                if (typep frag 'frame:frag-str)
                  collect frag into frag-strs
                else
                  collect frag into frag-funs
                finally (return (list frag-strs frag-funs)))
        (dolist (frag-str frag-strs)
          (format stream "~A~%" (frame:frag-str->definition frag-str string-literal-as-comment target)))
        (dolist (frag-fun frag-funs)
          (format stream "~%")
          (trivia:let-match1 (frame:frag-fun body frame) frag-fun
            (trivia:let-match1 (list blocks exit-label)
                (normalize:split-into-basic-blocks (normalize:normalize body))
              (let ((instrs
                      (frame:preserve-liveout
                       frame
                       (mapcan (lambda (stm)
                                 (instr-select:select-instrs stm frame target))
                               (normalize:trace-schedule blocks exit-label))
                       target)))
                (trivia:let-match1 (list prolog instrs epilog)
                    (frame:wrap-entry-exit frame instrs target)
                  (dolist (instr prolog)
                    (format t "~A~%" instr))
                  (dolist (instr instrs)
                    (format t "~A~%" (asm:format-instr instr)))
                  (dolist (instr epilog)
                    (format t "~A~%" instr)))))))))))
