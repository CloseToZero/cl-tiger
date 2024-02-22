(cl:defpackage :cl-tiger
  (:use :cl)
  (:local-nicknames
   (:utils :cl-tiger/utils)
   (:target :cl-tiger/target)
   (:frame :cl-tiger/frame)
   (:parse :cl-tiger/parse)
   (:type-check :cl-tiger/type-check)
   (:translate :cl-tiger/translate)
   (:normalize :cl-tiger/normalize)
   (:instr-select :cl-tiger/instr-select)
   (:graph :cl-tiger/graph)
   (:flow-graph :cl-tiger/flow-graph)
   (:liveness :cl-tiger/liveness)
   (:reg-alloc :cl-tiger/reg-alloc)
   (:build :cl-tiger/build))
  (:export
   #:compile-tiger))

(cl:in-package :cl-tiger)

(defun compile-tiger (file dst-dir target
                      &key
                        (string-literal-as-comment t)
                        (build-args nil))
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
        (let ((build-frag-strs nil)
              (build-frag-funs nil))
          (setf build-frag-strs
                (loop for frag-str in frag-strs
                      collect
                      (build:frag-str
                       (frame:frag-str->definition
                        frag-str string-literal-as-comment target))))
          (setf build-frag-funs
                (loop for frag-fun in frag-funs
                      collect
                      (trivia:let-match1 (frame:frag-fun body frame) frag-fun
                        (trivia:let-match1 (list blocks exit-label)
                            (normalize:split-into-basic-blocks (normalize:normalize body))
                          (let ((instrs
                                   (frame:preserve-live-out
                                    frame
                                    (mapcan (lambda (stm)
                                              (instr-select:select-instrs stm frame target))
                                            (normalize:trace-schedule blocks exit-label))
                                    target)))
                            (let ((instrs (reg-alloc:reg-alloc instrs frame target)))
                              (trivia:let-match1 (list prolog instrs epilog)
                                  (frame:wrap-entry-exit frame instrs target)
                                (build:frag-fun prolog instrs epilog))))))))
          (apply #'build:build build-frag-strs build-frag-funs dst-dir target build-args))))))
