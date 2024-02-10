(cl:defpackage :cl-tiger/utils
  (:use :cl)
  (:export
   #:get-line-map
   #:pos->line-info
   #:str-without-newlines))

(cl:in-package :cl-tiger/utils)

(defun get-line-map (text)
  "Construct a map of the following form:
((start-1 end-1) line-1)
((start-2 end-2) line-2)
((start-3 end-3) line-3)
...
start-n and end-n are character range of line-n (inclusive),
note that line-n doesn't include \r or \n, so it's easy to print
line-n without an newline."
  (let ((map nil))
    (loop with len = (length text)
          with start = 0
          for i from 0 below len
          for ch = (elt text i)
          for next-ch = (when (< (+ i 1) len) (elt text (+ i 1)))
          do (cond ((and (eql ch #\Return)
                         (eql next-ch #\Newline))
                    ;; \r\n
                    (push (list start (+ i 1) (subseq text start i)) map)
                    ;; skip \n
                    (incf i)
                    (setf start (+ i 1)))
                   ((or (eql ch #\Return)
                        (eql next-ch #\Newline))
                    ;; \r or \n
                    (push (list start i (subseq text start i)) map)
                    (setf start (+ i 1))))
          finally (when (< start len)
                    (push (list start (- len 1) (subseq text start len)) map)))
    (nreverse map)))

;; Returns nil or (line-number column-number line)
(defun pos->line-info (pos line-map)
  (block nil
    (let ((line-number 0))
      (dolist (line-info line-map)
        (incf line-number)
        (trivia:let-match1 (list start end line) line-info
          (when (and (<= start pos)
                     (<= pos end))
            (return (list line-number (1+ (- pos start)) line))))))))

(defun str-without-newlines (str)
  (cl-ppcre:regex-replace-all "\\n" (cl-ppcre:regex-replace-all "\\r" str "\\r") "\\n") )
