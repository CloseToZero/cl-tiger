(cl:defpackage :cl-tiger/x86-64-frame
  (:use :cl)
  (:local-nicknames
   (:target :cl-tiger/target)
   (:temp :cl-tiger/temp)
   (:ir :cl-tiger/ir)
   (:frame :cl-tiger/frame)))

(cl:in-package :cl-tiger/x86-64-frame)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass frame (frame:frame)
    ((num-formal-allocated
      :type fixnum
      :initform 0
      :accessor frame-num-formal-allocated)
     (num-local-allocated
      :type fixnum
      :initform 0
      :accessor frame-num-local-allocated)))

  (defclass access-in-frame (frame:access)
    ((offset
      :type fixnum
      :initarg :offset
      :reader access-in-frame-offset)))

  (defun access-in-frame (offset)
    (make-instance 'access-in-frame :offset offset))

  (defclass access-in-reg (frame:access)
    ((reg
      :type temp:temp
      :initarg :reg
      :reader access-in-reg-reg))))

(defun access-in-reg (reg)
  (make-instance 'access-in-reg :reg reg))

(defun alloc-formal (frame escape target)
  (if escape
      (let* ((word-size (frame:word-size target))
             (offset (+ (* (frame-num-formal-allocated frame) word-size)
                        ;; 2 * word-size for return address and saved static link
                        (* 2 word-size))))
        (incf (frame-num-formal-allocated frame))
        (access-in-frame offset))
      (access-in-reg (temp:new-temp))))

(defun alloc-local (frame escape target)
  (if escape
      (let* ((word-size (frame:word-size target))
             (offset (- (+ (* (frame-num-local-allocated frame) word-size) word-size))))
        (incf (frame-num-local-allocated frame))
        (access-in-frame offset))
      (access-in-reg (temp:new-temp))))

(defmethod frame:new-frame% (name formals target
                             (target-arch target:arch-x86-64) (target-os target:os-windows))
  (let ((frame (make-instance 'frame :name name :target target)))
    (setf (frame:frame-formals% frame)
          (mapcar (lambda (escape)
                    (alloc-formal frame escape target))
                  formals))
    frame))

(defmethod frame:alloc-local% (frame escape target
                               (target-arch target:arch-x86-64) (target-os target:os-windows))
  (alloc-local frame escape target))

(defvar *fp* (temp:new-named-temp "rbp"))
(defvar *rv* (temp:new-named-temp "rax"))

(defmethod frame:fp% (target
                      (target-arch target:arch-x86-64) (target-os target:os-windows))
  *fp*)

(defmethod frame:rv% (target
                      (target-arch target:arch-x86-64) (target-os target:os-windows))
  *rv*)

(defmethod frame:word-size% (target
                             (target-arch target:arch-x86-64) (target-os target:os-windows))
  8)

(defmethod frame:access-expr% (access fp-expr target
                               (target-arch target:arch-x86-64) (target-os target:os-windows))
  (trivia:ematch access
    ((access-in-frame offset)
     (ir:mem-expr
      (ir:bin-op-expr
       fp-expr
       ir:plus-bin-op
       (ir:int-expr offset))))
    ((access-in-reg reg)
     (ir:temp-expr reg))))

(defmethod frame:external-call% (name args target
                                 (target-arch target:arch-x86-64) (target-os target:os-windows))
  (ir:call-expr
   (ir:label-expr (temp:new-named-label name))
   args))

(defmethod frame:view-shift-for-fun-body% (frame body-stm target
                                           (target-arch target:arch-x86-64) (target-os target:os-windows))
  body-stm)
