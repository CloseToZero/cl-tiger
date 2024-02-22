(cl:defpackage :cl-tiger/target
  (:use :cl)
  (:export
   #:arch
   #:arch-x86-64

   #:os
   #:os-windows
   #:os-linux
   #:os-mac

   #:target
   #:target-arch
   #:target-os))

(cl:in-package :cl-tiger/target)

(serapeum:defunion arch
  arch-x86-64)

(serapeum:defunion os
  os-windows
  os-linux
  os-mac)

(serapeum:defconstructor target
  (arch arch)
  (os os))
