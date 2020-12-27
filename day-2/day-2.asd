(defpackage #:day-2
  (:use #:cl #:asdf #:uiop #:cl-ppcre)
  (:export :part-1 :part-2))

(in-package :day-2)

(defsystem day-2
  :serial t
  :components ((:file "day-2")))
