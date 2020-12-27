(defpackage #:day-6
  (:use #:cl #:asdf #:uiop #:cl-ppcre)
  (:export :part-1 :part-2))

(in-package :day-6)

(defsystem day-6
  :serial t
  :components ((:file "day-6")))
