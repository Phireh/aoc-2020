(defpackage #:day-1
  (:use #:cl #:asdf #:uiop)
  (:export :part-1 :part-2))

(in-package :day-1)

(defsystem day-1
  :serial t
  :components ((:file "day-1")))
