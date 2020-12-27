(defpackage #:day-5
  (:use #:cl #:asdf #:uiop)
  (:export :part-1 :part-2))

(in-package :day-5)

(defsystem day-5
  :serial t
  :components ((:file "day-5")))
