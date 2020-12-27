(defpackage #:day-3
  (:use #:cl #:asdf #:uiop)
  (:export :part-1 :part-2))

(in-package :day-3)

(defsystem day-3
  :serial t
  :components ((:file "day-3")))
