(in-package :day-1)

(defparameter *input-file* "input")

(defun read-file ()
  (map 'list #'parse-integer (uiop:read-file-lines *input-file*)))

(defun part-1 ()
  (let ((numbers (read-file))
        (n1 nil)
        (n2 nil))
    (loop :named outer :for i :from 0 :to (1- (length numbers)) :do
      (loop :for j :from i :to (1- (length numbers)) :do
            (setf n1 (nth i numbers))
            (setf n2 (nth j numbers))
            (when (= 2020 (+ n1 n2)) (return-from outer (values n1 n2 (* n1 n2))))))))

(defun part-2 ()
  (let ((numbers (read-file))
        (n1 nil)
        (n2 nil)
        (n3 nil)
        (sum nil))
    (loop :named outer :for i :from 0 :to (1- (length numbers)) :do
      (setf n1 (nth i numbers))
      (loop :for j :from i :to (1- (length numbers)) :do
        (setf n2 (nth j numbers))
        (loop :for k :from j :to (1- (length numbers)) :do              
          (setf n3 (nth k numbers))
          (setf sum (+ n1 n2 n3))
          (when (= 2020 sum) (return-from outer (values n1 n2 n3 (* n1 n2 n3)))))))))
