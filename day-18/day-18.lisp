(in-package #:day-18)

(defparameter *input-file* "input")

(defun read-file ()
  (uiop:read-file-lines *input-file*))

(defun calculate (line &optional (start 0))
  (loop :for i :from start :below (length line)
        :for ch = (elt line i)
        :with n = 0
        :with operation = nil
        :with first-number = t
        :do
    (case ch
      (#\Space
       nil)
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (if first-number
           (setf n (digit-char-p ch))
           (setf n (apply operation (list n (digit-char-p ch)))))
       (setf first-number nil))
      (#\+
       (setf operation #'+))
      (#\*
       (setf operation #'*))
      (#\(       
       (multiple-value-bind (m j)
           (calculate line (1+ i))
         (if operation
             (setf n (apply operation (list n m)))
             (setf n m))
         (setf i j))
       (setf first-number nil))
      (#\)
       (return (values n i))))

    :finally (return n)))

(defun part-1 ()
  (let ((input (read-file)))
    (reduce #'+
            (map 'list #'calculate input))))
