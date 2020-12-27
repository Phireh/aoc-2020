(in-package :day-2)

(defparameter *input-file* "input")
(defparameter *regex* "(\\d+)-(\\d+) (\\w): (\\w+)")

(defun read-file ()
  (uiop:read-file-lines *input-file*))

(defun check-line-part-1 (line)
  (let 
      ((lo (parse-integer (elt line 0)))
       (hi (parse-integer (elt line 1)))
       (ch (coerce (elt line 2) 'character))
       (passwd (elt line 3)))
    (<= lo (count ch passwd) hi)))

(defun check-line-part-2 (line)
  (let
      ((pos1 (1- (parse-integer (elt line 0))))
       (pos2 (1- (parse-integer (elt line 1))))
       (ch (coerce (elt line 2) 'character))
       (passwd (elt line 3)))
    (= (count ch (list (coerce (elt passwd pos1) 'character) (coerce (elt passwd pos2) 'character))) 1)))


(defun check (check-line-fun)
  (let ((lines (read-file))
        (valid 0))
    (dolist (ln lines valid)
      (when (funcall check-line-fun (nth-value 1 (cl-ppcre:scan-to-strings *regex* ln)))
        (setf valid (1+ valid))))))

(defun part-1 ()
  (check #'check-line-part-1))

(defun part-2 ()
  (check #'check-line-part-2))
