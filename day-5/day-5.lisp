(in-package :day-5)

(defparameter *input-file* "input")

(defun read-file ()
  (uiop:read-file-lines *input-file*))

(defun lookup-seat (str)
  (loop :for ch :across str
        :with lo-row = 0
        :with hi-row = 127
        :with lo-col = 0
        :with hi-col = 7
        :for mid-row = (floor (+ lo-row hi-row) 2)
        :for mid-col = (floor (+ lo-col hi-col) 2)
        :do
           (case ch
             (#\F (setf hi-row mid-row))
             (#\B (setf lo-row (1+ mid-row)))
             (#\L (setf hi-col mid-col))
             (#\R (setf lo-col (1+ mid-col))))
        :finally
           ; Making sure we actually return the last, correct value
           (setf mid-row (floor (+ lo-row hi-row) 2))
           (setf mid-col (floor (+ lo-col hi-col) 2))
           (return (values mid-row mid-col))))



(defun part-1 ()
  (let ((id 0)
        (highest 0))
    (dolist (ln (read-file) highest)
      (multiple-value-bind (row col) (lookup-seat ln)
        (setf id (+ (* 8 row) col))
        (setf highest (max id highest))))))

(defun part-2 ()
  (let ((ids (list))
        (adj-ids (list)))
    (dolist (ln (read-file))
      (multiple-value-bind (row col) (lookup-seat ln)
        (push (+ (* 8 row) col) ids)))
    (setf ids (sort ids #'<))
    (loop :for id1 :in ids
          :for id2 :in (cdr ids)
          :for diff = (abs (- id1 id2))
          :when (= diff 2) :do
            (push id1 adj-ids)
            (push id2 adj-ids)
          :finally (return adj-ids))))
