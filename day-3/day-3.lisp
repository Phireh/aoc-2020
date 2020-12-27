(in-package :day-3)

(defparameter *input-file* "input")

(defun read-file ()
  (uiop:read-file-lines *input-file*))

(defun check-tree (ch)
  (char= (coerce ch 'character) #\#))

(defun check-slope (right-slope down-slope)
  (let* ((count 0)
         (lines (read-file))
         (ln-len (length (nth 1 lines))))
    (loop :for x-coord = 0 :then (mod (+ right-slope x-coord) ln-len)
          :for y-coord = 0 :then (+ down-slope y-coord)
          :until (>= y-coord (length lines))
          :when (check-tree (elt (nth y-coord lines) x-coord))
            :do
               (incf count)
          :finally (return count))))

(defun part-1 ()
  (let ((count 0))
    (loop :for ln :in (read-file) :for x-coord = 0 :then (+ 3 x-coord) :do
      (when (>= x-coord (length ln)) (setf x-coord (mod x-coord (length ln))))
      (when (check-tree (elt ln x-coord)) (incf count))
          :finally (return count))))

(defun part-2 ()
  (values
   (check-slope 1 1)
   (check-slope 3 1)
   (check-slope 5 1)
   (check-slope 7 1)
   (check-slope 1 2)
   (*
    (check-slope 1 1)
    (check-slope 3 1)
    (check-slope 5 1)
    (check-slope 7 1)
    (check-slope 1 2))))
