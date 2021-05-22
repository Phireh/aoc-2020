(in-package :day-12)

(defparameter *input-file* "input")

(defun read-file ()
  (uiop:read-file-lines *input-file*))

(defvar rotation 90)
(defvar x-pos 0)
(defvar y-pos 0)

(defvar ship-x-pos 0)
(defvar ship-y-pos 0)

;; Relative to the ship
(defvar target-x-pos 0)
(defvar target-y-pos 0)


(defun move (&key north east south west forward)
  (when north (incf y-pos north))
  (when east (incf x-pos east))
  (when south (decf y-pos south))
  (when west (decf x-pos west))
  (when forward
    (case rotation
      ((0)   (move :north forward))
      ((90)  (move :east  forward))
      ((180) (move :south forward))
      ((270) (move :west  forward)))))

(defun move-part-2 (&key north east south west forward)
  (when north (incf target-y-pos north))
  (when east (incf target-x-pos east))
  (when south (decf target-y-pos south))
  (when west (decf target-x-pos west))
  (when forward
    (incf ship-x-pos (* target-x-pos forward))
    (incf ship-y-pos (* target-y-pos forward))))

(defun rotate (rot)
  (incf rotation rot)
  (when (>= rotation 360)
    (decf rotation 360))
  (when (< rotation 0)
    (incf rotation 360)))

(defun rotate-part-2 (rot)
  (case rot
    ((90 -270)
     (rotatef target-x-pos target-y-pos)
     (setf target-y-pos (- target-y-pos)))
    ((180 -180)
     (setf target-x-pos (- target-x-pos))
     (setf target-y-pos (- target-y-pos)))
    ((270 -90)
     (rotatef target-x-pos target-y-pos)
     (setf target-x-pos (- target-x-pos)))))

(defun part-1 ()
  (setf x-pos 0)
  (setf y-pos 0)
  (setf rotation 90)
  (let ((instructions (read-file)))
    (loop :for line :in instructions
          :for inst = (elt line 0)
          :for n = (parse-integer line :start 1)
          :do
             (case inst
               ((#\N) (move :north n))
               ((#\E) (move :east n))
               ((#\S) (move :south n))
               ((#\W) (move :west n))
               ((#\L) (rotate (- n)))
               ((#\R) (rotate n))
               ((#\F) (move :forward n)))))
  (+ (abs x-pos) (abs y-pos)))

(defun part-2 ()
  (setf ship-x-pos 0)
  (setf ship-y-pos 0)
  (setf target-x-pos 10)
  (setf target-y-pos 1)
  (let ((instructions (read-file)))
    (loop :for line :in instructions
          :for inst = (elt line 0)
          :for n = (parse-integer line :start 1)
          :do
             (case inst
               ((#\N) (move-part-2 :north n))
               ((#\E) (move-part-2 :east n))
               ((#\S) (move-part-2 :south n))
               ((#\W) (move-part-2 :west n))
               ((#\L) (rotate-part-2 (- n)))
               ((#\R) (rotate-part-2 n))
               ((#\F) (move-part-2 :forward n)))))
  (+ (abs ship-x-pos) (abs ship-y-pos)))
