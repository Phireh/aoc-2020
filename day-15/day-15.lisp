(in-package :day-15)

(defun play-turn (number-list)
  (let ((last-pos (position (car number-list) (cdr number-list))))
    (if last-pos
        (push (1+ last-pos) number-list)
        (push 0 number-list))))

(defvar *input* (list 14 1 17 0 3 20))

(defun part-1 ()
  (let ((initial-numbers (reverse *input*)))
    (loop :for game-turns = initial-numbers :then (play-turn game-turns)
          :when (= 2020 (length game-turns)) :return (car game-turns))))

(defun part-2 ()
  (let ((turn-table (make-hash-table :test #'equal))
        (initial-numbers *input*)
        (turn 0))
    (mapc (lambda (x) (setf (gethash x turn-table) (incf turn))) initial-numbers)    

    (loop :for i :from (1+ turn) :upto 30000000          
          :with last-n = (car (last initial-numbers))
          :with n = 0
          :for last-turn = nil :then (gethash last-n turn-table)
          :if last-turn            
            :do               
               (setf (gethash last-n turn-table) (1- i))
               (setf n (- i last-turn 1))
          :else
            :do (setf (gethash last-n turn-table) (1- i))
                (setf n 0)
          :do
              (setf last-n n)
          :finally (return n))))
