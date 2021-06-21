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

(defun tokenize (string)
  (coerce (remove #\Space string) 'list))

(defun shunting-yard (tokens)
  (let ((operators '())
        (output '()))
    (dolist (token tokens)
      (case token
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (push token output))
        (#\*
         (loop :while (and (consp operators) (char/= #\( (car operators)) (or (char= #\+ (car operators)) (char= #\* (car operators)))) :do (push (pop operators) output) :finally (push token operators)))
        (#\+
         (loop :while (and (consp operators) (char/= #\( (car operators)) (char= #\+ (car operators))) :do (push (pop operators) output) :finally (push token operators)))
        (#\(
         (push token operators))
        (#\)
         (loop :while (char/= #\( (car operators)) :do (push (pop operators) output) :finally (pop operators)))))

    (loop :while (plusp (length operators)) :do (push (pop operators) output) :finally (return (reverse output)))))

(defun calculate-rpn (tokens)
  (let ((numbers '()))
    (dolist (token tokens)
      (case token
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (push (digit-char-p token) numbers))
        (#\+
         (push (+ (pop numbers) (pop numbers)) numbers))
        (#\*
         (push (* (pop numbers) (pop numbers)) numbers))))

    (car numbers)))

(defun calculate-line (line)
  (calculate-rpn (shunting-yard (tokenize line))))


(defun part-1 ()
  (let ((input (read-file)))
    (reduce #'+
            (map 'list #'calculate input))))

(defun part-2 ()
  (let ((input (read-file)))
    (reduce #'+
            (map 'list #'calculate-line input))))
