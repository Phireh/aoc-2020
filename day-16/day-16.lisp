(in-package #:day-16)

(defparameter *input-file* "input")

(defun read-file ()
  (uiop:read-file-lines *input-file*))

(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

(defun part-1 ()
  (let ((input-lines (read-file))
        (rules '())
        (numbers '()))
    (loop :for line :in input-lines
          :until (string= "" line)
          :do
             (push (cons (parse-integer (subseq line (+ 2 (search ": " line)) (search "-" line)))
                         (parse-integer (subseq line (1+ (search "-" line)) (search " or" line))))
                   rules)
             (push (cons (parse-integer (subseq line (+ 4 (search " or " line)) (search "-" line :from-end t)))
                         (parse-integer (subseq line (1+ (search "-" line :from-end t)))))
                   rules))
    (setf numbers (flatten (loop :for line :in input-lines
                                 :with reading-numbers = nil
                                 :when reading-numbers            
                                   :collect (map 'list #'parse-integer (uiop:split-string line :separator ","))
                                 :when (string= "nearby tickets:" line)            
                                   :do (setf reading-numbers t))))
    (reduce #'+ (map 'list
                       (lambda (n)
                         (if (remove-if-not (lambda (rule) (<= (car rule) n (cdr rule))) rules) 0 n))
                       numbers))))
