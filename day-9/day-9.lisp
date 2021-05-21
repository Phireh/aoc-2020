(in-package :day-9)

(defparameter *input-file* "input")

(defun read-file ()
  (uiop:read-file-lines *input-file*))

(defun check-sum (n1 n2 n3)
  (= n3 (+ n1 n2)))

(defun check-sum-list (number-list number)
  (loop :named outer-loop :for i :from 0 :to (1- (length number-list))
        :do
           (loop :for j :from (1+ i) :to (1- (length number-list))
                 :for n1 = (nth i number-list)
                 :for n2 = (nth j number-list)
                 :when (check-sum n1 n2 number)
                   :do (return-from outer-loop t)
                 :finally (return nil))))

(defun part-1 ()
  (let ((numbers (map 'list #'parse-integer (read-file))))
    (loop :for number-list = (subseq numbers 0 25)
          :for number = (nth 25 numbers)
          :do (pop numbers)
;              (format t "Trying with list ~A and number ~A~%" number-list number)
          :unless (check-sum-list number-list number)
            :do
               (return number))))

(defun part-2 ()
  (let ((numbers (map 'list #'parse-integer (read-file)))
        (target (part-1)))

    (loop :named outer-loop
          :for i :from 0 :to (1- (length numbers))
          :do
             (loop :for j :from i :to (length numbers)
                   :for sublist = (subseq numbers i j)
                   :for local-sum = (reduce #'+ sublist)
                   :when (= local-sum target) :do
                     (return-from outer-loop
                       (+ (reduce #'min sublist) (reduce #'max sublist)))))))
