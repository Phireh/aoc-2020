(in-package :day-14)

(defparameter *input-file* "input")

(defun read-file ()
  (uiop:read-file-lines *input-file*))

(defun index (instruction)
  (parse-integer (subseq instruction (1+ (search "[" instruction)) (search "]" instruction))))

(defun number-to-bits (number mask)
  (reverse (loop :for index :below (max (length mask) (integer-length number))
                 :collect (if (char= #\X (elt (reverse mask) index))
                              (if (logbitp index number) 1 0)
                              (digit-char-p (elt (reverse mask) index))))))

(defun apply-mask (number mask)
  (reduce (lambda (a b) (+ (ash a 1) b)) (number-to-bits number mask)))

(defun modify-char (str new-char)
  (reverse (concatenate 'string (loop :for ch :across (reverse str) :with done = nil
                                              :if (and (not done) (char= ch #\X))
                                                :do (setf done t)
                                                :and
                                                  :collect new-char
                                              :else
                                                :collect ch))))

(defun part-1 ()
  (let ((instructions (read-file))
        (memory-table (make-hash-table :test #'equal)))
    (loop :for current-line :in instructions
          :with mask = ""
          :if (search "mask = " current-line) :do
            (setf mask (subseq current-line (+ 2 (search "= " current-line))))
          :else :do
            (setf (gethash (index current-line) memory-table)
                  (apply-mask (parse-integer (subseq current-line (+ 2 (search "= " current-line)))) mask)))

    (loop :for v :being :the :hash-value :in memory-table :sum v)))

(defun permutations (number mask)
  (if (search "X" mask) (list
                         (permutations number (modify-char mask #\1))
                         (permutations number (modify-char mask #\2)))
      (reduce (lambda (a b) (+ (ash a 1) b)) (reverse
                  (loop :for index :below (max (length mask) (integer-length number))
                        :collect
                        (case (elt (reverse mask) index)
                          (#\0 (if (logbitp index number) 1 0)) ; leave unchanged
                          (#\1 1)       ; overwrite 1
                          (#\2 0)))))))  ; overwrite 0

(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

(defun part-2 ()
  (let ((instructions (read-file))
        (memory-table (make-hash-table :test #'equal)))
    (loop :for current-line :in instructions
          :with mask = ""
          :if (search "mask = " current-line) :do
            (setf mask (subseq current-line (+ 2 (search "= " current-line))))
          :else :do
            (loop :for address :in (flatten (permutations (index current-line) mask))
                  :do (setf (gethash address memory-table) (parse-integer (subseq current-line (+ 2 (search "= " current-line)))))))
    (loop :for v :being :the :hash-value :in memory-table :sum v)))
