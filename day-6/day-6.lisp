(in-package :day-6)

(defparameter *input-file* "input")

(defun read-file ()
  (uiop:read-file-string *input-file*))

(defun part-1 ()
  (let ((count 0))    
    (dolist (ln (cl-ppcre:split "\\n\\n" (read-file)) count)
      (incf count (loop :with sorted-ln = (remove #\Newline (sort ln #'char<=))                   
                        :for i :from 0 :to (- (length sorted-ln) 2)
                        :for ch1 = (elt sorted-ln i)
                        :for ch2 = (elt sorted-ln (1+ i))
                        :with ln-count = 1
                        :when (char/= ch1 ch2) :do
                          (incf ln-count)                           
                        :finally
                           (return ln-count))))))

(defun check-ln (ln)
  (let ((table (make-hash-table :test #'equalp)))
    (loop
      :for ch :across ln
      :unless (char= ch #\Newline)
        :do
           (incf (gethash ch table 0)))
    (let ((person-count (1+ (count #\Newline ln))))
      (loop :for v :being :each :hash-values :of table :using (hash-key k)
            :with count = 0
            :when (= v person-count) :do
              (incf count)
            :finally
               (return count)))))

(defun part-2 ()
  (let ((count 0))    
    (dolist (ln (cl-ppcre:split "\\n\\n" (read-file)) count)
      (incf count (check-ln (string-trim '(#\Newline) ln))))))
