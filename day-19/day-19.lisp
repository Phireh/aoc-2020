(in-package #:day-19)

(defparameter *input-file* "input")

(defun read-file ()
  (uiop:read-file-lines *input-file*))

(defun read-rules (lines)
  (let ((rules '())
        (tests '()))
    (loop :for line :in lines
          :with reading-rules = t
          :if (string= "" line) :do
            (setf reading-rules nil)
          :else :do
            (if reading-rules (push line rules) (push line tests)))
    (values rules tests)))

(defun rule-to-regex (rule table)
  (format nil "(~{~A~})"
          (loop :for member :in rule :collect            
            (cond
              ((parse-integer member :junk-allowed t) (rule-to-regex (gethash member table) table))
              (t member)))))

(defun rule-to-regex-part-2 (n table)    
  (cond
    ;; HACK: Hard-coded solutions
    ((string= n "8")
     (format nil "(~A)+" (rule-to-regex-part-2 "42" table)))
    ((string= n "11")
     (let ((rule-42 (format nil "(~A)" (rule-to-regex-part-2 "42" table)))
           (rule-31 (format nil "(~A)" (rule-to-regex-part-2 "31" table))))
       (concatenate 'string
                    "("
                    "(" rule-42 rule-31 ")"
                    "|"
                    "(" rule-42 rule-42 rule-31 rule-31 ")"
                    "|"
                    "(" rule-42 rule-42 rule-42 rule-31 rule-31 rule-31 ")"
                    "|"
                    "(" rule-42 rule-42 rule-42 rule-42 rule-31 rule-31 rule-31 rule-31 ")"
                    ")")
       ))
    
    (t
     (format nil "(~{~A~})"
             (loop :for member :in (gethash n table) :collect
                                                     (cond
                                                       ((parse-integer member :junk-allowed t) (rule-to-regex-part-2 member table))
                                                       (t member)))))))

(defun part-1 ()
  (let ((rules (make-hash-table :test #'equalp)))
    (multiple-value-bind (lines tests) (read-rules (read-file))      
      (loop :for line :in lines
            :for key = (subseq line 0 (position #\: line))
            :for rule = (uiop:split-string (remove #\" (subseq line (+ 2 (position #\: line)))))
            :do
               (setf (gethash key rules) rule))
      (let ((language-regex (ppcre:create-scanner
                             (concatenate 'string "^" (rule-to-regex (gethash "0" rules) rules) "$"))))
        (reduce #'+ (map 'list (lambda (test) (if (ppcre:scan language-regex test) 1 0)) tests))))))

(defun part-2 ()
  (let ((rules (make-hash-table :test #'equalp)))
    (multiple-value-bind (lines tests) (read-rules (read-file))      
      (loop :for line :in lines
            :for key = (subseq line 0 (position #\: line))
            :for rule = (uiop:split-string (remove #\" (subseq line (+ 2 (position #\: line)))))
            :do
               (setf (gethash key rules) rule))
      (let ((language-regex (ppcre:create-scanner
                             (concatenate 'string "^" (rule-to-regex-part-2 "0" rules) "$"))))
        (reduce #'+ (map 'list (lambda (test) (if (ppcre:scan language-regex test) 1 0)) tests))))))
