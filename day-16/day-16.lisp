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

(defun part-2 ()
  (let ((input-lines (read-file))
        (rules '())
        (tickets '())
        (my-ticket '()))
    (loop :for line :in input-lines
          :until (string= "" line)
          :do
             (push (list
                    (subseq line 0 (search ":" line))
                    (cons (parse-integer (subseq line (+ 2 (search ": " line)) (search "-" line)))
                          (parse-integer (subseq line (1+ (search "-" line)) (search " or" line))))
                    (cons (parse-integer (subseq line (+ 4 (search " or " line)) (search "-" line :from-end t)))
                          (parse-integer (subseq line (1+ (search "-" line :from-end t))))))
                   rules)
          
          )
    (setf tickets (loop :for line :in input-lines
                        :with reading-numbers = nil
                        :when reading-numbers            
                          :collect (map 'list #'parse-integer (uiop:split-string line :separator ","))
                        :when (string= "nearby tickets:" line)            
                          :do (setf reading-numbers t)))
    
    (setf my-ticket (car (loop :for line :in input-lines
                               :with reading-ticket = nil
                               :when (zerop (length line))
                                 :do (setf reading-ticket nil)
                               :when reading-ticket
                                 :collect (map 'list #'parse-integer (uiop:split-string line :separator ","))
                               :when (string= "your ticket:" line)
                                 :do (setf reading-ticket t))))

    ;; Remove unvalid tickets
    (setf tickets
          (remove-if
          (lambda (ticket) (position nil ticket))
          (map 'list
               (lambda (ticket)
                 (map 'list
                      (lambda (n)
                        (if (remove-if-not (lambda (rule)
                                             (or (<= (caadr rule) n (cdadr rule))
                                                 (<= (caaddr rule) n (cdaddr rule)))) rules) n nil))
                      ticket))
               tickets)))
    (setf rules
          (map 'list
               (lambda (rule) (list rule (loop :for i :below (length (first tickets))
                                               :for success = t :then t
                                               :do (mapc (lambda (ticket) (let ((n (nth i ticket)))                                                                      
                                                                            (unless
                                                                                (or (<= (caadr rule) n (cdadr rule))
                                                                                    (<= (caaddr rule) n (cdaddr rule)))
                                                                              (setf success nil))))
                                                         tickets)
                                               :when success :collect i)))
               rules))

    (labels ((decide (rule-list &optional (fixed-positions '()))
               (loop :for rule :in rule-list
                     :for i = 0 :then (1+ i)
                     :with min-length = 99
                     :with max-length = 0
                     :with min-pos = 0
                     :with max-pos = 0
                     :with n = 0
                     :when (> (length (cadr rule)) max-length)
                       :do
                          (setf max-length (length (cadr rule)))
                          (setf max-pos i)
                     :when (and (< (length (cadr rule)) min-length) (not (position i fixed-positions)))
                       :do
                          (setf min-length (length (cadr rule)))
                          (setf min-pos i)
                          (setf n (caadr rule))
                     :finally
                        (if (= max-length 1)
                            (return rule-list)
                            (return (decide (loop :for r :in rule-list
                                                  :for j = 0 :then (1+ j)
                                                  :if (= min-pos j)
                                                    :collect r                                                                                              
                                                  :else
                                                    :collect (list (car r) (remove n (cadr r)))) (nconc fixed-positions (list min-pos))))))
               
               ))
      ;; end of definition
      (setf rules (decide rules)))

    (reduce #'*
     (map 'list
          (lambda (pos) (nth pos my-ticket))
          (map 'list
               #'caadr
               (remove-if-not (lambda (r) (search "departure" (caar r))) rules))))

    ))
