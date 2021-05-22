(in-package :day-13)

(defparameter *input-file* "input")

(defun read-file ()
  (uiop:read-file-lines *input-file*))

(defun part-1 ()
  (let* ((input (read-file))
         (timestamp (parse-integer (first input)))
         (ids (map 'list #'parse-integer (remove-if (lambda (str) (string= "x" str)) (uiop:split-string (elt input 1) :separator ","))))
         (arrivals (loop :for id :in ids :collect
                                         (loop :for n = timestamp :then (1+ n)
                                               :when (zerop (rem n id)) :do
                                                 (return n))))         
         (min-position (position (apply #'min arrivals) arrivals)))
    (* (elt ids min-position) (- (elt arrivals min-position) timestamp))))

(defun egcd (a b)
  (do ((r (cons b a) (cons (- (cdr r) (* (car r) q)) (car r))) ; (r+1 r) i.e. the latest is first.
       (s (cons 0 1) (cons (- (cdr s) (* (car s) q)) (car s))) ; (s+1 s)
       (u (cons 1 0) (cons (- (cdr u) (* (car u) q)) (car u))) ; (t+1 t)
       (q nil))
      ((zerop (car r)) (values (cdr r) (cdr s) (cdr u)))       ; exit when r+1 = 0 and return r s t
    (setq q (floor (/ (cdr r) (car r))))))                     ; inside loop; calculate the q
 
;;
;; Calculates the inverse module for a = 1 (mod m). 
;;
;; Note: The inverse is only defined when a and m are coprimes, i.e. gcd(a, m) = 1.”
;;
(defun invmod (a m)
  (multiple-value-bind (r s k) (egcd a m)
    (unless (= 1 r) (error "invmod: Values ~a and ~a are not coprimes." a m))  
    s))


(defun chinese-remainder (am)
"Calculates the Chinese Remainder for the given set of integer modulo pairs.
 Note: All the ni and the N must be coprimes."
  (loop :for (a . m) :in am
        :with mtot = (reduce #'* (mapcar #'(lambda(X) (cdr X)) am))
        :with sum  = 0
        :finally (return (mod sum mtot))
        :do
   (incf sum (* a (invmod (/ mtot m) m) (/ mtot m)))))


(defun part-2 ()
  (let* ((input (read-file))
         (ids (map 'list #'parse-integer (remove-if (lambda (str) (string= "x" str)) (uiop:split-string (elt input 1) :separator ","))))
         (numbers (loop :for num :in ids
                        :collect (cons num (position (write-to-string num) (uiop:split-string (elt input 1) :separator ",") :test #'string=)))))
    (chinese-remainder (cdr numbers))))

