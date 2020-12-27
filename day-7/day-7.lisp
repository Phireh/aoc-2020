(in-package :day-7)

(defparameter *input-file* "input")

(defun read-file ()
  (uiop:read-file-lines *input-file*))

(defun check-color (color table top &optional target)
  (let ((colorlist (gethash color table)))
    (if (and (not top) (zerop (length colorlist)))
        0
        (if top
            (loop :for v :being :the :hash-keys :of table
                  :for result =
                   (check-color v table nil color)
                  :sum (check-color v table nil color))
            (loop :for c :in colorlist
                  :with found = 0
                  :when (string= c target) :do
                    (return 1)
                  :unless (= found 1) :do
                    (setf found (check-color c table nil target))
                  :finally
                     (return found))))))

(defun count-bags (color table top n)
  (let ((colorlist (gethash color table)))
    (if (zerop (length colorlist))
        n
        (loop :for tuple :in colorlist
              :with result = 0
              :do (incf result (* n (count-bags (cadr tuple) table nil (car tuple))))
              :finally (return (+ (or (and top 0) n) result))))))

  (defun part-1 ()
    (let ((table (make-hash-table :test #'equalp)))
      (dolist (ln (read-file))
        (register-groups-bind (source contents) ("(.+) bags contain (.+)" ln)
          (cl-ppcre:do-matches-as-strings (match "\\d ([^,]+) bag" contents)
            (setf (gethash source table '()) (push (subseq match 2 (- (length match) 4)) (gethash source table '()))))))
      (check-color "shiny gold" table t)))

(defun part-2 ()
  (let ((table (make-hash-table :test #'equalp)))
    (dolist (ln (read-file))
      (register-groups-bind (source contents) ("(.+) bags contain (.+)" ln)
        (cl-ppcre:do-matches-as-strings (match "(\\d [^,]+) bag" contents)
          (setf (gethash source table '()) (push (list (parse-integer (subseq match 0 1)) (subseq match 2 (- (length match) 4))) (gethash source table '()))))))
    (count-bags "shiny gold" table t 1)))

