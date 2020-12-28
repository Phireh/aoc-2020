(in-package :day-10)

(defparameter *input-file* "input")

(defun read-file ()
  (uiop:read-file-lines *input-file*))

(defun count-sublists (small-list big-list)
  (loop :with count = 0
        :with head-ptr = 0
        :for search-space = (nthcdr head-ptr big-list)
        :for search-result = (search small-list search-space)
        :when search-result :do
          (incf count) (incf head-ptr (+ search-result (1- (length small-list))))
        :unless search-result :do
          (return count)))

(defun part-1 ()
  (let* ((numbers (nconc (list 0) (sort (map 'list #'parse-integer (read-file)) #'<)))
         (differences (nconc (map 'list #'- (cdr numbers) numbers) (list 3))))
    (break)
    (* (count 1 differences) (count 3 differences))))

(defun part-2 ()
  (let* ((numbers (nconc (list 0) (sort (map 'list #'parse-integer (read-file)) #'<)))
         (differences (nconc (list 3) (map 'list #'- (cdr numbers) numbers) (list 3))))
    (* (expt 7 (count-sublists '(3 1 1 1 1 3) differences))
       (expt 4 (count-sublists '(3 1 1 1 3) differences))
       (expt 2 (count-sublists '(3 1 1 3) differences)))))
