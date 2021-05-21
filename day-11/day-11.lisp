(in-package :day-11)

(defparameter *input-file* "input")

(defvar seat-arr nil)
(defvar line-len 0)
(defvar line-count 0)

(defun read-file ()
  (uiop:read-file-lines *input-file*))

(defun make-seat-array ()
  (let ((lines (read-file)))
    (setf line-count (length lines))
    (setf line-len (length (car lines)))
    (loop :for l :in lines
          :for i = 0 :then (1+ i)
          :with arr = (make-array (list line-count line-len) :element-type 'character)
          :do
             (loop :for c :across l
                   :for j = 0 :then (1+ j)
                   :do               
                      (setf (row-major-aref arr (array-row-major-index arr i j)) c))
          :finally
             (setf seat-arr arr))))

;; Gets the seat at position [i,j] in row-major order. Assumes valid input
(defun seat (i j)
  (row-major-aref seat-arr (array-row-major-index seat-arr i j)))

;; Gets the list of adjacent seats around [i,j] as a character list
(defun adjacent-seats (i j)
  (let ((adj-seat-list '()))
    ;; Up+left adjacent seat
    (when (and (> i 0) (> j 0))
      (push (seat (1- i) (1- j)) adj-seat-list))
    ;; Up adjacent seat
    (when (> i 0)
      (push (seat (1- i) j) adj-seat-list))
    ;; Up+right adjacent seat
    (when (and (> i 0) (< j (1- line-len)))
      (push (seat (1- i) (1+ j)) adj-seat-list))
    ;; Left adjacent seat
    (when (> j 0)
      (push (seat i (1- j)) adj-seat-list))
    ;; Right adjacent seat
    (when (< j (1- line-len))
      (push (seat i (1+ j)) adj-seat-list))
    ;; Down+left adjacent seat
    (when (and (< i (1- line-count)) (> j 0))
      (push (seat (1+ i) (1- j)) adj-seat-list))
    ;; Down adjacent seat
    (when (< i (1- line-count))
      (push (seat (1+ i) j) adj-seat-list))
    ;; Down+right adjacent seat
    (when (and (< i (1- line-count)) (< j (1- line-len)))
      (push (seat (1+ i) (1+ j)) adj-seat-list))
    adj-seat-list))

(defun adjacent-seats-part-2 (i j)
  (let ((adj-seat-list '()))
    ;; Up+left direction
    (loop :for row = (1- i) :then (1- row)
          :for col = (1- j) :then (1- col)
          :if (or (< row 0) (< col 0)) :do
            (return nil)
          :else :do
            (when (char/= (seat row col) #\.)
              (push (seat row col) adj-seat-list)
              (return)))
    
    ;; Up direction
    (loop :for row = (1- i) :then (1- row)
          :with col = j
          :if (< row 0) :do
            (return nil)
          :else :do
            (when (char/= (seat row col) #\.)
              (push (seat row col) adj-seat-list)
              (return)))

    ;; Up+right direction
    (loop :for row = (1- i) :then (1- row)
          :for col = (1+ j) :then (1+ col)
          :if (or (< row 0) (> col (1- line-len))) :do
            (return nil)
          :else :do
            (when (char/= (seat row col) #\.)
              (push (seat row col) adj-seat-list)
              (return)))

    ;; Left direction
    (loop :with row = i
          :for col = (1- j) :then (1- col)
          :if (< col 0) :do
            (return nil)
          :else :do
            (when (char/= (seat row col) #\.)
              (push (seat row col) adj-seat-list)
              (return)))

    ;; Right direction
    (loop :with row = i
          :for col = (1+ j) :then (1+ col)
          :if (> col (1- line-len)) :do
            (return nil)
          :else :do
            (when (char/= (seat row col) #\.)
              (push (seat row col) adj-seat-list)
              (return)))

    ;; Down+left direction
    (loop :for row = (1+ i) :then (1+ row)
          :for col = (1- j) :then (1- col)
          :if (or (> row (1- line-count)) (< col 0)) :do
            (return nil)
          :else :do
            (when (char/= (seat row col) #\.)
              (push (seat row col) adj-seat-list)
              (return)))

    ;; Down direction
    (loop :for row = (1+ i) :then (1+ row)
          :with col = j
          :if (> row (1- line-count)) :do
            (return nil)
          :else :do
            (when (char/= (seat row col) #\.)
              (push (seat row col) adj-seat-list)
              (return)))

    ;; Down+right direction
    (loop :for row = (1+ i) :then (1+ row)
          :for col = (1+ j) :then (1+ col)
          :if (or (> row (1- line-count)) (> col (1- line-len))) :do
            (return nil)
          :else :do
            (when (char/= (seat row col) #\.)
              (push (seat row col) adj-seat-list)
              (return)))    
    
    adj-seat-list))

;; Returns a new iteration of the seats
(defun iterate-seats ()
  (let ((new-iteration (make-array (list line-count line-len) :element-type 'character)))
    (loop :for i :from 0 :to (1- line-count)
          :do
             (loop :for j :from 0 :to (1- line-len)
                   :for adj-seats = (adjacent-seats i j)
                   :for c = (seat i j)
                   :with new-c = #\.
                   :when (char= c #\L) :do
                     (if (= (count #\# adj-seats) 0)
                         (setf new-c #\#)
                         (setf new-c #\L))
                   :when (char= c #\#) :do
                     (if (>= (count #\# adj-seats) 4)
                         (setf new-c #\L)
                         (setf new-c #\#))
                   :when (char= c #\.) :do
                     (setf new-c #\.)
                   :do
                      (setf (row-major-aref new-iteration (array-row-major-index new-iteration i j)) new-c))
          :finally (return new-iteration))))

;; Returns a new iteration of the seats
(defun iterate-seats-part-2 ()
  (let ((new-iteration (make-array (list line-count line-len) :element-type 'character)))
    (loop :for i :from 0 :to (1- line-count)
          :do
             (loop :for j :from 0 :to (1- line-len)
                   :for adj-seats = (adjacent-seats-part-2 i j)
                   :for c = (seat i j)
                   :with new-c = #\.
                   :when (char= c #\L) :do
                     (if (= (count #\# adj-seats) 0)
                         (setf new-c #\#)
                         (setf new-c #\L))
                   :when (char= c #\#) :do
                     (if (>= (count #\# adj-seats) 5)
                         (setf new-c #\L)
                         (setf new-c #\#))
                   :when (char= c #\.) :do
                     (setf new-c #\.)
                   :do
                      (setf (row-major-aref new-iteration (array-row-major-index new-iteration i j)) new-c))
             :finally (return new-iteration))))

(defun seat-array= (arr1 arr2)
  (loop :named outer-loop :for i :from 0 :to (1- line-count) :do
    (loop :named inner-loop :for j :from 0 :to (1- line-len)
          :unless (char= (row-major-aref arr1 (array-row-major-index arr1 i j))
                         (row-major-aref arr2 (array-row-major-index arr2 i j)))
            :do
               (return-from outer-loop nil))
        :finally
           (return-from outer-loop t)))

(defun occupied-seats ()
  (loop :for i :from 0 :to (1- line-count) :with n = 0 :do
    (loop :for j :from 0 :to (1- line-len)
          :when (char= (seat i j) #\#) :do
            (incf n))
        :finally (return n)))

(defun part-1 ()
  (make-seat-array)
  
  (loop :for i = 1 :then (1+ i)
        :for new-iteration = (iterate-seats)
        :if (seat-array= seat-arr new-iteration) :do
          (setf seat-arr new-iteration)          
          (return (values i (occupied-seats)))
        :else :do
           (setf seat-arr new-iteration)))

(defun part-2 ()
  (make-seat-array)
  (loop :for i = 1 :then (1+ i)
        :for new-iteration = (iterate-seats-part-2)
        :if (seat-array= seat-arr new-iteration) :do
          (setf seat-arr new-iteration)          
          (return (values i (occupied-seats)))
        :else :do
          (setf seat-arr new-iteration)))
