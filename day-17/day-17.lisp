(in-package #:day-17)

(defparameter *input-file* "input")

(defun read-file ()
  (uiop:read-file-lines *input-file*))

(defun alivep (ch)
  (when ch (char= #\# ch)))

(defun alive-neighbours (pos world)
  (let ((x (first pos))
        (y (second pos))
        (z (third pos)))

    (+ (if (alivep (gethash (list (1- x) (1+ y) (1+ z)) world)) 1 0)
       (if (alivep (gethash (list x      (1+ y) (1+ z)) world)) 1 0)
       (if (alivep (gethash (list (1+ x) (1+ y) (1+ z)) world)) 1 0)
       (if (alivep (gethash (list (1- x) y      (1+ z)) world)) 1 0)
       (if (alivep (gethash (list x      y      (1+ z)) world)) 1 0)
       (if (alivep (gethash (list (1+ x) y      (1+ z)) world)) 1 0)
       (if (alivep (gethash (list (1- x) (1- y) (1+ z)) world)) 1 0)
       (if (alivep (gethash (list x      (1- y) (1+ z)) world)) 1 0)
       (if (alivep (gethash (list (1+ x) (1- y) (1+ z)) world)) 1 0)

       (if (alivep (gethash (list (1- x) (1+ y) z) world)) 1 0)
       (if (alivep (gethash (list x      (1+ y) z) world)) 1 0)
       (if (alivep (gethash (list (1+ x) (1+ y) z) world)) 1 0)
       (if (alivep (gethash (list (1- x) y      z) world)) 1 0)
       
       (if (alivep (gethash (list (1+ x) y      z) world)) 1 0)
       (if (alivep (gethash (list (1- x) (1- y) z) world)) 1 0)
       (if (alivep (gethash (list x      (1- y) z) world)) 1 0)
       (if (alivep (gethash (list (1+ x) (1- y) z) world)) 1 0)

       (if (alivep (gethash (list (1- x) (1+ y) (1- z)) world)) 1 0)
       (if (alivep (gethash (list x      (1+ y) (1- z)) world)) 1 0)
       (if (alivep (gethash (list (1+ x) (1+ y) (1- z)) world)) 1 0)
       (if (alivep (gethash (list (1- x) y      (1- z)) world)) 1 0)
       (if (alivep (gethash (list x      y      (1- z)) world)) 1 0)
       (if (alivep (gethash (list (1+ x) y      (1- z)) world)) 1 0)
       (if (alivep (gethash (list (1- x) (1- y) (1- z)) world)) 1 0)
       (if (alivep (gethash (list x      (1- y) (1- z)) world)) 1 0)
       (if (alivep (gethash (list (1+ x) (1- y) (1- z)) world)) 1 0))))

(defun alive-neighbours-4d (pos world)
  (let ((x (first pos))
        (y (second pos))
        (z (third pos))
        (w (fourth pos)))
    
    (loop :for i :from (1- x) :upto (1+ x)
          :with count = 0
          :do
             (loop :for j :from (1- y) :upto (1+ y) :do
               (loop :for k :from (1- z) :upto (1+ z) :do
                 (loop :for l :from (1- w) :upto (1+ w)
                       :unless (and (= x i) (= y j) (= z k) (= l w)) :do
                         (incf count (if (alivep (gethash (list i j k l) world)) 1 0)))))
          :finally (return count))))

(defun unvisited-neighbours (key world)
  (let ((x (first key))
        (y (second key))
        (z (third key))
        (neighbour-list '()))
    (loop :for i :from (1- x) :upto (1+ x) :do      
      (loop :for j :from (1- y) :upto (1+ y) :do
        (loop :for k :from (1- z) :upto (1+ z)
              :unless (gethash (list i j k) world)
                :do (push (list i j k) neighbour-list))))
    neighbour-list))

(defun unvisited-neighbours-4d (key world)
  (let ((x (first key))
        (y (second key))
        (z (third key))
        (w (fourth key))
        (neighbour-list '()))

    (loop :for i :from (1- x) :upto (1+ x) :do
      (loop :for j :from (1- y) :upto (1+ y) :do
        (loop :for k :from (1- z) :upto (1+ z) :do
          (loop :for l :from (1- w) :upto (1+ w)
                :unless (gethash (list i j k l) world)
                  :do (push (list i j k l) neighbour-list))))
          :finally (return neighbour-list))))

(defun iterate-game (world)
  (let ((unvisited-cells '()))
    (loop :for k :being :the :hash-keys :in world :using (hash-value v)
          :when (char= #\# v) :do (setf unvisited-cells (nconc unvisited-cells (unvisited-neighbours k world))))

    (mapc (lambda (pos) (setf (gethash pos world) #\.)) unvisited-cells)

    

    (mapc (lambda (order) (setf (gethash (first order) world) (second order)))
         (loop :for k :being :the :hash-keys :in world :using (hash-value v)
               :collect (if (char= #\# v)
                            (if (<= 2 (alive-neighbours k world) 3)
                                (list k #\#)   ; If we are alive and have 2-3 neighbours, stay alive
                                (list k #\.))  ; Otherwise die
                            (if (= (alive-neighbours k world) 3)
                                (list k #\#)   ; If we are dead and have 3 neighbours, become alive
                                (list k #\.))) ; Otherwise stay dead

               ))

    world))

(defun iterate-game-4d (world)
  (let ((unvisited-cells '()))
    (loop :for k :being :the :hash-keys :in world :using (hash-value v)
          :when (char= #\# v) :do (setf unvisited-cells (nconc unvisited-cells (unvisited-neighbours-4d k world))))

    
    (mapc (lambda (pos) (setf (gethash pos world) #\.)) unvisited-cells)

    

    (mapc (lambda (order) (setf (gethash (first order) world) (second order)))
         (loop :for k :being :the :hash-keys :in world :using (hash-value v)
               :collect (if (char= #\# v)
                            (if (<= 2 (alive-neighbours-4d k world) 3)
                                (list k #\#)   ; If we are alive and have 2-3 neighbours, stay alive
                                (list k #\.))  ; Otherwise die
                            (if (= (alive-neighbours-4d k world) 3)
                                (list k #\#)   ; If we are dead and have 3 neighbours, become alive
                                (list k #\.))) ; Otherwise stay dead

               ))

    world))

(defun part-1 ()
  (let ((initial-slice (read-file))
        (world (make-hash-table :test #'equal)))

    (loop :for line :in initial-slice
          :for i = 0 :then (1+ i)
          :do
             (loop :for ch :across line
                   :for j = 0 :then (1+ j)
                   :do
                      (setf (gethash (list i j 1) world) ch)))

    (loop :for i :from 0 :below 6
          :do (setf world (iterate-game world))
          :finally (return (loop :for k :being :the :hash-keys :in world :using (hash-value v)
                                 :with count = 0
                                 :if (char= #\# v)
                                   :do (incf count)
                                 :finally (return count))))))

(defun part-2 ()
  (let ((initial-slice (read-file))
        (world (make-hash-table :test #'equal)))

    (loop :for line :in initial-slice
          :for i = 0 :then (1+ i)
          :do
             (loop :for ch :across line
                   :for j = 0 :then (1+ j)
                   :do
                      (setf (gethash (list i j 1 1) world) ch)))


    (loop :for i :from 0 :below 6
          :do (setf world (iterate-game-4d world))
          :finally (return (loop :for k :being :the :hash-keys :in world :using (hash-value v)
                                 :with count = 0
                                 :if (char= #\# v)
                                   :do (incf count)
                                 :finally (return count))))
      )
  )
