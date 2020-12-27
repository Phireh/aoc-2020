(in-package :day-8)

(defparameter *input-file* "input")

(defun read-file ()
  (uiop:read-file-lines *input-file*))


(defun part-1 ()
  (let* ((program (read-file))
         (memory (make-array (length program) :element-type 'integer :initial-element 0)))
    (loop :with ln-num = 0
          :with accumulator = 0
          :for ln = (nth ln-num program)
          :for command = (subseq ln 0 3)
          :for argument = (parse-integer (subseq ln 4))
          :for ln-times = (elt memory ln-num)
          :unless (zerop ln-times) :do
            (return accumulator)
          :do
             (incf (elt memory ln-num))
             (cond
               ((string= command "nop")
                (incf ln-num))
               ((string= command "acc")
                (incf accumulator argument) (incf ln-num))
               ((string= command "jmp")
                (incf ln-num argument))))))

(defun format-line (ln)
  (list (subseq ln 0 3) (parse-integer (subseq ln 4)) 0 nil))

(defun part-2 ()
  (let ((program (map 'list #'format-line (read-file))))
    (loop :with ln-num = 0
          :with saved-state = nil
          :with accumulator = 0
          :with modified-program = nil
          :with tries = 0
          :for ln = (nth ln-num program)
          :for command = (car ln)
          :for argument = (cadr ln)
          :for ln-times = (caddr ln)
          :for changed-line = (cadddr ln)

          ; If we try to execute past the last line we have found our solution
          :when (= ln-num (length program)) :do
            (return accumulator)

          ; Restore previous state and try to change another instruction if we are looping
          :unless (zerop ln-times) :do
            (format t "Found loop on try number ~S~%" tries)
            (setf program (car saved-state))
            (setf accumulator (cadr saved-state))
            (setf ln-num (caddr saved-state))
            (setf modified-program nil)
            (setf ln (nth ln-num program))
            (setf command (car ln))
            (setf argument (cadr ln))
            (setf ln-times (caddr ln))
            (setf changed-line (cadddr ln))
            
            
          ; Change line in the program and continue execution on modified source
          ; only if we haven't tried this line and we aren't running an already modified
          ; program  
          :unless (or changed-line (string= "acc" command) modified-program) :do
            (incf tries)
            (format t "Attempting to change instruction at line ~S on try number ~S~%" ln-num tries)
            (setf changed-line (setf (cadddr ln) t))
            (setf saved-state (list (copy-tree program) accumulator ln-num))
            (setf modified-program t)
            (if (string= "nop" command)
                (setf command (setf (car ln) "jmp"))  ; switch nop -> jmp
                (setf command (setf (car ln) "nop"))) ; switch jmp -> nop


          ; Instruction dispatch
          :do
             (incf (caddr ln))
             (cond 
               ((string= command "nop")
                (incf ln-num))
               ((string= command "acc")
                (incf accumulator argument) (incf ln-num))
               ((string= command "jmp")
                (incf ln-num argument))))))

