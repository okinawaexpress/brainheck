(defun generate-program (filename)
  ;parse input by character and build list
  (let* ((program nil))
    (with-open-file (stream filename)
      (do ((character (read-char stream nil)
		      (read-char stream nil)))
	  ((null character))
	(setq program (append program (list character)))))
    program))

(defun program-init (filename)
  ;wrapper for program
  (execute-instruction 0 (let ((atape nil)) (setf atape (make-array '(10)))) 0 (generate-program filename) nil))

(defun edit-cell (tape pointer value)
  ;set value at current pointer and return tape
  (progn (setf (aref tape pointer) value) tape))

(defun execute-instruction (pointer tape pc program stack)
  ;main function
  (case (nth pc program)
    (#\> (execute-instruction (1+ pointer) tape (1+ pc) program stack))
    (#\< (execute-instruction (1- pointer) tape (1+ pc) program stack))
    (#\+ (execute-instruction pointer (edit-cell tape pointer (1+ (aref tape pointer))) (1+ pc) program stack))
    (#\- (execute-instruction pointer (edit-cell tape pointer (1- (aref tape pointer))) (1+ pc) program stack))
    (#\, (execute-instruction pointer (edit-cell tape pointer (read)) (1+ pc) program stack))
    (#\. (progn (format t "~D" (aref tape pointer)) (execute-instruction pointer tape (1+ pc) program stack)))
    (#\[ (if (eq (car (nth 0 stack)) pc)
	     (execute-instruction pointer tape (1+ pc) program stack)
	     (execute-instruction pointer tape (1+ pc) program (push (cons pc nil) stack))))
    (#\] (if (not (eq (aref tape pointer) 0))
	     (execute-instruction pointer tape (car (nth 0 stack)) program stack)
	     (execute-instruction pointer tape (1+ pc) program (cdr stack))))
    ('() tape)))
