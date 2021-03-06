(defun parallel-test ()
  (let ((list nil)
        (lock (make-lock "list-lock")))
    (process-run-function "pr1" (lambda () (add-to-list list 'a 'b 'c lock)))
    (process-run-function "pr2" (lambda () (add-to-list list 'd 'e 'f lock)))
    (process-run-function "pr3" (lambda () (add-to-list list 'g 'h 'i lock)))
    (sleep 10)
    list))

(defun add-to-list (list t1 t2 t3 lock)
  (with-lock-grabbed (lock)
    (setf list (append list (list t1 t2 t3)))))
