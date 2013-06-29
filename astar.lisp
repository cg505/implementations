;; Key to numbers
;; 0 untouched
;; 1 start
;; 2 finish
;; 3 visited
;; 4 active
;; 5 wall
(defun init-plane ()
  (let ((plane (make-array '(6 6))))
    (setf (aref plane 1 1) 1)
    (setf (aref plane  5 4) 2)
    (setf (aref plane 2 4) 5)
    (setf (aref plane 3 3) 5)
    (setf (aref plane 4 2) 5)
    (print-plane plane)))

;; -------------
;;|             |
;;|   S         |
;;|             |
;;|             |
;;|             |
;;|         F   |
;; -------------

(defun print-plane (plane)
  (format t " -")
  (dotimes (w (array-dimension plane 1))
    (format t "--"))
  (format t "~%")
  (dotimes (h (array-dimension plane 0))
    (format t "| ")
    (dotimes (w (array-dimension plane 1))
      (format t "~a " (case (aref plane h w)
                        (0 " ")
                        (1 "S")
                        (2 "F")
                        (3 "X")
                        (4 "O")
                        (5 "W")
                        (t "?"))))
    (format t "|~%"))
  (format t " -")
  (dotimes (w (array-dimension plane 1))
    (format t "--"))
  (format t "~%")
  plane)

(defun g (x y)
  ;; distance formula
  (sqrt (+ (* x x) (* y y))))

;; a heap is (cons heap-array number-of-elements)

(defun heap-init (size)
  (cons (make-array (list size)) 0))

(defun heap-insert (heap value)
  (setf (aref (car heap) (cdr heap)) value)
  (setf (cdr heap) (1+ (cdr heap)))
  (heap-sort-up heap (1- (cdr heap))))

(defun heap-sort-up (heap index)
  (format t "~a, ~a~%" index (1- (floor (1+ index) 2)))
  (if (and (not (= index 0))
           (> (aref (car heap) index) (aref (car heap) (1- (floor (1+ index) 2)))))
      (let ((goingup (aref (car heap) index)))
        (setf (aref (car heap) index) (aref (car heap) (1- (floor (1+ index) 2))))
        (setf (aref (car heap) (1- (floor (1+ index) 2))) goingup)
        (heap-sort-up heap (1- (floor (1+ index) 2))))
    ;; base case: just return heap if alread sorted
    heap))

(defun heap-pop (heap)
  (let ((popped (aref (car heap) 0)))
    (setf (aref (car heap) 0) (aref (car heap) (1- (cdr heap))))
    (decf (cdr heap))
    (heap-sort-down heap 0)
    popped))

(defun heap-sort-down (heap index)
  (if (>= (* 2 (1+ index)) (cdr heap))
      heap ;; base case
    (let ((new-index nil))
      (if (> (aref (car heap) (1- (* 2 (1+ index))))
             (aref (car heap)     (* 2 (1+ index)))) ;; second, no 1- so actually 1+
          ;; first is larger
          (setf new-index (1- (* 2 (1+ index))))
        ;; second is larger
        (setf new-index (* 2 (1+ index))))
      (let ((goingdown (aref (car heap) index)))
        (setf (aref (car heap) index)
              (aref (car heap) new-index))
        (setf (aref (car heap) new-index) goingdown))
      (heap-sort-down heap new-index))))


;; distracted/bored
;; good way to blow your stack
(defun ackermann (m n)
  (cond
   ((= m 0)
    (1+ n))
   ((and (> m 0) (= n 0))
    (ackermann (1- m) 1))
   ((and (> m 0) (> n 0))
    (ackermann (1- m) (ackermann m (1- n))))
   (t
    (format t "Houston, we have a problem.~%"))))
