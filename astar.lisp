;; Key to numbers
;; 0 untouched
;; 1 start
;; 2 finish
;; 3 visited
;; 4 active
;; 5 wall
(defun init-plane ()
  (let ((plane (make-array '(10 10))))
    (setf (aref plane 1 1) 1)
    (setf (aref plane 8 9) 2)
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
      (format t "~a " (case (aref plane w h)
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

(defun h (x y)
  ;; distance formula
  (sqrt (+ (expt (- 8 x) 2) (expt (- 9 y) 2))))

;; a heap is (cons heap-array number-of-nodes)
;; it is an array comprised of nodes
;; a node is (cons thing priority)
;; the node at the top of the tree is at the top of the tree
;; the two children of a node with index n are at 2n and 2n+1, when indices start at 1 (:[)

(defun heap-init (size)
  (cons (make-array (list size)) 0))

(defun heap-insert (heap value priority)
  (setf (aref (car heap) (cdr heap)) (cons value priority))
  (setf (cdr heap) (1+ (cdr heap)))
  (heap-sort-up heap (1- (cdr heap))))

(defun heap-sort-up (heap index)
  ;; (format t "~a, ~a~%" index (1- (floor (1+ index) 2)))
  (if (and (not (= index 0))
           (> (cdr (aref (car heap) index))
              (cdr (aref (car heap) (1- (floor (1+ index) 2))))))
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
    (car popped)))

(defun heap-sort-down (heap index)
  (if (>= (* 2 (1+ index)) (cdr heap))
      heap ;; base case
    (let ((new-index nil))
      (if (> (cdr (aref (car heap) (1- (* 2 (1+ index)))))
             (cdr (aref (car heap)     (* 2 (1+ index))))) ;; second, no 1- so actually 1+
          ;; first is larger
          (setf new-index (1- (* 2 (1+ index))))
        ;; second is larger
        (setf new-index (* 2 (1+ index))))
      (let ((goingdown (aref (car heap) index)))
        (setf (aref (car heap) index)
              (aref (car heap) new-index))
        (setf (aref (car heap) new-index) goingdown))
      (heap-sort-down heap new-index))))

(defun heap-peek (heap)
  (aref (car heap) 0))

(defun expand-node (node plane queue)
  (if (not (= (second node) 0))
      (add-node node 'left plane queue))
  (if (not (= (third node) 0))
      (add-node node 'up plane queue))
  (if (not (= (second node) 9))
      (add-node node 'right plane queue))
  (if (not (= (third node) 9))
      (add-node node 'down plane queue)))

;; direction is 'up, 'down, 'left, or 'right
(defun add-node (orig-node direction plane queue)
  (let ((new-node nil))
    (case direction
      (up (setf new-node (list (1+ (first orig-node))
                                (second orig-node)
                                (1- (third orig-node)))))
      (down (setf new-node (list (1+ (first orig-node))
                                  (second orig-node)
                                  (1+ (third orig-node)))))
      (left (setf new-node (list (1+ (first orig-node))
                                  (1- (second orig-node))
                                  (third orig-node))))
      (right (setf new-node (list (1+ (first orig-node))
                                   (1+ (second orig-node))
                                   (third orig-node))))
      (t (break "invalid direction")))
    (case (aref plane (second new-node) (third new-node))
      (2 (throw 'goal new-node))
      (0 (heap-insert queue new-node (- 0 (+ (first new-node) (h (second new-node) (third new-node)))))
         (setf (aref plane (second new-node) (third new-node)) 3)))))

;; heap values are (list g x y)
(defun astar ()
  (let ((plane (init-plane))
        (queue (heap-init 36)))
    (heap-insert queue '(0 1 1) (- 0 (h 1 1)))
    (catch 'goal
      (loop while (> (cdr queue) 0)
            do (expand-node (heap-pop queue) plane queue)
            do (print-plane plane)
            ;; do (format t "~a" queue)
                ))))

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
