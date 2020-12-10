;;;; Advent of Code 2020
;;;; Queue implementation using lists


;;; Implementation:
;;;
;;; We have the head of the list, obviously, and a pointer to the tail
;;; - ie the last cons cell, as normally returned by last.  These are
;;; kept in an outer cons cell with the car being the head and the cdr
;;; holding the tail.  The exception is an empty queue, which will
;;; just be nil, so it will test as false.
;;;


(defun make-queue (&rest elts)
  "Make a queue out of a variable number of arguments.  If there is only one argument and it is a list, use that as arguments.  List is reused."
  (cond
   ((endp elts) nil)
   ((and (listp (car elts)) (endp (cdr elts)))
    (cons (car elts) (last (car elts))))
   (t (cons elts (last elts)))))


(defmacro queue-push (elem queue)
  "Push an element onto the queue and return the queue.  The queue is modified in place."
  ;; We actually need to push the element at the back
  (let ((newlast (gensym)))
  `(setf ,queue
	 (if ,queue
	     (let ((,newlast (cons ,elem nil)))
	       (rplacd (cdr ,queue) ,newlast)
	       (rplacd ,queue ,newlast)
	       ,queue)
    (make-queue ,elem)))))



(defun queue-first (queue)
  "Return the element at the head of the queue"
  (when (endp queue) (error "Trying to get first element of empty queue"))
  (caar queue))



(defmacro queue-pop (queue)
  "Pop an element from the queue.  The updated queue is returned"
  ;; We pop from the front
  `(setf ,queue
	 (cond
	  ((null ,queue) (error "Pop from empty queue"))
	  ;; Only one element in queue
	  ((endp (cdar ,queue)) nil)
	  (t (rplaca ,queue (cdar ,queue)) ,queue))))



(defun queue-list (queue)
  "Return a list of the elements in the queue"
  (car queue))



;; (defun queue-test nil
;;   (let ((q (make-queue 1 2 3)))
;;     (and
;;      (equal (queue-list q) '(1 2 3))
;;      (queue-push 4 q)
;;      (equal (queue-list q) '(1 2 3 4))
;;      (queue-pop q)
;;      (eql (queue-first q) 2)
;;      (queue-pop q)
;;      (equal (queue-list q) '(3 4))
;;      (queue-pop q)
;;      ;; Queue should now become empty
;;      (progn (queue-pop q) (not q))
;;      (queue-push 6 q)
;;      (equal (queue-list q) '(6))
;;      (queue-push 7 q)
;;      (equal (queue-list q) '(6 7)))))
