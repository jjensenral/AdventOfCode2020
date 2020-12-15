;;;; Advent of Code 2020
;;;; Day 15
;;;; jens.jensen@stfc.ac.uk


;;; There was an earlier version which used a seek through the vector.
;;; This took forever to run on problem 2.  This one has go-faster-stripes
;;; painted on it, making it go faster (obviously).


;;; On the author's slow laptop, this is the timing for the fast version:
;;; (time (solve1-test))
;;; (0.006862 . t)
;;; and the second version (not the test, good luck with that)
;;; (23.365308 . ...)
;;; (22.916457 . ...)


(defun solve1-test nil
  (and
   (eql (solve1-input '(0 3 6) 2020) 436)
   (eql (solve1-input '(1 3 2) 2020) 1)
   (eql (solve1-input '(2 1 3) 2020) 10)
   (eql (solve1-input '(1 2 3) 2020) 27)
   (eql (solve1-input '(2 3 1) 2020) 78)
   (eql (solve1-input '(3 2 1) 2020) 438)
   (eql (solve1-input '(3 1 2) 2020) 1836)))


(defun solve1 nil (solve1-input [9 19 1 6 0 5 4] 2020))

(defun solve2 nil
  (byte-compile 'algo)
  (solve1-input [9 19 1 6 0 5 4] 30000000))


(defun solve1-input (input total-length)
  "Solve for input which can be a list or a vector of starting values"
  (let ((len (length input))
	(position-table (make-hash-table)))
    ;; pos table will store the most recent two position of its key,
    ;; or nil if there wasn't one
    (dotimes (k len) (puthash (elt input k) (cons k nil) position-table))
    (algo (vconcat input (make-vector (- total-length len) nil)) (1- len) total-length position-table)))


(defun algo (v k total-length position-table)
  "Process vector v with the most recent number at k.  The final number is returned."
  (let ((limit (1- total-length)))
    (while (< k limit)

      (let* ((most-recent (aref v k))
	     (prior (cdr  (gethash most-recent position-table)))
	     (new-elt (if prior (- k prior) 0)))
	(setf k (1+ k)			; now pointing at first nil
	      (aref v k) new-elt)
	(puthash new-elt (cons k (car (gethash new-elt position-table))) position-table))))

  (aref v k))
