;;;; Advent of Code 2020
;;;; Day 15
;;;; jens.jensen@stfc.ac.uk



;;; Implementation note.  We considered storing the vector backwards
;;; as most find/position functions are happier working forwards.
;;;
;;; However, using cl-position makes this unnecessary:
;;; (cl-position 6 '[1 2 3 6 3 4 5 6 7] :from-end t :end 4)
;;; => 3


;;; vectors in ELisp are not super fast...(on the author's slow laptop)
;;; (time (solve1-test))
;;; (0.399136 . t)

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
  (let ((len (length input)))
    (algo (vconcat input (make-vector (- total-length len) nil)) (1- len) total-length)))


(defun algo (v k total-length)
  "Process vector v with the most recent number at k.  The final number is returned."
  (let ((limit (1- total-length)))
    (while (< k limit)

      (let* ((most-recent (aref v k))
	     ;; The location at end is not included so we won't find the
	     ;; same number again.
	     (prior (cl-position most-recent v :from-end t :end k)))

	(setf k (1+ k)			; now pointing at first nil
	      (aref v k) (if prior (- k prior 1) 0)))))

  (aref v 2019))
