;;;; Advent of Code
;;;; 10 Dec
;;;; jens.jensen@stfc.ac.uk


;;; Using utils.el in the same repository so load that first

;;; From day 09 except we don't need to test the numbers
(defun read-data nil
  "Read data from the input buffer, returning the result as a list"
  (map-data-buffer "^\\([0-9]+\\)$" #'string-to-int))


(defun read-data-and-extend nil
  "Extend data with 0 and 3+max and sort in ascending order"
  (let* ((data (read-data)))
    (sort (list* 0 (+ 3 (apply #'max data)) data) #'<)))

  
(defun difference (elems)
  "Return the difference between consecutive elements of elems"
  ;; Slightly lazy implementation, using CL's map
  (map 'list #'- (cdr elems) elems))


(defun solve1 nil
  "First part of puzzle"
  ;; count-elems from utils.el
  (let ((counts (count-elems (difference (read-data-and-extend) #'<))))
    (* (cdr (assoc 1 counts)) (cdr (assoc 3 counts)))))


;;; Some thinking is required here:
;;;
;;; (difference (sort (list 0 16 10 15 5 1 11 7 19 6 12 4 22) #'<))
;;; => (difference '(0 1 4 5 6 7 10 11 12 15 16 19 22))
;;; => (1 3 1 1 1 3 1 1 3 1 3 3)
;;;
;;; Clearly both numbers giving rise to a 3 difference *must* be
;;; present, ie (1 4), (7 10), (12 15), (16 19) and (19 22).  Between
;;; 4 and 7, we have 5 or 6, either or both of which may be present,
;;; giving four options.  And 11 can be absent or not.
;;;
;;; Similarly, if we'd started with (0 1 4 5 7 10 ...) ie (1 3 1 2
;;; 3...), only the 5 would be optional, giving two options.
;;;
;;; If we had (1 4 5 7 8 11) => (3 1 2 1 3), 5 and 7 would be
;;; optional, but they cannot both be absent, giving three
;;; possibilities.  In (1 4 5 7 9 11 12 15) => (3 1 2 2 1 3),
;;; 5 can be absent; neither 7 or 9 can be absent, but 11 can be.
;;; Four possibilites.  In (1 4 5 6 8 9 11 14) => (3 1 1 2 1 2 3)
;;; 5 or 6 can be absent, but not both, so 3 possibilites;
;;; 8 or 9 can be absent, but not both.
;;;
;;; Interestingly there are no twos in the actual data?
;;;
;;; For (1 4 5 6 7 8 11) => (3 1 1 1 1 3), we have seven
;;; possibilities: all except (1 4 8 11).

;;; I am cheating slightly here because these are the only patterns
;;; that occur in the data; in fact the above discussion is more than
;;; adequate to actually solve the puzzle.
;;;
;;; (count-elems (split-sequence (lambda (x) (eq x 3)) (difference (read-data-and-extend))))
;;; => (((1) . 1) ((1 1) . 1) ((1 1 1) . 1) (nil . 4) ((1 1 1 1) . 4))


;;; solve2 takes 1ms to run and find the solution!

(defun solve2 nil
  (let* ((data (difference (read-data-and-extend)))
	 ;; Also in utils.el in this repo
	 (seqs (split-sequence (lambda (x) (eq x 3)) data)))
    (apply #'*
	   (mapcar (lambda (ones-list) (case (length ones-list)
					 ((0 1) 1)
					 (2 2)
					 (3 4)
					 (4 7)))
		   seqs))))
