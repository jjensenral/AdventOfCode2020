;;;; Advent of Code 2020
;;;; 09 December
;;;; jens.jensen@stfc.ac.uk
;;;;
;;;; Depends on util.el in the same repository.
;;;; And queue.el which is written for this puzzle.


;;; Summing numbers in a window of integers


(defvar +data-buffer+ "09.test" "Data to process")


(defvar +window-size+ 5 "Size of \"window\" of values to consider")


(defun read-data nil
  "Read data from the input buffer, returning the result as a list"
  (let ((data (map-data-buffer "^\\([0-9]+\\)$" #'string-to-int)))
    ;; We need to check they are all integers, as string-to-int can
    ;; auto-demote a number to float.
    ;; #'every from CL would do the trick nicely.
    (dolist (elt data)
      (unless (typep elt 'integer)
	(signal 'wrong-type-argument (list 'integer elt))))
    data))


(defun sum-of-two-p (elem list-of-nums)
  "Check whether elem is a sum of any two numbers in list-of-nums"
  (cond
   ;; We have one or zero elements
   ((or (endp list-of-nums) (endp (cdr list-of-nums))) nil)
   ;; See if the first element is usable
   ((member (- elem (car list-of-nums)) (cdr list-of-nums)) t)
   ;; If not, recursively check the rest
   (t (sum-of-two-p elem (cdr list-of-nums)))))


(defun solve1 nil
  (catch 'found
    (let* ((preamble (read-data))
	   ;; Split all data into two lists and turn the first into a queue
	   (tmp (nthcdr (1- +window-size+) preamble))
	   ;; amble is what comes after the preamble, of course
	   (amble (cdr tmp)))

      ;; Now cut after tmp
      (rplacd tmp nil)
      (setq preamble (make-queue preamble))

      ;; Now we can loop through amble.  Amble through amble?
      (while amble
	(unless (sum-of-two-p (car amble) (queue-list preamble))
	  (throw 'found (car amble)))
	;; Now we need to update the queue
	(queue-pop preamble)
	(queue-push (car amble) preamble)
	;; And reduce amble by one
	(pop amble)))
    ;; Not found
    nil))


;;; solve2 runs in 0.051s according to the time utility in utils.el

(defun solve2 nil
  (let ((amble (read-data))
	(target (solve1)))

    ;; Catches have dynamic extent
    (catch 'found
      (while amble
	(awhen (try-sum target 0 0 amble)
	       ;; cut the rest of the list, doesn't matter if we destroy it
	       (rplacd (nthcdr (1- it) amble) nil)
	       (let* ((smallest (apply #'min amble))
		      (largest (apply #'max amble)))
		 (throw 'found (+ smallest largest))))
	(pop amble))
      nil)))


;;; This code works on the small example, but exceeds a limit on the large

(defun try-sum (target sum count elems)
  "Try to sum target with the first elements of elems, returning the count if there is one"
  (cond
   ((= target sum) count)
   ((< target sum) nil)
   ((endp elems) nil)
   (t (try-sum target (+ sum (car elems)) (1+ count) (cdr elems)))))


(defun try-sum (target sum count elems)
  "Less elegant version of the above, but not recursing"
  (while (and (< sum target) elems)
    (incf sum (car elems))
    (setq elems (cdr elems))
    (incf count))
  (if (= sum target) count nil))
