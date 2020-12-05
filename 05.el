;;;; Advent of Code 2020
;;;; 05 December
;;;; jens.jensen@stfc.ac.uk



;;; As usual, the code assumes the input data has already been read
;;; into a buffer called 05.input
;;;

(defvar +data-buffer+ "05.input")



;;; Test code!
(defun test-decode-seat nil
  "Quick basic check of decode-seat, just returns true or false"
  (equal (mapcar #'decode-seat '("FBFBBFFRLR" "BFFFBBFRRR" "FFFBBBFRRR" "BBFFBBFRLL"))
	 '(357 567 119 820)))


(defun decode-seat (s)
  "Decode a string of Fs, Bs, Ls and Rs into a single integer, as if Fs and Ls were 0 and the others 1, msb first"
  (string-to-number (fix-digits s) 2))



(defun map-data-buffer (func)
  "Call a mapping function on every entry in the data buffer; returns a list of the results"
  ;; New values are consed to the front of the list, and we reverse
  ;; the final result
  (let ((accumulator nil))
    (save-current-buffer
      (set-buffer +data-buffer+)
      (goto-char 0)
      (while (re-search-forward "^[LRFB]+$" nil t)
	(push (funcall func (buffer-substring-no-properties (match-beginning 0) (match-end 0))) accumulator)))
    (nreverse accumulator)))


(defun fix-digits (s)
  "Return a fresh string s with Fs and Ls replaced by 0, and Bs and Rs replaced with 1"
  ;; Compatibility note: CL's #'map would be better as it can generate
  ;; strings directly; #'mapcar always returns a list.
  (coerce
   (mapcar (lambda (c) (cond
		       ((or (eql c ?F) (eql c ?L)) ?0)
		       ((or (eql c ?B) (eql c ?R)) ?1)
		       (t c))) s)
   'string))



(defun solve1 nil
  "Find max entry in the data buffer"
  ;; This only works as long as a non-empty list of integers is returned
  (apply #'max (map-data-buffer #'decode-seat)))


(defun solve2 nil
  "Find a gap inside of the data"

  (let ((all-seats (sort (map-data-buffer #'decode-seat) #'<))
	(seat-gap nil)
	(other-stuff nil))

    ;; OK, let's look at pairs of entries
    ;; I know I have slightly broken my rule by using #'map (it's from CL,
    ;; not the core ELisp)
    (map nil (lambda (a b)
	       (let ((diff (- b a)))
		 (cond
		  ((= diff 2) (push (1+ a) seat-gap)) ; definitely a gap
		  ((> diff 2) (push (cons a b) other-stuff))))) ; something else

	 ;; we look at consecutive elements by passing in the list
	 ;; twice, one offset; the length of the map is that of the
	 ;; shorter sequence so we don't need to chop off the last
	 ;; element in the other one.
	 all-seats
	 (cdr all-seats))

    ;; we just print the results
    (print seat-gap)
    (print other-stuff)))
