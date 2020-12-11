;;;; Utilities file, for stuff that I keep writing for each puzzle
;;;; jens.jensen@stfc.ac.uk


;;; Macro for timing code.


(defmacro time (&rest code)
  "Return the wallclock runtime of code.
Returns a cons of seconds, and the return value"
  `(let* ((now (current-time))
	  (retval ,@code)
	  (then (time-subtract (current-time) now)))
     (cons (+ (ash (nth 0 then) 16) (nth 1 then)
	      (/ (nth 2 then) 1000000.0) (/ (nth 3 then) (expt 10. 12)))
	   retval)))



(defmacro incf (var &optional inc)
  "Increment var (a place) by inc, default 1"
  `(setf ,var (+ ,var (or ,inc 1))))



(defun count-regexp-string (regexp string)
  "Count the occurrences of regexp in string, à la #'how-many"
  (let ((count 0)
	(pos 0))
    (while (string-match regexp string pos)
      (incf count)
      (setq pos (match-end 0)))
    count))



;;; We count whether to accumulate the matches if there are
;;; parentheses in the regexp by counting the number of parentheses.
;;; This should be slightly safer than expecting the substring matches
;;; to return nil, as they might legitimately return nil on a failed
;;; substring (with \| alternatives).


(defun map-data-buffer (regexp func)
  "Call a mapping function on every entry in the data buffer (as named by +data-buffer+); returns a list of the results.  The function receives either a string, or if there are matching parentheses in the regexp, it will receive a list of the parenthesis matches."

  (save-current-buffer
    (set-buffer +data-buffer+)
    (goto-char 0)

    ;; New values are consed to the front of the list, and we reverse
    ;; the final result to get the right order
    (let ((accumulator nil)
	  ;; Are we processing substrings or the whole match?
	  (match-count (count-regexp-string "\\\\(" regexp)))

      (save-current-buffer
	(set-buffer +data-buffer+)
	(goto-char 0)
	(while (re-search-forward regexp nil t)
	  (push
	   (if (zerop match-count)
	       ;; Passing the whole match string to the function
	       (funcall func (buffer-substring-no-properties (match-beginning 0) (match-end 0)))
	     ;; Or an assembled list of substrings
	     (let ((result nil))
	       (dotimes (k match-count)
		 (push (buffer-substring-no-properties (match-beginning (1+ k)) (match-end (1+ k))) result))
	       (apply func (nreverse result))))
	   ;; push onto accumulator
	   accumulator)
	  ;; Step past the end of the match to make sure we don't match again
	  (goto-char (1+ (match-end 0)))))
      (nreverse accumulator))))



;;; I've needed this a few times, an anaphoric while (which means the
;;; tested value is available as 'it' in the body).

(defmacro awhile (test &rest body)
  "Anaphoric while; the value of the test is available as 'it' inside the body of the while"
  `(let ((it ,test))
     (while (setq it ,test) ,@body)))


(defmacro awhen (test &rest body)
  "Anaphoric when"
  `(let ((it ,test)) (when it ,@body)))



;;; First seen in 06.el
(defun add-to-alist (elt lst)
  "Add element count to alist, returning the resulting list.  The input list may or may not be modified."
  (let ((match (assoc elt lst)))
    (if match
	(progn (rplacd match (1+ (cdr match))) lst)	; replace in-place
      (cons (cons elt 1) lst))))

(defun count-elems (seq)
  "In a sequence, return the elements and their count eg \"abracadabra\" => ((100 . 1) (99 . 1) (114 . 2) (98 . 2) (97 . 5)).  #'equal is used for equality"
  (let ((result nil))
    (mapc (lambda (c) (setq result (add-to-alist c result))) seq)
    result))


;;; This is a standard lispy thing, more or less
;;; Compatibility note 1: subseq is in CL but it gives us generic split (vector, list, string)
;;; Compatibility note 2: ELisp does have #'split-string
;;; Compatibility note 3: fun fact - even CL does not have a split-sequence as part of the standard

;;; (split-sequence (lambda (c) (eq c ?x)) "abcxdefxghi")
;;; => ("abc" "def" "ghi")
;;; (split-sequence (lambda (c) (eq c ?x)) "xyzzyx")
;;; => ("" "yzzy" "")
;;; (split-sequence #'null (list 'a 'b nil 'c 'd nil nil 'e))
;;; => ((a b) (c d) nil (e))
;;; (split-sequence #'atom [(1 2) 3 (4 5) 6 7])
;;; ([(1 2)] [(4 5)] [] [])


(defun split-sequence (test seq)
  "Split sequence at elements satisfying test, returning a list of sequences of the pieces"
  (let* ((m1 (position-if test seq)))
    (cons (subseq seq 0 m1)
	  (if m1
	      (split-sequence test (subseq seq (1+ m1)))
	    nil))))
