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
