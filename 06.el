;;;; Advent of Code 2020
;;;; 06 December
;;;; jens.jensen@stfc.ac.uk


;;; As usual, the code reads data from a buffer

(defvar +data-buffer+ "06.input"
  "Name of buffer containing the data to process")





(defun read-next-line nil
  "Read the next line of a-z and return the string found on it, or nil if no matches (or end of buffer)"
  (if (eql (point) (buffer-end 1))
      ;; end of buffer
      nil
    (progn
      (re-search-forward "^\\([a-z]*\\)$" nil t)
      (let* ((beg (match-beginning 1)) (end (match-end 1)))
	;; Ensure we step past the regexp so it doesn't match the next time
	(goto-char (1+ end))
	(buffer-substring-no-properties beg end)))))




(defun read-next-group nil
  "Return list of group entries"
  (let ((group nil)
	line)
    ;; This is where loop could be more elegant?
    ;; Note that (equal "" nil) is safe (and false)
    (while (and (setq line (read-next-line))
		(not (equal line "")))
      (push line group))
    ;; The order probably doesn't matter but let's return them in the
    ;; order we found them
    (nreverse group)))



;;; On the test data, (map-groups #'identity)
;;; => (("abc") ("a" "b" "c") ("ab" "ac") ("a" "a" "a" "a") ("b"))


(defun map-groups (func)
  "Read all groups in the data buffer calling func on them, and return a list of the results"
  (let ((results nil))
    (save-current-buffer
      (set-buffer +data-buffer+)
      (goto-char 0)
      (let (group)
	(while (setq group (read-next-group))
	  (push (funcall func group) results))))
    (nreverse results)))


;;; Don't confuse this with the (similar) emacs built-in, #'add-to-list
;;;
;;; (add-to-alist ?A (list (cons ?A 2) (cons ?B 4))) => ((65 . 3) (66 . 4))
;;; (add-to-alist ?B (list (cons ?A 2) (cons ?B 4))) => ((65 . 2) (66 . 5))
;;; (add-to-alist ?C (list (cons ?A 2) (cons ?B 4))) => ((67 . 1) (65 . 2) (66 . 4))

(defun add-to-alist (elt lst)
  "Add element count to alist, returning the resulting list.  The input list may or may not be modified."
  (let ((match (assoc elt lst #'eql)))
    (if match
	(progn (rplacd match (1+ (cdr match))) lst)	; replace in-place
      (cons (cons elt 1) lst))))



;;; (count-char-string "abracadabra")
;;; => ((100 . 1) (99 . 1) (114 . 2) (98 . 2) (97 . 5))

(defun count-char-string (s)
  "Return an alist of characters and their count in the string"
  (let ((result nil))
    (mapc (lambda (c) (setq result (add-to-alist c result))) s)
    result))


;;; (count-char-group '("ab" "ac"))
;;; => ((99 . 1) (98 . 1) (97 . 2))


(defun count-char-group (group)
  "Return the total count of characters as an alist in a group (list) of strings"
  (count-char-string (apply #'concat group)))



(defun solve1 nil
  "Add all the distinct characters (ignoring count) from each group"
  (apply #'+
	 (map-groups (lambda (group) (length (count-char-group group))))))



;;; This is akin to remove-if-not or delete-if-not, picking elements
;;; from a list which satisfy predicate, like in Common Lisp:
;;; (delete-if-not #'evenp (list 2 3 4 5))
;;; => (2 4)
;;;
;;; (remove-if-not #'stringp '("abc" 234 "wombat" -3.1415927))
;;; => ("abc" "wombat")

(defun remove-if-not (pred lst)
  "Remove items not satisfying predicate, ie keep those that do.  Result is freshly consed so lst is unchanged"
  (mapcan (lambda (x) (if (funcall pred x) (list x) nil)) lst))
    


(defun solve2-group-count (group)
  "Return the number of entries that are in every person's response in the group"
  ;; This should work unless an individual response has repeated characters
  (let ((count (count-char-group group))
	(len (length group)))
    ;; Select elements whose count equals the group count and count how many
    (length (remove-if-not (lambda (cell) (eql (cdr cell) len)) count))))


(defun solve2 nil
  "Count characters that are in every entry in each group, adding the lot"
  (apply #'+ (map-groups #'solve2-group-count)))
