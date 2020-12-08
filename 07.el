;;;; Advent of Code 2020
;;;; 07 Dec
;;;; jens.jensen@stfc.ac.uk


(defvar +data-buffer+ "07.test"
  "Name of buffer to slurp data from")


;;; The basic data structure is a hash table with strings as keys,
;;; which maps strings to alists.
;;;
;;; E.g.
;;;
;;; "light red bags contain 1 bright white bag, 2 muted yellow bags."
;;;
;;; becomes ('contains')
;;;
;;; "light red" => (("bright white" . 1) ("muted yellow" . 2))
;;; and faded blue bags contain no other bags
;;; "faded blue" => nil
;;;
;;; or maybe backwards ('contained-in')?
;;;
;;; "bright white" => (("light red" . 1))
;;; "muted yellow" => (("light red" . 2))
;;; (and nothing points to faded blue.)

;;; Terminology
;;;
;;; In this example,
;;;
;;; "light red" => (("bright white" . 1) ("muted yellow" . 2))
;;;
;;; let's call "light red" the *key*, and the list is the *value*;
;;; here the value has two *entries* which are cons cells (pairs, if
;;; you like) which have *entry-key* and *entry-count*.

;; (let ((v (make-hash-table :test #'equal)))
;;   (add-to-hash-table "light red" v '("bright white" . 1))
;;   (add-to-hash-table "light red" v '("muted yellow" . 2))
;;   (add-to-hash-table "dark orange" v '("bright white" . 3))
;;   (add-to-hash-table "dark orange" v '("muted yellow" . 4))
;;   v)
;; => #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ("light red" (("muted yellow" . 2) ("bright white" . 1)) "dark orange" (("muted yellow" . 4) ("bright white" . 3)) ...))


(defun add-to-hash-table (key hash entry)
  "Add entry to hash table, returning hash"
  (let* ((v (gethash key hash))
	 (ekey (car entry))
	 (a (assoc ekey v)))		; note assoc uses #'equal in Elisp

    ;; This is probably a mistake if it's already there
    (when a
      (error "entry key \"%s\" already present for key = %s" ekey key))

    (if v
	(push entry (gethash key hash))
      (puthash key (list entry) hash)))
  hash)



(defun parse-data (callback)
  "Call callback function with arguments key, entry-key, entry-count, or arguments key, "", 0"
  (save-excursion
    (set-buffer +data-buffer+)
    (goto-char 0)

    (while (re-search-forward "^\\([a-z]+ [a-z]+\\) bags contain.*$" nil t)
      (let ((eol (match-end 0))
	    (key (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
	    (collecting-entries t))

	;; reset to the end of the first match, just in case
	(goto-char (match-end 1))

	(while collecting-entries
	  (cond
	   ;; The end of line (eol) prevents no other bags from
	   ;; skipping ahead, merely looking in the current line
	   ((re-search-forward "no other bags" eol t) (funcall callback key "" 0) (setq collecting-entries nil))
	   (t
	    
	    ;; This should always match, unless the buffer is truncated or something;
	    ;; note it uses the default of throwing exception if it fails
	    (re-search-forward "\\([0-9]+\\) \\([a-z]+ [a-z]+\\) bags?\\([,\\.]\\)")

	    (let ((count (string-to-int (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
		  (entry-key (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
		  (terminator (buffer-substring-no-properties (match-beginning 3) (match-end 3))))

	      (funcall callback key entry-key count)
	      (when (equal terminator ".") (setq collecting-entries nil))))))))))



(defun contained-in nil
  "Return a hash table of the relation contained-in"
  (let ((hash (make-hash-table :test #'equal)))
    (parse-data (lambda (k ek n)
		  (unless (equal ek "")
		    (add-to-hash-table ek hash (cons k n)))))
    hash))


(defun contains nil
  "Return a hash table of the relation contains"
  (let ((hash (make-hash-table :test #'equal)))
    (parse-data (lambda (k ek n)
		  (add-to-hash-table k hash (cons ek n))))
    hash))




;; on the test input, (solve1) returns
;; ("light red" "dark orange" "bright white" "muted yellow")

(defun solve1 nil
  (let ((graph (contained-in))
	(result nil)
	(todo (list "shiny gold")))

    ;; Essentially a graph search
    (while todo
      (let* ((current (car todo))
	     ;; drop the counts
	     (next (mapcar #'car (gethash current graph))))

	(setq todo (cdr todo))
	;; can we meet a bag more than once?
	(dolist (e next)
	  (unless (member e result)
	    (push e result))
	  (push e todo))))
    result))


(defun bag-count (graph key)
  "Return the accumulated bag count of bag colour identified by key"
  (let ((matches (gethash key graph)))
    ;; The 1+ is the current bag; all the machinery below counts inside bags
    (1+
     (apply #'+
	    (mapcar (lambda (entry)
		      (if (equal (car entry) "")
			  ;; one bag, contains nothing else
			  0
			;; one bag, plus whatever's inside
			(* (cdr entry) (bag-count graph (car entry)))))
		    matches)))))


(defun solve2 nil
  "Iterate through contains, counting numbers"
  ;; The shiny gold itself doesn't count?
  (1- (bag-count (contains) "shiny gold")))
