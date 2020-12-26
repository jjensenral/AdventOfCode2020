;;;; Advent of Code 2020
;;;; Day 19
;;;; jens.jensen@stfc.ac.uk


(defvar +data-buffer+ "19.input" "Buffer with input data preloaded")

;;; First part of problem:
;;; (time (length (solve1)))
;;; (0.579557 . ...)

;;; For the second part of the problem, paste the lines
;;; 8: 42 | 42 8
;;; 11: 42 31 | 42 11 31
;;; into the input buffer at the end of the rules section.
;;;
;;; Unfortunately with the compiled-in defaults, it exceeds the
;;; variable binding depth (the number of shadowing bindings of a
;;; given variable).


;;; Rules are held in an array indexed by rule number.  Each entry is
;;; eitheer a list of rule numbers (applied sequentially, and 'and'
;;; implied), a list of list of rule numbers (an 'or' being implied in
;;; the outer list), or a string, e.g. from the first example:
;;;
;;; [(1 2) "a" ((1 3) (3 1)) "b"]
;;;
;;; or the second example,
;;;
;;; [(4 1 5) ((2 3) (3 2)) ((4 4) (5 5)) ((4 5) (5 4)) "a" "b"]


;;; Spent too much time on this puzzle; this is the second rewrite
;;; which at least now emphasises concise and clearer code (and
;;; correctness).  It is quite possible this is the "significantly
;;; more difficult" approach that we were advised to avoid.



(defun matchp (string rule rules)
  (catch 'match
    (equal (match-rules-p string rule rules) "")))



(defun match-rules-p (string rule-list rules)
  "Match a rule list recursively"
  (let ((this-rule (car rule-list))
	(rest-of-rules (cdr rule-list)))

    (typecase this-rule
      ;; Because of the backtracking, we must throw once a match is found
      (null (when (equal string "") (throw 'match t)) string)

      (string (and (not (equal string ""))
		   (eql (aref string 0) (aref this-rule 0))
		   (match-rules-p (substring string 1) rest-of-rules rules)))

      ((or integer cons)
       (match-rules-cons string this-rule rest-of-rules rules))

      (t (error "Unknown %S not list, string, or int" rule)))))



(defun match-rules-cons (string next-rule rest-of-rules rules)
  ;; This is the "clever" bit
  "Prepend next rule to rest-of-rules and call match-rules-p recursively"

  ;; First check if it's an OR because this is a special case,
  ;; requiring (eventually) one call to match-rules-p for each branch
  (cond
   ((and (consp next-rule) (consp (car next-rule)))
    ;; We call ourselves to splice the list back in (as an AND)
    (match-rules-cons string (car next-rule) rest-of-rules rules)
    ;; Second branch
    (match-rules-cons string (cadr next-rule) rest-of-rules rules))

   ((integerp next-rule) (match-rules-cons string (aref rules next-rule) rest-of-rules rules))

   ;; In case a string filters through, send it back
   ((stringp next-rule) (match-rules-p string (cons next-rule rest-of-rules) rules))

   ;; Finally, only AND should remain; splice the AND list onto the rules list
   ((consp next-rule)
    (let ((todo (copy-seq next-rule)))
      ;; Splice todo with rest-of-rules which leaves the latter unchanged
      (rplacd (last todo) rest-of-rules)
      (match-rules-p string todo rules)))

   (t (error "Unknown rule %S" next-rule))))






(defun solve1 nil
  "Parse input from the buffer whose name is stored in +data-buffer+"
  (save-current-buffer
    (set-buffer +data-buffer+)
    (goto-char 0)
    (let ((rules (parse-rules))
	  (strings (parse-strings)))
      ;; For debugging use delete-if-not in place of count-if
      (delete-if-not (lambda (str) (matchp str (list 0) rules)) strings))))



(defun parse-rules ()
  "Parse a rule section from the current buffer, returning a vector of rules"
  (let ((result []))
    (while (re-search-forward "^\\([0-9]+\\): \\(.*\\)$" nil t)
      (let ((idx (string-to-number (match-string 1)))
	    (stuff (match-string 2)))
	(aset-may-extend result idx (parse-rule-data stuff))))
    result))



(defun parse-rule-data (stuff)
  "Parse the string of rule data and return whatever is the appropriate encoding of the rule"
  (cond
   ;; Split at spaces (using the utility from utils.el)
   ;; Note the "? " which is Space.
   ((string-match "^\\([0-9]+ ?\\)+$" stuff)
    (mapcar #'string-to-number (split-sequence (lambda (c) (eq c ? )) stuff)))

   ;; For the OR, we already know how to parse the lists
   ((string-match "^\\([0-9 ]+\\) | \\([0-9 ]+\\)$" stuff)
    (let ((substr1 (match-string 1 stuff))
	  (substr2 (match-string 2 stuff)))
      (list (parse-rule-data substr1) (parse-rule-data substr2))))
   
   ;; string
   ((string-match "^\"\\([ab]\\)\"$" stuff)
    (match-string 1 stuff))

   (t (error "Unable to parse %s" stuff))))



(defun parse-strings nil
  "From current position in current buffer, gather all the strings consisting entirely of the characters ?a or ?b"
  (let ((strings nil))
    (while (re-search-forward "^[ab]+$" nil t)
      (push (match-string 0) strings))
    (when (< 2 (- (buffer-end 1) (point)))
      (warn "Finished reading strings at %d, expected %d" (point) (buffer-end 1)))
    (nreverse strings)))
