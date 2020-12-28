;;;; Advent of Code 2020
;;;; 19 Dec
;;;; jens.jensen@stfc.ac.uk
;;;;
;;;; Common Lisp version
;;;; Requires: ppcre
;;;;
;;;; This code is a translation of the ELisp code to Common Lisp -
;;;; just to prove that the algorithm works


;;; In contrast to the Emacs version which read data from a buffer,
;;; this one reads from a file.
;;; Tested with SBCL.

(defparameter +data-file+ "19.input")



;;; Rules are held in an array indexed by rule number.  Each entry is
;;; eitheer a list of rule numbers (applied sequentially, and 'and'
;;; implied), a list of list of rule numbers (an 'or' being implied in
;;; the outer list), or a string, e.g. from the first example:
;;;
;;; #((1 2) "a" ((1 3) (3 1)) "b")
;;;
;;; or the second example,
;;;
;;; #((4 1 5) ((2 3) (3 2)) ((4 4) (5 5)) ((4 5) (5 4)) "a" "b")


;;; Spent too much time on this puzzle; this is the second rewrite
;;; which at least now emphasises concise and clearer code (and
;;; correctness).  It is quite possible this is the "significantly
;;; more difficult" approach that we were advised to avoid.



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
   ((characterp next-rule) (match-rules-p string (cons next-rule rest-of-rules) rules))

   ;; Finally, only AND should remain; splice the AND list onto the rules list
   ((consp next-rule)
    (let ((todo (copy-seq next-rule)))
      ;; Splice todo with rest-of-rules which leaves the latter unchanged
      (rplacd (last todo) rest-of-rules)
      (match-rules-p string todo rules)))

   (t (error "Unknown rule ~S" next-rule))))



(defun match-rules-p (string rule-list rules)
  "Match a rule list recursively"
  (let ((this-rule (car rule-list))
	(rest-of-rules (cdr rule-list)))

    (etypecase this-rule
      ;; Because of the backtracking, we must throw once a match is found
      (null (when (equal string "") (throw 'match t)) string)

      (character (and (not (equal string ""))
		      (eql (aref string 0) this-rule)
		      (match-rules-p (subseq string 1) rest-of-rules rules)))

      ((or integer cons)
       (match-rules-cons string this-rule rest-of-rules rules)))))



(defun matchp (string rule rules)
  (catch 'match
    (equal (match-rules-p string rule rules) "")))





(defun string-to-number (x)
  "Convert a string containing a number to a number"
  ;; There's probably a better way to do this but this'll do
  (reduce (lambda (a b) (+ (* a 10) b)) (map 'list (lambda (c) (- (char-code c) (char-code #\0))) x)))



(defun add-rule (rules pos-string rule-data)
  "Add a rule to vector rules"
  (let ((pos (string-to-number pos-string))
	(data (parse-rule-data rule-data)))
    (when (>= pos (length rules))
      (setq rules (adjust-array rules (1+ pos) :initial-element nil)))
    (setf (aref rules pos) data))
  rules)



(defun parse-input (input-file)
  "Parse input from the file whose name is in input-file"
  (let ((rules (make-array 0 :adjustable t :element-type t))
	(strings nil)
	(scan-rule (ppcre:create-scanner "^([0-9]+): (.*)$" :single-line-mode t))
	(scan-ab (ppcre:create-scanner "^[ab]+$" :single-line-mode t)))

  (with-open-file (input input-file :if-does-not-exist :error
			 :direction :input)
    (loop for line = (read-line input nil nil)
       while line
       do (multiple-value-bind (match regs) (ppcre:scan-to-strings scan-rule line :sharedp nil)
	    (when match (setq rules (add-rule rules (svref regs 0) (svref regs 1)))))
       do (let ((match (ppcre:scan scan-ab line)))
	    (when match (push line strings)))))
  (values rules (nreverse strings))))



(defun solve (&key (input-file +data-file+) return-matches replace-rules)
  "Read file for input, optionally return the matches (default is only the count) and replace rules 8 and 11"
  (let ((func (if return-matches #'delete-if-not #'count-if)))
    (multiple-value-bind (rules strings) (parse-input input-file)
      (when replace-rules
	(setf (aref rules 8) '((42) (42 8))
	      (aref rules 11) '((42 31) (42 11 31))))
      ;; For debugging use delete-if-not in place of count-if
      (funcall func (lambda (str) (matchp str (list 0) rules)) strings))))




(defun parse-rule-data (stuff)
  "Parse the string of rule data and return whatever is the appropriate encoding of the rule"

   ;; Split at spaces (using the utility from utils.el)
   (let ((match (ppcre:scan "^([0-9]+ ?)+$" stuff)))
     (when match
       (return-from parse-rule-data (mapcar #'string-to-number (ppcre:split " " stuff)))))

   ;; For the OR, we already know how to parse the lists
   (multiple-value-bind (match regs) (ppcre:scan-to-strings "^([0-9 ]+) \\| ([0-9 ]+)$" stuff)
     (when match
       (return-from parse-rule-data
	 (let ((substr1 (svref regs 0))
	       (substr2 (svref regs 1)))
	   (list (parse-rule-data substr1) (parse-rule-data substr2))))))
   
   ;; string
   (multiple-value-bind (match regs) (ppcre:scan-to-strings "^\"([ab])\"$" stuff)
     (when match (return-from parse-rule-data (aref (aref regs 0) 0))))

   (error "Unable to parse ~s" stuff))



