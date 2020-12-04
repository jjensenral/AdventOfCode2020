;;; Advent of Code 2020
;;; 04 December
;;; jens.jensen@stfc.ac.uk
;;;
;;; Parsing passport records.  How to use - in Emacs (eg in *scratch*)
;;; load the file with (load ...), load data into a buffer called 04.input
;;; and then run (solve1), resp. (solve2)
;;;


(defvar +data-buffer+ "04.input"
  "Buffer with the input (test or actual) data loaded into it")



(defvar +fields+ '(
		 "byr"
		 "iyr"
		 "eyr"
		 "hgt"
		 "hcl"
		 "ecl"
		 "pid"
		 "cid")
  "Valid passport fields pasted from the specification")



(defun read-next-passport nil
  "Read a passport entry from the current buffer, advancing point to after the passport.  No validation is done; data is returned as a "

  ;; Check if we're on a blank line and step off it
  ;; (this is safe even at end of buffer because char-after returns nil)
  (while (eql (char-after (point)) ?\n) (forward-char))

  ;; Check if point is near the actual end of the buffer, in which
  ;; case we're probably done
  (if (= (buffer-end 1) (point))

      nil				; return nil for no more joy

    ;; .. else capture the region holding a single passport entry; end
    ;; will be nil if there is no blank line at the end of the buffer
    (let ((pos (point))
	  (end (and (re-search-forward "^$" nil t) (car (match-data)))))
      (read-next-passport-1 pos (or end (buffer-end 1))))))



(defun read-next-passport-1 (beg end)
  "Helper function for read-next-passport; read an entry between two positions, positioning point at end when done"

  (let ((passport (make-hash-table :test #'equal)))
    (goto-char beg)
    (while (re-search-forward "\\([a-z]\\{3\\}\\):\\([^[:space:]]+\\)" end t)
      (let* ((m (match-data))
	     ;; The first pair of entries (0 1) give the whole match,
	     ;; the following pairs are captures
	     (key (buffer-substring (nth 2 m) (nth 3 m)))
	     (value (buffer-substring (nth 4 m) (nth 5 m))))
	(setf (gethash key passport) value)))

    ;; Bonus sanity check - check if we're at or near the end - if not,
    ;; the regexp may have failed to pick up a valid entry
    (when (> (- end (point)) 2)
      (error "Reading passport may have failed at %d" (point)))

    passport))



;;; Lisp compatibility note: unlike CL, #'member and #'delete always
;;; use #'equal for comparison, so this function works


(defun passport-missing-fields (pp)
  "For a passport entry, return a list of missing fields in the passport"
  (let ((missing (copy-sequence +fields+)))
    (maphash (lambda (k v)
	       (declare (ignore v))
	       (unless (member k missing)
		 (error "Cannot happen - Suspicious field %s found" k))
	       (setq missing (delete k missing)))
	     pp)
    missing))



(defmacro incf (var)
  "Simple increment-by-one incf"
  `(setq ,var (1+ ,var)))



(defun list-empty-or-cid-p (x)
  "Check if the list is empty or contains only the string \"cid\""
  (or
   ;; No missing entries
   (endp x)
   ;; One missing entry, and it's "cid"
   (and (equal (car x) "cid") (endp (cdr x)))))



(defun solve1 nil
  "Return the number of entries with anything other than \"cid\" missing, and the total number, as a cons pair"
  (solve (lambda (pp)
	   (list-empty-or-cid-p (passport-missing-fields pp)))))



(defun solve2 nil
  "Return the number of entries that are valid according to the criteria of the puzzle"
  (solve #'passport-valid-p))



(defun passport-valid-p (pp)
  "Check whether the passport is valid according to part 2 of the puzzle"
  (let ((missing (passport-missing-fields pp)))
    (and
     (list-empty-or-cid-p missing)	; no missing entries
     ;; Hm, maybe making the passport an alist would have been easier?
     ;;
     ;; Squeezing the test in here means we only test passports with
     ;; no missing fields.
     (let ((all-good t))
       (maphash (lambda (k v)
		  ;; If there's a dodgy entry, no point running the rest of the tests!
		  (when (and all-good (not (passport-field-good-p k v)))
		    (setq all-good nil)))
		pp)
       all-good))))



(defun solve (pred)
  "Count the number of passport entries satisying pred, returning the count and the total number of entries as a cons pair"
  (let ((good 0) (all 0))
    (save-current-buffer
      (set-buffer +data-buffer+)
      (goto-char 0)
      (let (pp)
	(while (setq pp (read-next-passport))
	  (when (funcall pred pp) (incf good))
	  (incf all))))
    (cons good all)))



;;; Field check dispatch function

(defun passport-field-good-p (key value)
  "Check field value according to the type of field"
  ;; Can't use case as it's in cl... we're trying to do plain ELisp
  (cond
   ((equal key "byr") (passport-field-good-yr value 1920 2002))
   ((equal key "iyr") (passport-field-good-yr value 2010 2020))
   ((equal key "eyr") (passport-field-good-yr value 2020 2030))
   ((equal key "hgt") (passport-field-good-hgt value))
   ((equal key "hcl") (passport-field-good-hcl value))
   ((equal key "ecl") (member value '("amb" "blu" "brn" "gry" "grn" "hzl" "oth")))
   ((equal key "pid") (string-match "^[[:digit:]]\\{9\\}$" value))
   ((equal key "cid") t)
   (t (error "Unknown key %s encountered" key))))

;;; And the dispatched-to functions

(defun passport-field-good-yr (s low hi)
  "Check string is a number between low and hi (inclusive)"
  (and
   (string-match "^[[:digit:]]\\{4\\}$" s)
   (<= low (string-to-number s) hi)))


(defun passport-field-good-hgt (s)
  "Check height field according to specifications"
  (if (string-match "^\\([[:digit:]]\\{2,3\\}\\)\\(in\\|cm\\)$" s)
      (let ((num (match-string 1 s))
	    (unit (match-string 2 s)))
	(or
	 (and (equal unit "in") (<= 59 (string-to-number num) 76))
	 (and (equal unit "cm") (<= 150 (string-to-number num) 193))))
    ;; No match
    nil))


(defun passport-field-good-hcl (s)
  "Check hcl names a colour as #RRGGBB"
  (string-match "^#[[:xdigit:]]\\{6\\}$" s))
