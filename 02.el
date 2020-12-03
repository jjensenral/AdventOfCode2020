;;; 2020 Advent of Code
;;; 02 December
;;; Read buffer of data, counting the occurrence of a letter in a string
;;; <low> hyphen <high> space <letter> colon space <string> 
;;;
;;; jens.jensen@stfc.ac.uk


(defvar +line-regexp+ "^\\([0-9]+\\)-\\([0-9]+\\) \\([a-z]\\): \\([a-z0-9]+\\)$"
  "Regular expression for matching lower, upper count, letter, and string")


;; read-line is not used in ELISP
(defun read-line ()
  "Attempt to read line defined by +line-regexp+ at (or after) current position, returning a list of (low hi char word) or nil if it failed.
The position is after the match."
  (if (re-search-forward +line-regexp+
			   nil		; unbounded
			   t		; do not raise error
			   )
    (let* ((m (match-data))
	   ;; Ignore first pair (entries 0-1) which is whole line
	   (low (buffer-substring-no-properties (nth 2 m) (nth 3 m)))
	   (high (buffer-substring-no-properties (nth 4 m) (nth 5 m)))
	   (char (buffer-substring-no-properties (nth 6 m) (nth 7 m)))
	   (pwd (buffer-substring-no-properties (nth 8 m) (nth 9 m))))
      (list
       (string-to-number low)
       (string-to-number high)
       (string-to-char char)
       pwd))

    ;; re-search-forward returns nil
    nil))



(defun solve1 nil
  "Assuming data has been loaded into a read-only buffer called 02.input, count the number of good \"passwords\" and all of them"
  (save-current-buffer
    (set-buffer "02.input")
    (goto-char 0)
    (let (m
	  (good 0)
	  (all 0))
      
      (while (setq m (read-line))
	(when (<= (first m) (count (third m) (fourth m)) (second m))
	  (setq good (1+ good)))
	(setq all (1+ all)))

      (cons good all))))



(defun xor (a b)
  "Return logic XOR of two boolean values, ie true if exactly one is true"
  (if a (not b) b))




(defun solve2 nil
  "Assuming data has been loaded into a read-only buffer called 02.input, count the number of good \"passwords\" and all of them"
  (save-current-buffer
    (set-buffer "02.input")
    (goto-char 0)
    (let (m
	  (good 0)
	  (all 0))
      
      (while (setq m (read-line))
	(when (xor (eql (aref (fourth m) (1- (first m))) (third m))
		   (eql (aref (fourth m) (1- (second m))) (third m)))
	  (setq good (1+ good)))
	(setq all (1+ all)))

      (cons good all))))
