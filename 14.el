;;;; Advent of Code 2020
;;;; 14.el
;;;; jens.jensen@stfc.ac.uk

;;;; Uses the incf macro from utils (in the same repo)
;;;; and map-data-buffer and maybe some more stuff.


;;; It would be nice to bind this locally as with
;;; ( ... (declare (special *data-buffer*)) ... )
;;; but ELisp does this in a different way which doesn't seem to work (yet?)
(defvar +data-buffer+ "14.input")


(defun make-mask (template)
  "Create a function that implements the mask template.  The template should be a string of ?X, ?0 and ?1s."
  (let* ((word-length (length template))
	 (full-word (1- (ash 1 word-length)))
	 (ones (make-mask-1 template ?1))
	 (zeros (logxor full-word (make-mask-1 template ?0))))
    `(lambda (reg) (logior ,ones (logand reg ,zeros)))))


(defun make-mask-1 (template char)
  "Convert letters to bits"
  (let ((result 0))
    (mapc (lambda (c)
	    (setq result (logior (lsh result 1) (if (eql c char) 1 0))))
	  template)
    result))



;;; memory is implemented as a hash table rather than an array

;;; (let ((y (make-hash-table))) (puthash 'a 3 y) (puthash 'b 7 y) (sum-memory y))
;;; => 10

(defun sum-memory (mem)
  "Return the sum of the values in memory"
  (let ((result 0))
    (maphash (lambda (k v)
	       (declare (ignore k))
	       (incf result v))
	     mem)
    result))



;;; (decode-instruction (cons "mask" "1XX0"))
;;; => (mask lambda (reg) (logior 8 (logand reg 14)))
;;;
;;; (decode-instruction (cons "mem[999]" "299792458"))
;;; => (999 . 299792458)


(defun decode-instruction (instr mask-func)
  "Instruction is a cons cell with the stuff before and after the '='"
  (cond
   ((equal (car instr) "mask") (cons 'mask (funcall mask-func (cdr instr))))
   ((string-match "^mem\\[\\([0-9]+\\)\\]$" (car instr))
    (cons (string-to-int (match-string 1 (car instr))) (string-to-int (cdr instr))))
   (t (error "Unable to parse instruction %s" instr))))



(defun execute-memory-instruction (instr mask memory)
  "Execute a single (decoded, valid) mem instruction with the current mask, modifying memory accordingly"
  (puthash (car instr) (funcall mask (cdr instr)) memory))



(defun execute-buffer (mask-func mem-func)
  "Execute code from Day 14"
  (let ((data (map-data-buffer "^\\([^ ]+\\) = \\([0-9X]+\\)$" (lambda (a b) (cons a b))))
	(memory (make-hash-table))
	(mask #'identity))
    (while data
      (let ((instr (decode-instruction (car data) mask-func)))
	(cond
	 ((eq (car instr) 'mask) (setq mask (cdr instr)))
	 ((integerp (car instr)) (funcall mem-func instr mask memory))
	 (t (error "Unknown entry %s" (car data)))))
      (pop data))
    ;; Finally the result
    (sum-memory memory)))


(defun solve1 nil
  (execute-buffer #'make-mask #'execute-memory-instruction))


;;; Part two of puzzle seems to require rewriting some core bits
;;; and it takes nearly 2/3 of a second to run!
;;; (time (solve2)) => (0.623429 . ...)

(defun solve2 nil
  (execute-buffer #'make-mask2 #'execute-memory-instruction2))


;;; Now a mask is more complicated
;;; We can ignore the zeros, and record the position of the Xes
;;; We return (ones pos1 pos2 ..)
;;;
;;; (make-mask2 "000000000000000000000000000000X1001X")
;;; => (36 18 0 5)
;;; - 18 is #b10010
;;; (make-mask2 "00000000000000000000000000000000X0XX")
;;; => (36 0 0 1 3)

(defun make-mask2 (template)
  "Second mask, returning a list word length, of ones, and the positions of the Xes as bit values"
  (let ((word-length (length template))		; as before
	(ones (make-mask-1 template ?1))	; as before
	(xpos nil)
	(offs 0))			; offset into template
    (awhile (position ?X template :start offs)
	    ;; bits are counted from the other end
	    (push (- word-length it 1) xpos)
	    (setq offs (1+ it)))
    (list* word-length ones xpos)))


;;; (explode-number 7 '(0 1 3)) => (#b1011 . #b1011)
;;; (explode-number 5 '(0 1 3)) => (#b1001 . #b1011)
;;; (explode-number 3 '(0 1 3)) => (#b0011 . #b1011)

(defun explode-number (num bits)
  "Take number and move its bits to the locations specified in bits where bits has LSB first; also returns a mask with 1s in the same positions"
  (let ((result 0) (mask 0))
    (dolist (b bits)
      (setq result (logior result (lsh (logand num 1) b))
	    mask (logior mask (lsh 1 b))
	    num (lsh num -1)))
    (cons result mask)))


(defun execute-memory-instruction2 (instr mask memory)
  "Execute memory instruction according to second set of rules"
  (let* ((full-word (1- (lsh 1 (car mask))))
	 (base (logior (car instr) (cadr mask))) ; add the ones
	 (bits (cddr mask))
	 (num-x (length bits))		; number of Xes
	 (val (cdr instr)))		; value
    (dotimes (w (lsh 1 num-x))
      (let ((enum (explode-number w bits)))
	;; Clear 1s in (cdr enum) from base, then add 1s
	;; from (car enum), and store
	(puthash (logior (car enum) (logand (logxor full-word (cdr enum)) base)) val memory)))))

