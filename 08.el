;;;; Advent of Code 2020
;;;; 08 December
;;;; jens.jensen@stfc.ac.uk
;;;;
;;;; Load utils.el, then this code.
;;;; Entry points are #'solve1 and #'solve2


(defvar +data-buffer+ "08.input"
  "buffer containing the data to process")


;;; For convenience, we use code from utils.el (in the same directory)
;;; where code that seems to be common to most puzzles now lives.


;;; Test data returns [("nop" . 0) ("acc" . 1) ("jmp" . 4) ("acc" . 3) ("jmp" . -3) ("acc" . -99) ("acc" . 1) ("jmp" . -4) ("acc" . 6)]
;;;
;;; Compatibility note - Emacs vectors use different notation from Common Lisp vectors [1 2 3] vs #(1 2 3)

(defun read-code nil
  "Read code from the buffer, returning a vector of cons cells"
  (coerce
   (map-data-buffer "^\\([a-z]\\{3\\}\\) \\([-\\+][0-9]+\\)$" (lambda (op num) (cons op (string-to-int num))))
   'vector))


;;; Test "natural end"
;;; (evaluate [("nop" . 0) ("acc" . +1)] (lambda (a b c d) nil))
;;; => ((accumulator . 1) (step-number . 2) (visited 0 1) (program-counter . 2) (natural-end . t))
;;;
;;; If a program jumped off the end otherwise (like a ("jmp" . +10) in
;;; the code above), an out of bounds exception would be raised.


(defun standard-stop-function (acc step visited pc)
  "Stop function as used by #'evaluate, used to catch \"infinite loops\" by detecting whether we are about to execute an instruction that has been executed before"
  (declare (ignore acc step))
  (member pc visited))


(defun evaluate (code stop-func)
  "Evaluate the code according to the instructions, stop func is passed the state of the evaluator immediately before the next instruction and should return true if it needs the evaluator to stop.  The final state is returned."
  (let ((accumulator 0)
	(step-number 0)
	(code-length (length code))	; length of an array is quite slow so we cache it
	(visited nil)			; locations (as defined by program counter) visited so far
	(program-counter 0)		; program-counter is the index into the code array of the current instr.
	(natural-end nil)
	(running t))

    (while running
      (if (funcall stop-func accumulator step-number visited program-counter)
	  (setq running nil)
	(progn				; else
	  (push program-counter visited)
	  (incf step-number)
	  (let* ((instr (aref code program-counter)))
	    (cond
	     ((equal (car instr) "nop") (incf program-counter))
	     ((equal (car instr) "acc") (incf accumulator (cdr instr)) (incf program-counter))
	     ((equal (car instr) "jmp") (incf program-counter (cdr instr)))
	     (t (error "Unknown instruction %s encountered at %d" (car instr) program-counter))))))

      ;; check for a natural end of the program
      (when (= program-counter code-length) (setq running nil natural-end t)))

    (list (cons 'accumulator accumulator)
	  (cons 'step-number step-number)
	  (cons 'visited (nreverse visited))
	  (cons 'program-counter program-counter)
	  (cons 'natural-end natural-end))))




;;; #'time is also from utils.  Here, +data-buffer+ is "08.input" which is loaded into a buffer already.
;;; (time (solve1))
;;; (0.002082 (accumulator . 1810) (step-number . 174) (visited 0 236 237 238 496 497 573 574 575 120 130 ...) (program-counter . 573) (natural-end))


(defun solve1 nil
  "First puzzle question"
  (evaluate (read-code) #'standard-stop-function))



(defun try-evaluate (code)
  "Run code, checking for natural exit vs exception or infinite loop; returning the value of the accumulator for a natural end, or nil otherwise"
  (condition-case nil

      ;; Evaluate code with the same stop function as before
      (let ((result (evaluate code #'standard-stop-function)))
	(and (cdr (assoc 'natural-end result)) (cdr (assoc 'accumulator result))))

    ;; Catch array out of bounds (a jump to lala land).
    ;; We want other errors to be raised, eg. unknown instruction.
    (args-out-of-range nil)))



;;; The strategy for the second part is to look at the visited instructions, starting with the last
;;; (so reversing the visited list once again).

(defun solve2 nil
  "Second puzzle question - fix the program"
  (catch 'found
    (let ((code (read-code))
	  (reverse-visited (nreverse (cdr (assoc 'visited (solve1))))))
      (dolist (candidate reverse-visited)
	(let* ((old-instr (aref code candidate))
	       (old-cmd (car old-instr))
	       (new-cmd (cond
			   ((equal old-cmd "nop") "jmp")
			   ((equal old-cmd "jmp") "nop")
			   (t nil))))
	  (when new-cmd
	    ;; Replace the #'car of the cell in place
	    (rplaca old-instr new-cmd)
	    (awhen (try-evaluate code) (throw 'found it))
	    ;; Restore the instruction and move on
	    (rplaca old-instr old-cmd)))))))

	 
	 
