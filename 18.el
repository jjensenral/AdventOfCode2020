;;;; Advent of Code 2020
;;;; Day 18
;;;; jens.jensen@stfc.ac.uk

;;; incf from util.el (or from elsewhere) is required

(defvar +data-buffer+ "18.input"
  "Name of buffer to load data from")



;;; (lex-expr "2 * 3 + (4 * 5)")
;;; => (2 * 3 + (4 * 5))
;;; (the * and + are the symbols '* and '+)

(defun lex-expr (expr)
  "process an expression (as a string) returning a list of items"
  (cond
   ;; Nothing to see here?
   ((string-match "^ *$" expr) nil)
   ;; Parenthesis expression
   ((string-match "^ *(" expr)
    (let ((paren-rest (lex-expr-paren expr)))
      (cons (lex-expr (car paren-rest))
	    (lex-expr (cdr paren-rest)))))
   ;; Number
   ((string-match "^ *\\([0-9]+\\)" expr) (cons (string-to-int (match-string 1 expr)) (lex-expr (substring expr (match-end 0)))))
   ;; Operator, we turn it into (the) symbol
   ((string-match "^ *\\([\\+\\*]\\)" expr) (cons (intern (match-string 1 expr)) (lex-expr (substring expr (match-end 0)))))
   (t (error "Unable to parse \"%s\"" expr))))



(defun lex-expr-paren (expr)
  "From a parenthesised expression return the contents as a string, plus the remaining part of the string, in a cons cell"
  (let* ((nested 1)
	 (len (length expr))
	 (pos (1+ (position 40 expr)))	; 40 is open paren
	 (q pos))
    (while (and (< q len) (> nested 0))
      (case (elt expr q)
	(40 (incf nested))		; another open paren
	(41 (decf nested)))		; close paren
      (incf q))
    (when (> nested 0) (error "Unmatched open paren in %s" expr))
    ;; q is now stepped past the matching close paren
    (cons
     (substring expr pos (1- q))
     (substring expr q))))



(defun eval-expr (lex-expr)
  "Evaluate an expression as returned by the lexer"
  (cond
   ((null lex-expr) (error "Unexpected end of input"))
   ((integerp lex-expr) lex-expr)
   ((atom lex-expr)
    (error "Unexpected token %s" lex-expr))
   (t					; it's a list
    (let ((acc (eval-expr (car lex-expr)))
	  (todo (cdr lex-expr)))
      (while todo
	(cond
	 ;; something operator something
	 ((symbolp (cadr lex-expr))
	  ;; We can actually funcall the symbol (in Lisp1)
	  ;; provided it names a function
	  (setq acc (funcall (car todo)
			     acc
			     (eval-expr (cadr todo)))
		todo (cddr todo)))
	 (t (error "Got stuck on %s" todo))))
      acc))))



(defun test1 nil
  (let ((test-data '(("2 * 3 + (4 * 5)" . 26)
		     ("1 + 2 * 3 + 4 * 5 + 6" . 71)
		     ("5 + (8 * 3 + 9 + 3 * 4 * 3)" . 437)
		     ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" . 12240)
		     ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" . 13632))))
    (dolist (test test-data)
      (let ((got  (eval-expr (lex-expr (car test)))))
	(unless (eql (cdr test) got)
	  (error "Failed %s, expected %d and got %d" (car test) (cdr test) got))))
    t))



(defun solve1 nil
  "Process data loaded into a buffer whose name is stored in +data-buffer+"
  (let ((result 0))
    (save-current-buffer
      (set-buffer +data-buffer+)
      (goto-char 0)
      (while (re-search-forward "^.+$" nil t)
	(incf result (eval-expr (lex-expr (match-string 0))))))
    result))



(defun test2 nil
  (let ((test-data '(("2 * 3 + (4 * 5)" . 46)
		     ("1 + (2 * 3) + (4 * (5 + 6))" . 51)
		     ("5 + (8 * 3 + 9 + 3 * 4 * 3)" . 1445)
		     ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" . 669060)
		     ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" . 23340))))
    (dolist (test test-data)
      (let ((got  (eval-expr-2 (car test))))
	(unless (eql (cdr test) got)
	  (error "Failed %s, expected %d and got %d" (car test) (cdr test) got))))
    t))



;;; The grammar is something like:
;;;
;;; expr -> expr * factor
;;; expr -> factor
;;; factor -> factor + term
;;; factor -> term
;;; term -> <integer>
;;; term -> ( expr )
;;;
;;; What sort of parser is required to parse this grammar?  It would
;;; be nice to use a predictive parser as it would be relatively
;;; simple to code; we can still do that by looking ahead for the
;;; distinguishing operator.
;;;
;;; Reference: A W Appel with M Ginsburg: Modern Compiler
;;; Implementation in C, ch. 3., CUP (1997)


(defun find-expr (lexed-expression symbol)
  "Search lexed-expression for the last occurrence of symbol (see grammar) and return the sublists before and after (as the car and cdr of a cons cell).  lexed-expression is unharmed but may share cells with the result"
  (let* ((pos (position symbol lexed-expression :from-end t))
	 (end (and pos (nthcdr (1+ pos) lexed-expression))))
    (if pos
	(cons (subseq lexed-expression 0 pos) end)
      nil)))



(defun parse-expr (lexed-expression)
  "Taking an expression from the lexer, this will return a parse tree corresponding to the second rule set"
  (let ((subexprs (find-expr lexed-expression '*)))
    (if subexprs
	(list '* (parse-expr (car subexprs))
	      (parse-factor (cdr subexprs)))
      (parse-factor lexed-expression))))



(defun parse-factor (lexed-expression)
  (let ((subexprs (find-expr lexed-expression '+)))
    (if subexprs
	(list '+ (parse-factor (car subexprs))
	      (parse-term (cdr subexprs)))
      (parse-term lexed-expression))))



(defun parse-term (lexed-expression)
  (cond
   ((integerp (car lexed-expression)) (car lexed-expression))
   ((consp (car lexed-expression)) (parse-expr (car lexed-expression)))
   (t (error "Unexpected term %s" (car lexed-expression)))))



(defun eval-expr-2 (string)
  "Evaluate an expression string according to the second set of rules"
  ;; Note we can just pass the whole tree directly to Lisp to
  ;; evaluate! because data is code! (sometimes)
  (eval (parse-expr (lex-expr string))))



(defun solve2 nil
  "Process data using rule 2 loaded into a buffer whose name is stored in +data-buffer+"
  (let ((result 0))
    (save-current-buffer
      (set-buffer +data-buffer+)
      (goto-char 0)
      (while (re-search-forward "^.+$" nil t)
	(incf result (eval-expr-2 (match-string 0)))))
    result))
  
