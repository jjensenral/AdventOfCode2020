;;;; Advent of Code
;;;; Day 13
;;;; jens.jensen@stfc.ac.uk

;;; These are just the inputs with commas replaced with space and x
;;; with nil


(defvar +test+ '(939 7 13 nil nil 59 nil 31 19))

;;; Reminder about defvar, it will define variables only if they are
;;; unbound.  If you have one already, it does not override it and
;;; you'll need to use setq instead.
(defvar +input+ '(1001171 17 nil nil nil nil nil nil 41 nil nil nil 37 nil nil nil nil nil 367 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 19 nil nil nil 23 nil nil nil nil nil 29 nil 613 nil nil nil nil nil nil nil nil nil nil nil nil 13))



(defun least-multiple-greater (k m)
  "Find the least multiple of m greater or equal to k"
  (* m (ceiling k m)))


(defun solve1 (data)
  (let* ((target (car data))
	 (times (remove nil (cdr data)))
	 (d (mapcar (lambda (m) (- (least-multiple-greater target m) target)) times))
	 (y (apply #'min d))		; least number of minutes
	 (z (position y d)))		; index of least number
    (* y (nth z times))))



;;; The optionals below are just used on recursion, saves us
;;; writing a separate helper function (which one might do for
;;; cleaner code, or high performance).
;;;
;;; (make-positions '(a nil b c nil d nil))
;;; => ((a . 0) (b . 2) (c . 3) (d . 5))

(defun make-positions (elts &optional k)
  "Pair non-nil elements with their index"
  (cond
   ((endp elts) nil)
   ((null (car elts)) (make-positions (cdr elts) (1+ (or k 0))))
   (t (cons (cons (car elts) (or k 0))
	  (make-positions (cdr elts) (1+ (or k 0)))))))


;;; (pairwise-gcds '(2 4 12 8))
;;; (2 2 2 4 4 4)
;;; (pairwise-gcds (remove nil (cdr +test+))) => (1 1 ...)
;;; (find-if-not (lambda (x) (eql x 1)) (pairwise-gcds (remove nil (cdr +input+))))
;;; => nil
;;;
;;; So all the inputs are pairwise coprime.

(defun pairwise-gcds (elts)
  "Return a list of the pairwise gcds"
  (cond
   ;; 1 or 0 elements left
   ((endp (cdr elts)) nil)
   (t (nconc (mapcar (lambda (b) (gcd (car elts) b)) (cdr elts))
	     (pairwise-gcds (cdr elts))))))



;;; For Euclid's extended algorithm.  This could probably be
;;; made a bit more elegant but it'll do for now.

(defun divisors (a b)
  ;; Compatibility note - #'floor in Common Lisp returns both
  ;; quotient and remainder
  (let ((r (mod a b))
	(q (floor a b)))
    (if (zerop r) (cons q nil)
      (cons q (divisors b r)))))

;;; Given coprime (a,b) the coefficients x and y s.t. ax+by=1
;;; can be found by passing 1 0 divs, resp. 0 1 divs, into
;;; this function, with divs being the divisors above.

(defun addup (s0 s1 divs)
  (if divs
      (addup s1 (- s0 (* (car divs) s1)) (cdr divs))
    s0))


(defun inverse (k m)
  "Find an inverse of k modulo m, provided (gcd k m) is 1."
  ;; The otherwise superfluous mod ensures the number returned
  ;; is in range 0 .. m-1
  (mod (addup 1 0 (divisors k m)) m))


;;; For the kth entry m_k, we need the least t s.t. t is congruent to
;;; -k modulo m_k.
;;;
;;; Since they are pairwise coprime (see above), we can use the
;;; Chinese Remainder Theorem, eg
;;; https://mathworld.wolfram.com/ChineseRemainderTheorem.html
;;;


(defun solve2 (data)
  (let* ((target (car data))
	 (times-offs (make-positions (cdr data)))
	 ;; Unzip
	 (times (mapcar #'car times-offs))
	 (offs (mapcar (lambda (x) (- (cdr x)))  times-offs))

	 ;; Compatibility note: ELisp is case dependent
	 ;; (nitpicking note - so is CL, but the Reader normally
	 ;; uppercases symbol names)

	 (M (apply #'* times))
	 (b (mapcar (lambda (m) (inverse (/ M m) m)) times))
	 (T (mod
	     (apply #'+
		    (map 'list #'*
			 offs
			 b
			 (mapcar (lambda (m) (/ M m)) times)))
	     M)))

    ;; OK, after all this work, we may have a solution.
    ;; Check we haven't overflowed
    (unless (typep M 'integer) (error "Multiple is %s" M))
    T))
