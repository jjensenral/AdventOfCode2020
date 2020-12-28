;;;; AdventOfCode 2020
;;;; 11 Dec
;;;; jens.jensen@stfc.ac.uk

;;; As usual, the data buffer holds the input
;;; For convenience we use the reader code from utils.el (in the
;;; same repo) to hold some of the code, so load that, too.
;;;
;;; BUG BUG BUG
;;; There is a bug in the second part; it works on the test data but
;;; not the production data.


(defvar +data-buffer+ "11.input"
  "Data to be processed")

(defvar +max-iter+ 500
  "Maximum number of iterations to allow")


;;; #'equal can be used for comparison of objects in Emacs'
;;; implementation (at least instances of this particular class)

(defclass seats nil
  ((rows :type integer :reader seats-rows :initarg :seats-in-row)
   (cols :type integer :reader seats-cols :initarg :seats-in-col)
   (data :type list :initarg :data :accessor seats-data))
  :documentation "Arrangement of seats")


(defun copy-seats (seats)
  "Deep copy of seats object"
  (make-instance 'seats
		 :seats-in-row (slot-value seats 'rows)
		 :seats-in-col (slot-value seats 'cols)
		 :data (mapcar #'copy-seq (seats-data seats))))


(defun seats-in-bounds-p (seats row col)
  "Return whether both row and col reference valid row, resp. col"
  (and (< -1 row (seats-rows seats))
       (< -1 col (seats-cols seats))))


(defun seats-whats-at (seats row col)
  "Return the element at row col or nil if there isn't one"
  (if (seats-in-bounds-p seats row col)
      (aref (nth row (seats-data seats)) col)
    nil))


(defun seats-flip (seats row col)
  "Update seats in place, (de)occupying seat at row col"
  (let* ((c (seats-whats-at seats row col))
	 (new-c (if (eql c ?#) ?L (if (eql c ?L) ?# c))))
    (setf (aref (nth row (seats-data seats)) col) new-c)))


(defun pretty-print-seats (seats)
  "Simple pretty-print function"
  ;; ancient lisp non-magic
  (mapc (lambda (s) (princ s) (terpri)) (seats-data seats)))


(defun read-seats nil
  "Read seats from the buffer defined by +data-buffer+ and return the seats class"
  (let ((raw-data (map-data-buffer "^[#L\\.]+$" #'identity)))
    (make-instance 'seats :seats-in-row (length raw-data)
		   :seats-in-col (length (car raw-data))
		   :data raw-data)))


(defun count-neighbours (seats row col)
  "Count the number of neighbours (occupied seats adjacent) to the seat at row and col"
  (count ?# (mapcar (lambda (delta) (seats-whats-at seats (+ row (car delta)) (+ col (cdr delta))))

		    '((1 . 0) (1 . 1) (-1 . 0) (1 . -1)
		      (0 . 1) (-1 . -1) (0 . -1) (-1 . 1)))))


(defun update-seats (seats)
  "Update seats, returning a fresh object according to the rules"
  (let ((new-seats (copy-seats seats)))
    (dotimes (row (seats-rows seats))
      (dotimes (col (seats-cols seats))
	(let* ((s (seats-whats-at seats row col))
	       (nbrs (if (eql s ?.) 0 (count-neighbours seats row col))))
	  (when (or (and (eql s ?L) (zerop nbrs))
		    (and (eql s ?#) (>= nbrs 4)))
	    (seats-flip new-seats row col)))))
    new-seats))



(defun solve1 nil
  (let* ((maxiter 50)
	 (s (read-seats))
	 (s1 (update-seats s)))
    (while (not (or (equal s1 s) (zerop maxiter)))
      (setq s s1 s1 (update-seats s1) maxiter (1- maxiter)))
    (if (zerop maxiter)
	"Maximum iterations exceeded"
      (apply #'+ (mapcar (lambda (str) (count ?# str)) (seats-data s))))))

