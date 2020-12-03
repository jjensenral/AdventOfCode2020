;;; Advent of Code 2020
;;; 03 December
;;; jens.jensen@stfc.ac.uk
;;;
;;; Assumes input data has been loaded into a buffer called 03.input.
;;; This buffer can be read-only.


(defvar +data-buffer+ "03.input"
  "Name of buffer containing input data")

(defvar +width+ nil
  "Width of line for input data")

(defvar +length+ nil
  "Length (number of lines in buffer)")


(defun set-buffer-width-length nil
  (save-current-buffer
    (set-buffer +data-buffer+)
    (goto-char 0)
    (when (re-search-forward "^[\\.#]+$")
      (let ((m (match-data)))
	(setq +width+ (- (second m) (first m)))))
    (goto-char 0)
    (setq +length+ (1- (how-many "^.*$"))))
  (cons +length+ +width+))


(defun thing-at (x y)
  "Return the character at line x column y (0 indexed); pretends that columns repeat ad inf by replicating the column"
  ;; goto-line is absolute and sets point at beginning of line
  (goto-line (1+ x))
  (forward-char (mod y +width+))
  (char-after (point)))


(defmacro incf (var &optional delta)
  `(setq ,var (+ ,var (or ,delta 1))))



(defun solve1 (step-x step-y)
  (save-current-buffer
    (set-buffer +data-buffer+)
    (set-buffer-width-length)
    (goto-char 0)	  ; probably not needed

    ;; c counts the number of occurrences of ?\#
    ;; y and x are the column and line number, respectively.
    ;; The first point is not counted (and it is a ?\. anyway)
    (let ((c 0) (y step-y) (x step-x))
      (while (< x +length+)
	(when (eql ?# (thing-at x y))
	  (incf c))
	(incf x step-x)
	(incf y step-y)
	)

      c)))



(defun solve2 ()
  "Solve for more slopes, returning the product of the number of trees (#s) encountered"
  (let* ((slopes '((1 . 1) (1 . 3) (1 . 5) (1 . 7) (2 . 1)))
	 (trees (mapcar (lambda (xy) (solve1 (car xy) (cdr xy))) slopes)))
    ;; reduce is in 'cl
    (apply #'* trees)))
    
