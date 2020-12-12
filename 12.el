;;;; Advent of Code 2020
;;; Day 12
;;; jens.jensen@stfc.ac.uk

;;; Load util.el for dependencies

(defvar +data-buffer+ "12.input"
  "data for the puzzle")

;;; Ship status is a simple list (east north . facing)

(defun ship-east (ship) (car ship))
(defun ship-north (ship) (cadr ship))
(defun ship-facing (ship) (cddr ship))

(defsetf ship-east (ship) (fred) `(setf (car ,ship) ,fred))
(defsetf ship-north (ship) (wilma) `(setf (cadr ,ship) ,wilma))
(defsetf ship-facing (ship) (barney) `(setf (cddr ,ship) ,barney))


;;; Note that north repeats.  We could also have used a wrap-around
;;; list but this is easier; we turn by taking the value following the
;;; current one.
;;;
;;; We use the . below to make it easier to read and understand how
;;; it's used (with assoc); of course (a . (b c)) is (a b c)

(defvar +directions+ '((?L . (north west south east north))
		       (?R . (north east south west north)))
  "Directions round the compass")



(defun read-data nil
  "Read data from buffer in +data-buffer+ and parse, returning a list of cons cells with instruction as character and number"
  (map-data-buffer "^\\([NSEWLRF]\\)\\([0-9]+\\)$"
		   (lambda (d n)
		     (cons (string-to-char d) (string-to-int n)))))


;;; (turn-ship '(0 0 . east) '(?R . -90))
;;; => (0 0 . north)


(defun turn-ship (ship cmd)
  "cmd is ?L or ?R, update the ship and return it"
  (let ((dir (car cmd))
	(val (cdr cmd))
	(facing (ship-facing ship)))
    (when (< val 0)
      (setq val (- val) dir (if (eql dir ?L) ?R ?L)))
    (unless (zerop (mod val 90))
      (error "Unexpected turn %d degrees" val))
    (setq val (/ val 90))
    ;; OK we now need to turn "dir" "val" times (which could be 0?)
    (while (> val 0)
      ;; This works by (1) using assoc to look up the directions
      ;; and then (2) using member to find the sublist starting with
      ;; the current direction and pick the next with cadr
      (setq facing (cadr (member facing (cdr (assoc dir +directions+))))
	    val (1- val)))
    (setf (ship-facing ship) facing))
  ship)



(defun go-ship (ship cmd)
  "Move the ship according to the instruction, modifying ship and returning it as well"
  (case (car cmd)
    (?N (incf (ship-north ship) (cdr cmd)))
    (?S (incf (ship-north ship) (- (cdr cmd))))
    (?E (incf (ship-east ship) (cdr cmd)))
    (?W (incf (ship-east ship) (- (cdr cmd))))
    ((?L ?R) (turn-ship ship cmd))
    (?F (case (ship-facing ship)
	  (east (incf (ship-east ship) (cdr cmd)))
	  (west (incf (ship-east ship) (- (cdr cmd))))
	  (north (incf (ship-north ship) (cdr cmd)))
	  (south (incf (ship-north ship) (- (cdr cmd))))
	  (t (error "Unknown direction %s" (ship-facing ship)))))
    (t (error "Unknown command %s" (car cmd))))
  ship)



(defun sea-trials nil
  "See if the ship sails as she should"
  (let ((directions '((?F . 10) (?N . 3) (?F . 7) (?R . 90) (?F . 11)))
	(ship (list* 0 0 'east)))
    (mapc (lambda (d) (go-ship ship d)) directions)
    (if
	(equal ship '(17 -8 . south))
	t
      (format "Expected %s got %s" '(17 -8 . south) ship))))



(defun solve1 nil
  (let ((ship (list* 0 0 'east))
	(cmds (read-data)))
    (mapc (lambda (d) (go-ship ship d)) cmds)
    (+ (abs (ship-east ship)) (abs (ship-north ship)))))


(defun turn-wayp (wayp cmd)
  "Turn waypoint according to cmd which should be (?L . deg) or (?R . deg) with deg a non-negative multiple of 90.  It returns the value rather than modify it."
  ;; Didn't see any funny stuff in the first part so we spend less
  ;; time tidying data in the second
  (let ((val (/ (cdr cmd) 90)))
    ;; Make all turns right turns
    (when (eql (car cmd) ?L) (setq val (- 4 val)))
    ;; val should now be between 0 and 4
    (let ((east (ship-east wayp)) (north (ship-north wayp)))
      (case val
	((0 4) wayp)
	(1 (setf (ship-east wayp) north
		 (ship-north wayp) (- east)))
	(2 (setf (ship-east wayp) (- east)
		 (ship-north wayp) (- north)))
	(3 (setf (ship-east wayp) (- north)
		 (ship-north wayp) east))
	(t (error "Shouldn't happen val=%d" val)))))
  wayp)



;;; The ship accessors will also work for the waypoint.
;;; Note the waypoint location is relative to the ship,
;;; not to the origin
(defun go-ship-or-waypoint (ship wayp cmd)
  (case (car cmd)
    (?N (incf (ship-north wayp) (cdr cmd)))
    (?S (incf (ship-north wayp) (- (cdr cmd))))
    (?E (incf (ship-east wayp) (cdr cmd)))
    (?W (incf (ship-east wayp) (- (cdr cmd))))
    ((?L ?R) (turn-wayp wayp cmd))
    (?F (incf (ship-east ship) (* (cdr cmd) (ship-east wayp)))
	(incf (ship-north ship) (* (cdr cmd) (ship-north wayp))))
    (t (error "Unknown command %s" (car cmd)))))


(defun sea-trials-2 nil
  (let ((directions '((?F . 10) (?N . 3) (?F . 7) (?R . 90) (?F . 11)))
	(ship (list* 0 0 'east))
	;; wayp is like a ship but facing nil
	(wayp (list 10 1)))
    (mapc (lambda (d) (go-ship-or-waypoint ship wayp d)) directions)
    (format "Got ship=%s and wayp=%s" ship wayp)))



(defun solve2 nil
  (let ((ship (list* 0 0 'east))
	(wayp (list 10 1))
	(cmds (read-data)))
    (mapc (lambda (d) (go-ship-or-waypoint ship wayp d)) cmds)
    (+ (abs (ship-east ship)) (abs (ship-north ship)))))
