;;;; Advent of Code
;;;; 17 Dec
;;;; jens.jensen@stfc.ac.uk


;;; (let ((y (make-hash-table :test #'equal)))
;;;   (read-grid-layer y +grid-test1+ 0)
;;;   (solve y #'iterate))
;;; => 112

;;; (time (let ((y (make-hash-table :test #'equal)))
;;;   (read-grid-layer-2 y +grid-input+ 0 0)
;;;   (solve y #'iterate-2)))
;;; (16.705002 . ....)



(defvar +grid-test1+ ".#.
..#
###
" "first test input")


(defvar +grid-input+ "##..#.#.
#####.##
#######.
#..#..#.
#.#...##
..#....#
....#..#
..##.#..
" "Full puzzle input")



;;; The best approach is probably to introduce a coordinate like (x y
;;; . z) (two cons cells, although a vector might also have worked)
;;; and build a hash table.
;;;
;;; For setting a point to nil, we can either (puthash (x y . z) nil
;;; table), or (remhash (x y . z) table).  They are functionally
;;; equivalent except the latter frees up memory when it eventually
;;; gc's.
;;;
;;; The main problem compared to native arrays is it becomes hard to
;;; print slices, but we'd be in trouble anyway since Emacs doesn't do
;;; multidimensional adjustable arrays.  Another problem is equality:
;;; (equalp (make-hash-table) (make-hash-table))
;;; => nil
;;;
;;; A possibly faster and more cunning approach would be to use
;;; bitmasks, although it would need some more thought.


(defun copy-grid (grid)
  "Return a direct clone of the given grid"
  (let ((newgrid (make-hash-table :test #'equal)))
    (maphash (lambda (k v) (puthash k v newgrid)) grid)
    newgrid))



(defun count-active-nbrs (grid xyz)
  "Return the number of active neighbours to the point xyz"
  (let ((count 0)
	(offset '(-1 0 1)))
    (dolist (dx offset)
      (dolist (dy offset)
	(dolist (dz offset)
	  (when (and (not (= 0 dx dy dz))
		     (gethash (list* (+ (car xyz) dx)
				     (+ (cadr xyz) dy)
				     (+ (cddr xyz) dz))
			      grid))
	    (incf count)))))
    count))



;;; (let ((y (make-hash-table :test #'equal)))
;;;   (read-grid-layer y ".#.
;;; ..#
;;; ###
;;; " 0))
;;; => #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data ((1 -1 . 0) t (2 -2 . 0) t (0 -3 . 0) t (1 -3 . 0) t (2 -3 . 0) t ...))



(defun read-grid-layer (grid string z)
  "Read data from a string, updating grid with the new values"
  (let ((pos 0)				; starting pos
	(y 0))
    (while (string-match "^[\\.#]+$" string pos)
      (setq pos (match-end 0)
	    y (1- y))
      (let ((row (match-string 0 string)))
	(dotimes (x (length row))
	  (when (eql (aref row x) ?#)
	    (puthash (list* x y z) t grid))))))
    grid)
      


;;; (let ((y (make-hash-table :test #'equal)))
;;;   (puthash '(2 1 . 3) t y)
;;;   (puthash '(-1 2 . 0) t y)
;;;   (grid-envelope y 1))
;;; => ((-2 . 3) (0 . 3) (-1 . 4))


(defun grid-envelope (grid delta)
  "Return a list of X Y and Z ranges (each being a cons cell), plus delta, ie delta=1 would expand the ranges by 1 in each dimension"
  ;; This is a bit simplistic but it works
  (let ((minx +1000000) (maxx -1000000)
	(miny +1000000) (maxy -1000000)
	(minz +1000000) (maxz -1000000))
    (maphash (lambda (k v)
	       (when v
		 (when (< (car k) minx) (setq minx (car k)))
		 (when (> (car k) maxx) (setq maxx (car k)))
		 (when (< (cadr k) miny) (setq miny (cadr k)))
		 (when (> (cadr k) maxy) (setq maxy (cadr k)))
		 (when (< (cddr k) minz) (setq minz (cddr k)))
		 (when (> (cddr k) maxz) (setq maxz (cddr k)))))
	     grid)
    (list (cons (- minx delta) (+ maxx delta)) (cons (- miny delta) (+ maxy delta)) (cons (- minz delta) (+ maxz delta)))))

		 


(defun iterate (grid)
  "Return a new grid which is the iteration of the first"
  (let ((newgrid (copy-grid grid))
    ;; Need to consider all surrounding cells, ie 1 out from current
    ;; envelope
	(range (grid-envelope grid 1)))

    ;; loop is probably the cleaner way to, er, loop.
    (loop for x from (caar range) upto (cdar range)
	  do (loop for y from (caadr range) upto (cdadr range)
		   do (loop for z from (caaddr range) upto (cdaddr range)
			    as xyz = (list* x y z)
			    as nbrs = (count-active-nbrs grid xyz)
			    do (update-cell newgrid xyz nbrs))))

    newgrid))



(defun update-cell (grid xyz nbrs)
  "Given a grid, a coordinate, and the number of neighbours, update in place the cell identified by the coordinate.  Returns t if it was updated, nil otherwise."
  (cond
   ;; Current cell is active, make inactive?
   ((and (gethash xyz grid) (not (<= 2 nbrs 3)))
    (remhash xyz grid) t)
   ;; Current cell is inactive, make active?
   ((and (not (gethash xyz grid)) (= nbrs 3))
    (puthash xyz t grid) t))
  nil)



;;; (let ((y (make-hash-table :test #'equal)))
;;;   (read-grid-layer y ".#.
;;; ..#
;;; ###
;;; " 0)
;;;   (print-grid y))^J
;;; z=0
;;; .#.
;;; ..#
;;; ###
;;; => nil


(defun print-grid (grid)
  "Print grid by layer, as in the puzzle description"
  (let ((envelope (grid-envelope grid 0)))
    (loop for z from (caaddr envelope) upto (cdaddr envelope)
	  do (princ (format "z=%d\n" z))
	  do (loop for y from (cdadr envelope) downto (caadr envelope)
		   do (loop for x from (caar envelope) upto (cdar envelope)
			    do (princ (format "%c" (if (gethash (list* x y z) grid) ?# ?.))))
		   do (terpri))
	  do (terpri)))
  nil)



(defun solve (grid iterate-function)
  "Iterate the grid six times before breakfast and count the number of #s.  The grid argument is not modified."
  (dotimes (i 6) (setq grid (funcall iterate-function grid)))
  (let ((count 0))
    (maphash (lambda (xyz val)
	       (when val		; should always be t
		 (incf count)))
	     grid)
    count))



;;; Part two is kind of like part 1, only with 1/3 more code...

(defun count-active-nbrs-2 (grid xyzw)
  "Return the number of active neighbours to the point xyz"
  (let ((count 0)
	(offset '(-1 0 1)))
    (dolist (dx offset)
      (dolist (dy offset)
	(dolist (dz offset)
	  (dolist (dw offset)
	    (when (and (not (= 0 dx dy dz dw))
		       (gethash (list* (+ (car xyzw) dx)
				       (+ (cadr xyzw) dy)
				       (+ (caddr xyzw) dz)
				       (+ (cdddr xyzw) dw))
				grid))
	      (incf count))))))
    count))



(defun read-grid-layer-2 (grid string z w)
  "Read data from a string, updating grid with the new values"
  (let ((pos 0)				; starting pos
	(y 0))
    (while (string-match "^[\\.#]+$" string pos)
      (setq pos (match-end 0)
	    y (1- y))
      (let ((row (match-string 0 string)))
	(dotimes (x (length row))
	  (when (eql (aref row x) ?#)
	    (puthash (list* x y z w) t grid))))))
  grid)



(defun grid-envelope-2 (grid delta)
  "Return a list of X Y and Z ranges (each being a cons cell), plus delta, ie delta=1 would expand the ranges by 1 in each dimension"
  ;; This is a bit simplistic but it works
  (let ((minx +1000000) (maxx -1000000)
	(miny +1000000) (maxy -1000000)
	(minz +1000000) (maxz -1000000)
	(minw +1000000) (maxw -1000000))
    (maphash (lambda (k v)
	       (when v
		 (when (< (car k) minx) (setq minx (car k)))
		 (when (> (car k) maxx) (setq maxx (car k)))
		 (when (< (cadr k) miny) (setq miny (cadr k)))
		 (when (> (cadr k) maxy) (setq maxy (cadr k)))
		 (when (< (caddr k) minz) (setq minz (caddr k)))
		 (when (> (caddr k) maxz) (setq maxz (caddr k)))
		 (when (< (cdddr k) minw) (setq minw (cdddr k)))
		 (when (> (cdddr k) maxw) (setq maxw (cdddr k)))))
	     grid)
    (list (cons (- minx delta) (+ maxx delta)) (cons (- miny delta) (+ maxy delta)) (cons (- minz delta) (+ maxz delta)) (cons (- minw delta) (+ maxw delta)))))



(defun iterate-2 (grid)
  "Return a new grid which is the iteration of the first"
  (let ((newgrid (copy-grid grid))
    ;; Need to consider all surrounding cells, ie 1 out from current
    ;; envelope
	(range (grid-envelope-2 grid 1)))

    ;; loop is probably the cleaner way to, er, loop.
    (loop for x from (caar range) upto (cdar range)
	  do (loop for y from (caadr range) upto (cdadr range)
		   do (loop for z from (caaddr range) upto (cdaddr range)
			    do (loop for w from (car (cadddr range)) upto (cdr (cadddr range))
				     as xyzw = (list* x y z w)
				     as nbrs = (count-active-nbrs-2 grid xyzw)
				     do (update-cell newgrid xyzw nbrs)))))

    newgrid))


(defun print-grid-2 (grid)
  "Print grid by layer, as in the puzzle text"
  (let ((envelope (grid-envelope-2 grid 0)))
    (loop for w from (car (nth 3 envelope)) upto (cdr (nth 3 envelope))
	  do (loop for z from (caaddr envelope) upto (cdaddr envelope)
		   do (princ (format "z=%d, w=%d\n" z w))
		   do (loop for y from (cdadr envelope) downto (caadr envelope)
			    do (loop for x from (caar envelope) upto (cdar envelope)
				     do (princ (format "%c" (if (gethash (list* x y z w) grid) ?# ?.))))
			    do (terpri))
		   do (terpri))))
  nil)
