;;;; race.lisp

(in-package #:race)

;;; "race" goes here. Hacks and glory await!



(defparameter *track* nil)

(defparameter *track-parameters* nil)

(defun fill-array-with-lines (array lines)
  "Fill ARRAY with content of LINES. 
Assumes the array is big enough."
  (loop :for line :in lines :for r :from 0 :do
     (loop :for char :across line :for c :from 0 :do
	(setf (aref array c r) (char-code char))))
  array)

(defun convert-lines-to-array (lines)
  "Convert a list of lines two a two dimensional array of characters."
  (let* ((width (reduce #'max lines :key #'length))
	 (height (length lines))
	 (result (make-array `(,width ,height) 
			     :element-type '(unsigned-byte 8) 
			     :initial-element 0)))
    (fill-array-with-lines result lines)))

(defun read-track (in-file-name)
  "Read the track stored in the file with name IN-FILE-NAME.
Returns the read track.

The track is an ASCII file depicting the track grid.
The following characters are special:
 ?\S -- the starting position.  There should be just one S specified.
 ?\F -- A finish line character.  There can be multiple Fs.  
        The finish line needs to be crossed upwards 
        If the path crosses an F upwards it is considered to be finished..
 ?\- -- A forbidden area.  This is used to depict the boundary
        of the track.

An example of a track is:

   ----------------------
   ----              ----
   ----  S -------   ----
   ----FFFF-------   ----
   ----    -------   ----
   ----              ----
   ----------------------

the rule on the finishing line means that you can not immediately
go down from S over F, but have to go around before crossing F to finish
the path."
  (with-open-file (s in-file-name)
    (let ((lines
	   (loop :for line = (read-line s nil nil)
	      :while line :collect line)))
      (convert-lines-to-array lines))))


(defun print-track (array &optional (stream t))
  "Print thet track ARRAY to STREAM.
Just prints the file being read in by `read-track'."
  (loop :for r :from 0 :below (array-dimension array 1) :do
     (loop :for c :from 0 :below (array-dimension array 0) :do
	(format stream "~C" (code-char (aref array c r))))
     (format stream "~&")))

(defparameter +START+ (char-code #\S))
(defparameter +PIVOT+ (char-code #\P))

(defun find-special-place (array thing)
  "Find in the track ARRAY the THING. 
Return the location as a cons cell."
  (loop :for c :from 0 :below (array-dimension array 0) :do
     (loop :for r :from 0 :below (array-dimension array 1) :do
	(when (eql thing (aref array c r))
	  (return-from find-special-place (cons c r))))))


(defstruct point-in-path
  "Contains a point in a path.  Keeps track of position, speed and the previous
position on the path."
  col row col-speed row-speed (prev-point nil))

(defun unique-location (position)
  "Returns an identifier given POSITION (which should be a `point-in-path'.
The identifier identifies the position in the search state space for finding
paths on the track.  This is different from position itself because position
keeps track of the previous positions and for the state space we only need
location on the track and speed.
"
  (list 
   (point-in-path-col position)
   (point-in-path-row position)
   (point-in-path-row-speed position)
   (point-in-path-col-speed position)))

(defun point-in-path-prev-col (position)
  (- (point-in-path-col position) (point-in-path-col-speed position)))

(defun point-in-path-prev-row (position)
  (- (point-in-path-row position) (point-in-path-row-speed position)))

(defun position-in-range (position track)
  (let ((row (point-in-path-row position))
	(col (point-in-path-col position)))
    (and (>= row 0)
	 (>= col 0)
	 (< row (array-dimension track 1))
	 (< col (array-dimension track 0)))))

(defun new-position (position ddx ddy)
  (let ((dx (+ (point-in-path-col-speed position) ddx))
	(dy (+ (point-in-path-row-speed position) ddy)))
    (make-point-in-path 
     :col (+ (point-in-path-col position) dx)
     :row (+ (point-in-path-row position) dy)
     :col-speed dx
     :row-speed dy
     :prev-point position)))

(defun print-position (position &optional (stream t))
  (format stream "[~D ~D]" (point-in-path-col position) 
	  (point-in-path-row position)))

(defun find-shortest-path-classical (track)
  (let* ((queue (make-priority-queue))
	 (start-pos (find-special-place track +START+))
	 (start-point (make-point-in-path 
					 :col (car start-pos)
					 :row (cdr start-pos)
					 :col-speed 0
					 :row-speed 0))
	 (seen (make-hash-table :test #'equalp)))

    (flet ((add-new-position (pos priority)
	     "Add position to queue to be handled.
But only if the POS is not already seen."
	     (let ((uniq (unique-location pos)))
	       (unless (gethash uniq seen)
		 (setf (gethash uniq seen) t)
		 (priority-queue-push pos priority queue))))
	   (candidate-positions (position)
	     "New list of potential positions, not checked for validity."
	     (loop :for ddx :from -1 upto 1 
		:append
		(loop :for ddy :from -1 upto 1
		   :collect (new-position position (- ddx) (- ddy)))))
	   (process-candidate (position)
	     "Returns T if position is valid.  
This means that the position is inside the track and the line
between the previous position and this position does not leave the track."
	     (when (position-in-range position track)
	       (labels ((print-winning-track ()
			  "Print to standard out the track with the shortest path.
This will also abort the search and return from the encompassing function."
			  (let ((copy-of-track (alexandria:copy-array track))
				(length 0))
			    (loop :for pos = position :then (point-in-path-prev-point pos)
			       :while (point-in-path-prev-point pos)
			       :do
			       (incf length)
			       (iterate-points-in-line (point-in-path-prev-col pos)
						       (point-in-path-prev-row pos)
						       (point-in-path-col pos)
						       (point-in-path-row pos)
						       (lambda (x y) 
							 (setf (aref copy-of-track x y) (char-code #\Space))))
			       (setf  (aref copy-of-track 
					    (point-in-path-col pos)
					    (point-in-path-row pos))
				      (char-code #\#)))
			    (print-track copy-of-track)
			    (format t "~%~% Length: ~D~&" length))
			  (return-from find-shortest-path-classical))
			(check-position (x y)
			  (case (aref track x y)
			    (#. (char-code #\-) nil)
			    (#. (char-code #\F)
				(when (< (point-in-path-row-speed position) 0)
				  ;; winning!!!
				  (print-winning-track)))
			    (t t))))
		 
		 (iterate-points-in-line (point-in-path-prev-col position)
					 (point-in-path-prev-row position)
					 (point-in-path-col position)
					 (point-in-path-row position)
					 #'check-position)))))
      
      
      (add-new-position start-point 0)
      
      (loop 
	 :until (priority-queue-empty-p queue)
	 :for (priority . position) = (priority-queue-pop-with-priority queue)
	 :do
	 (loop :for candidate :in (candidate-positions position) :do
	    (when (process-candidate candidate)
	      (add-new-position candidate (- priority 1))))))))





(defun iterate-points-in-line (x0 y0 x1 y1 function)
  "Call function FUNCTION for all points on line from (x0 y0) to (x1 y1).
If FUNCTION returns nil it will stop iterating an the return value is the 
return value of the last call to FUNCTION.

Note 1. FUNCTION takes two argumnets x and y.
Note 2. It is not guaranteed that FUNCTION is called in order for the points on the line.
        The only guarantee is that eventually all points on the line are fed to FUNCTION
        once.  (If FUNCTION never returns nil."
  (let ((dx (- x1 x0))
	(dy (- y1 y0)))
    (cond 
      ((and (>= dx 0)
	    (>= dy 0)
	    (>= dx dy))
       (iterate-points-in-line-octant-1 x0 y0 x1 y1 
					function))
      
      ((and (>= dx 0)
	    (>= dy 0)
	    (< dx dy))
       (iterate-points-in-line-octant-1 y0 x0 y1 x1 
					(lambda (x y) (funcall function y x))))
      
      ((and (>= dx 0)
	    (< dy 0)
	    (>= dx (- dy)))
       (iterate-points-in-line-octant-1 x0 (- y0) x1 (- y1) 
					(lambda (x y) (funcall function x (- y)))))
      
      ((and (>= dx 0)
	    (< dy 0)
	    (< dx (- dy))) 
       (iterate-points-in-line-octant-1 (- y0) x0 (- y1) x1
					(lambda (x y) (funcall function y (- x)))))

      ((and (< dx 0)
	     (>= dy 0)
	     (>= (- dx) dy))
       (iterate-points-in-line-octant-1 (- x0) y0 (- x1) y1
					(lambda (x y) (funcall function (- x) y))))
      
      ((and (< dx 0)
	 (>= dy 0)
	 (< (- dx) dy))
       (iterate-points-in-line-octant-1 y0 (- x0) y1 (- x1)
					(lambda (x y) (funcall function (- y) x))))
      
      ((and (< dx 0)
	 (< dy 0)
	 (>= (- dx) (- dy)))
       (iterate-points-in-line-octant-1 x1 y1 x0 y0 
					function))

      ((and (< dx 0)
	 (< dy 0)
	 (< (- dx) (- dy)))
       (iterate-points-in-line-octant-1 y1 x1 y0 x0 
					(lambda (x y) (funcall function y x)))))))


(defun iterate-points-in-line-octant-1 (x0 y0 x1 y1 function)
  "Call FUNCTION for each point in the line from (x0 y0) to (x1 y1).
Keep iterating while function returns non NIL.

This assumes that the line (x0 y0) -- (x1 y1) is in octant 1.
Which means x1 >= x0, y1 >= y0 and y1 - y0 <= x1 -x0."
  
  (loop 
     :with dx = (- x1 x0)
     :with dy = (- y1 y0)
     :with D = (- (* 2 dy) dx)
     :with D-pos-incr = (- (* 2 dy) (* 2 dx))
     :with D-neg-incr = (* 2 dy)
     :with x = x0
     :with y = y0
     :for result = (funcall function x y)
     :while result
     :while (< x x1)
     :finally (return result)
     :do
     (if (> D 0)
	 (progn 
	   (incf y)
	   (incf D D-pos-incr))
	 (progn
	   (incf D D-neg-incr)))
     (incf x)))


(defun test-draw (x0 y0 x1 y1)
  (let ((image (make-array '(20 20) :element-type '(unsigned-byte 8) :initial-element (char-code #\.))))
    (flet ((draw-point (x y &optional (val (char-code #\*)))
	     (unless (= (random 10) 3)
	       (setf (aref image x y) val))))
      (draw-point x0 y0 (char-code #\S))
      (draw-point x1 y1 (char-code #\E))
      (format t "Result: ~A~%" (iterate-points-in-line x0 y0 x1 y1 #'draw-point))
      (print-track image)
      image)))
