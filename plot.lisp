;;;; Plotting facilities for CL
;;;; Powered by gnuplot, inspired by Ryan Adams's cl-gnuplot

;;;; NOTE: Depends on SBCL
;;;; TODO: Make this more platform-independent

;;;; ** IDEA (TODO?): Implement an error handler to catch gnuplot errors and
;;;; ** translate them into CL errors; maybe also provide restarts?

;; The plot class and its constructor
;; -----------------------------------------------------------------------------
(defclass plot ()
  ((gnuplot-process :initform (sb-ext:run-program
			       "/usr/bin/gnuplot"
			       nil
			       :wait nil
			       :pty nil
			       :input :stream
			       :error :output
			       :output t))
   ; gnuplot-process input stream will be sent here
   (comm-stream :accessor comm-stream
		:initform nil)
   ; if redraw-after-every-update? evals to t, then changing any of the plot
   ; instance's properties will result in an immediate redrawing
   (redraw-after-every-update? :accessor redraw?
			       :initform t)
   ; the set of axes on the plot (i.e., (x y z) for Cartesian coords, (r theta
   ; phi) for spherical polar coords, etc.)
   ; Default is Cartesian
   (axes :initform '(x y z))
   ; plot title
   (title :accessor plot-title
	  :initform nil)
   ; list of labels on the axes; of form (x y z)
   (labels :accessor plot-labels
	   :initform '(nil nil nil))
   ; if t, the plot key is displayed
   (key-on :accessor plot-key-on
	   :initform t)
   ; location of the plot key; follows gnuplot conventions
   (key-location :accessor plot-key-location)
   ; style of the plot key
   (key-style :accessor plot-key-style)
   ; list of axis ranges, of form ((min max) ... )
   (scale :accessor plot-scale
	  :initform '())
   ; sampling info according to gnuplot conventions
   ; of form (samples isosamples)
   (samples :accessor plot-sampling-size
	    :initform '())
   ; a list of all the datasets to plot...
   (data :initform '())
   ; ...and a list of the filenames in which each dataset is found
   (data-files :initform '())))

(defun make-plot ()
  "The plot object constructor!"
  (let ((p (make-instance 'plot)))
    (setf (comm-stream p)
	  (sb-ext:process-input (slot-value p 'gnuplot-process)))
    p))

;; -----------------------------------------------------------------------------
;; Some utility functions and macros for use in the high-level methods
;; associated with plot
;; -----------------------------------------------------------------------------
(defun length>=1 (list)
  (and (listp list) (first list)))

(defun maybe-extend-labels (labels)
  (cond ((stringp labels) (list labels nil nil))
	((and (listp labels) (= (length labels) 2)) (append labels '(nil)))
	((and (listp labels) (= (length labels) 3)) labels)
	(t (error "Uh oh..."))))

; a macro we'll need to make writing the accessors easier
(defmacro with-redraw? (plot &body body)
  (let ((result (gensym)))
    `(if (and (redraw? ,plot) (length>=1 (slot-value ,plot 'data-files)))
	 (let ((,result (progn ,@body)))
	   (draw-plots ,plot)
	   ,result)
	 (progn ,@body))))

(defmacro with-comm-flush (plot &body body)
  `(progn ,@body
	  (force-output (comm-stream ,plot))))

(defun gnuplot-arg-string-format (plot-cmd? &rest args)
  (let ((control-string (if plot-cmd? "~{~A~^, ~}" "~{~A~^ ~}"))
	(type-corrected-args (mapcar #'(lambda (x)
					 (if (symbolp x) (string-downcase x) x))
				     args)))
    (format nil control-string type-corrected-args)))

; this is our low-level gnuplot interface
; end-users should not need to use it
(defmethod command-gnuplot ((plot plot) command arg)
  (let ((type-corrected-command (string-downcase command)))
    (format t "~A ~A~%" type-corrected-command arg)
    (format (comm-stream plot) "~A ~A~%" type-corrected-command arg)))

(defmethod draw-plots ((plot plot))
  (with-comm-flush plot
    (command-gnuplot
     plot
     'plot
     (apply #'gnuplot-arg-string-format
	    (cons 't (mapcar #'(lambda (x) (format nil "~S" x))
			     (slot-value plot 'data-files)))))))

(defmethod gnuplot-set ((plot plot) &rest args)
  (command-gnuplot plot 'set
		   (apply #'gnuplot-arg-string-format (cons 'nil args))))
;; -----------------------------------------------------------------------------
;; Below are the methods on plot that make up the basic gnuplot interface.
;; -----------------------------------------------------------------------------

(defmethod (setf redraw?) (r? (plot plot))
  (setf (slot-value plot 'redraw-after-every-update?) r?))

(defmethod (setf plot-title) (new-title (plot plot))
  (with-redraw? plot
    (gnuplot-set plot 'title (format nil "~S" new-title))
    (setf (slot-value plot 'title) new-title)))

(defmethod (setf plot-labels) (new-labels (plot plot))
  (with-redraw? plot
    (let ((maybe-extended-new-labels (maybe-extend-labels new-labels))
	  (label-names (mapcar #'(lambda (x)
				   (format nil "~Alabel" (string-downcase x)))
			       (slot-value plot 'axes))))
      (mapcar #'(lambda (label-name label)
		  (gnuplot-set plot label-name
			       (if (null label) "\"\""
				   (format nil "~S" label))))
	      label-names
	      maybe-extended-new-labels)
      (setf (slot-value plot 'labels) new-labels))))

(defmethod (setf plot-key-on) (key-set? (plot plot))
  (with-redraw? plot
    (gnuplot-set plot 'key (if key-set? "on" "off"))
    (setf (slot-value plot 'key-on) key-set?)))

(defmethod (setf plot-key-location) (location (plot plot))
  (with-redraw? plot
    (gnuplot-set plot 'key location)
    (setf (slot-value plot 'key-location) location)))

; style: horizontal, vertical, box, etc.
; style should be specified by a string
(defmethod (setf plot-key-style) (style (plot plot))
  (with-redraw? plot
    (gnuplot-set plot 'key (string-downcase style))
    (setf (slot-value plot 'key-style) style)))

(defmethod add-data ((plot plot) dim &rest data)
  (let ((tmp-file-name (format nil "/tmp/clplot-~A.dat"
			       (random (get-universal-time)))))
    (with-open-file (tmp-file tmp-file-name :direction :output)
      (flet ((write-out-2d-data (a b)
	       (loop for i across a and j across b
		  do (format tmp-file "~A ~A~%" i j)))
	     (write-out-3d-data (a b c)
	       (loop for i across a and j across b and k across c
		  do (format tmp-file "~A ~A ~A~%" i j k))))
	(case dim
	  (|2D| (write-out-2d-data (first data) (second data)))
	  (|3D| (write-out-3d-data (first data) (second data) (third data))))))
    (setf (slot-value plot 'data-files)
	  (append (slot-value plot 'data-files) (list tmp-file-name))))
  (draw-plots plot))

(defmethod (setf plot-scale) (scales (plot plot))
  (with-redraw? plot
    (let ((scale-names (loop for axis in (slot-value plot 'axes) collect
			    (format nil "~Arange" (string-downcase axis))))
	  (ranges (loop for s in scales collect
		       (apply #'(lambda (a b) (format nil "[~D:~D]" a b)) s))))
      (loop for s in scale-names and r in ranges do
	   (gnuplot-set plot s r)))
    (setf (slot-value plot 'scale) scales)))

(defmethod destroy-plots ((plot plot))
  "End communications between C.L. layer and gnuplot, then kill the gnuplot
  process"
  (close (comm-stream plot))
  (sb-ext:process-kill (slot-value plot 'gnuplot-process) '15))