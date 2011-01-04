;;;; An example of the use of plot.lisp

;;; NOTE: To use the code below, plot.lisp must already be loaded in your
;;; session

;; create a plot object
(defvar *plot* (make-plot))

;; set some parameters of our plot
(setf (plot-title *plot*) "This is the title"
      (plot-labels *plot*) '("X Axis" "Y Axis")
      (plot-scale *plot*) '((-4 4) (-4 4))
      (plot-key-location *plot*) "bmargin left"
      (plot-key-style *plot*) "horizontal box")

;; now plot some data
(add-data *plot* '2d #(-2 -1 0 1 2) #(-2 -1 0 1 2))

;; finally, below is the function to close the plot and end the gnuplot process
;; use it wisely
;;(destroy-plots *plot*)