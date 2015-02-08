(in-package :puzzlesolver)

(defparameter *debug* nil)
(defvar *placements* 0)
(defvar *start-time* 0)
(defvar *solutions* 0)

(defun reset (&key (board *board*) (pieces *pieces*))
  (when board
    (reset-board board))
  (when pieces
    (reset-pieces pieces))
  (setq *placements* 0))

(defun find-piece (&optional avoid)
  (loop for p in *pieces*
        unless (or (in-use-of p) (member p avoid))
        return p))

(defun fill-board (&optional (board *board*) (start-pos (list 0 0)))
  (loop with tried-pieces = nil
        for piece = (find-piece tried-pieces)
        while piece
        do (progn
             (loop while (< (current-orientation-of piece) (max-orientations-of piece))
                   do (multiple-value-bind (next-position placed-position)
                          (place-piece piece start-pos :board board :print-p *debug*)
                        (case next-position
                          ((:complete)
                           (incf *placements*)
                           (incf *solutions*)
                           (let ((duration (/ (- (get-internal-real-time) *start-time*)
                                              internal-time-units-per-second)))
                             (format t "~&Solution found after trying ~d placements in ~f seconds~%~
                                      (~f placements/sec):~%"
                                     *placements*
                                     duration
                                     (/ *placements* duration)))
                           (print-board board)
                           (remove-piece piece placed-position)
                           (inc-orientation piece))
                          ((nil)
                           ;; Not placeable, try next orientation/piece
                           )
                          (t 
                           (incf *placements*)
                           ;; Recurse to place next piece
                           (fill-board board next-position)
                           (remove-piece piece placed-position :print-p *debug*)
                           (inc-orientation piece)))))
             (push piece tried-pieces)
             (reset-piece piece))))

(defun solve-puzzle ()
  (reset)
  (let ((*start-time* (get-internal-real-time)))
    (fill-board)
    (let ((duration (/ (- (get-internal-real-time) *start-time*)
                       internal-time-units-per-second)))
      (format t "~&~d solutions in ~f seconds (~f solutions/sec)~%"
              *solutions* duration (/ *solutions* duration))
      (format t "~d placements tried (~f placements/second)"
              *placements* (/ *placements* duration)))))
