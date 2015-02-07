(in-package :puzzlesolver)

(defparameter *debug* t)

(defun reset (&key (board *board*) (pieces *pieces*))
  (when board
    (reset-board board))
  (when pieces
    (reset-pieces pieces)))

(defun find-piece (&optional avoid)
  (loop for p in *pieces*
        unless (or (in-use-of p) (member p avoid))
        return p))

(defun fill-board (&optional (board *board*) (start-pos (list 0 0)))
  (loop with tried-pieces = nil
        for piece = (find-piece tried-pieces)
        while piece
        do (progn
             (loop while (< (1+ (current-orientation-of piece)) (max-orientations-of piece))
                   do (multiple-value-bind (next-position placed-position)
                          (place-one-piece piece start-pos :board board :print-p *debug*)
                        (break)
                        (case next-position
                          ((:complete)
                           (format t "Solution found:~%")
                           (print-board board)
                           (remove-piece piece placed-position))
                          ((nil)
                           ;; Not placeable, try next orientation/piece
                           )
                          (t
                           ;; See if this leads to a full board
                           (fill-board board next-position)
                           (remove-piece piece placed-position)
                           ))))
             (push piece tried-pieces)
             (reset-piece piece))))
