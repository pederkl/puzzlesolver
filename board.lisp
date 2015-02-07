(in-package :puzzlesolver)

(defun print-board (&optional (board *board*))
  (let ((dimensions (array-dimensions board)))
    (terpri)
    (loop for i below (+ 2 (second dimensions)) do (princ #\.))
    (loop for r below (first dimensions)
          do 
          (terpri)
          (princ #\.)
          (loop for c below (second dimensions)
                if (aref board r c)
                do (princ (aref board r c))
                else
                do (princ " "))
          (princ #\.))
    (terpri)
    (loop for i below (+ 2 (second dimensions)) do (princ #\¨)))
  (values))

(defun place-piece (piece upper-left-position &key (board *board*) unset-p test-p print-p)
  (let* ((shape (current-shape-of piece))
         (piece-dimensions (array-dimensions shape))
         (board-dimensions (array-dimensions board))
         (unset-next-free-pos upper-left-position)
         (upper-left-position (copy-list upper-left-position))
         (next-free-pos))
    (loop for c from 0
          while (not (aref shape 0 c))
          finally (decf (second upper-left-position) c))
    (when (or (< (first board-dimensions)
                 (+ (first upper-left-position) (first piece-dimensions)))
              (< (second board-dimensions)
                 (+ (second upper-left-position) (second piece-dimensions)))
              (< (second upper-left-position) 0))
      (if test-p
          (return-from place-piece nil)
        (error "Piece would extend outside board.")))
    (loop for pr below (first piece-dimensions)
          for br = (+ (first upper-left-position) pr)
          do
          (loop for pc below (second piece-dimensions)
                for bc = (+ (second upper-left-position) pc)
                if (aref shape pr pc)
                do (cond
                    (test-p
                     (if (aref board br bc)
                         (return-from place-piece nil)))
                    (unset-p
                     (setf (aref board br bc) nil))
                    ((aref board br bc)
                     (error "Test first! Overlap, board may be inconsistent."))
                    (t
                     (setf (aref board br bc)
                           (aref shape pr pc))))
                if (and (not unset-p)
                        (not next-free-pos)
                        (= br (first upper-left-position))
                        (not (aref board br bc)))
                do (setf next-free-pos (list br bc))))
    (cond
     (unset-p
      (setq next-free-pos unset-next-free-pos))
     ((not next-free-pos)
      ;; This piece filled its entire top row.
      ;; Look for next free space in rest of this row or the rows below.
      (loop named next-free-search
            for r from (first upper-left-position) below (first board-dimensions)
            for c1 = (+ (second upper-left-position) (second piece-dimensions)) then 0
            do (loop for c from c1 below (second board-dimensions)
                     unless (aref board r c)
                     do 
                     (setq next-free-pos (list r c))
                     (return-from next-free-search)))))
    (cond (unset-p
           (setf (in-use-of piece) nil))
          ((not test-p)
           (setf (in-use-of piece) upper-left-position)))
    (when print-p
      (print-board board))
    (values (or next-free-pos :complete)
            upper-left-position)))

(defun place-one-piece (piece upper-left-position &key (board *board*) print-p)
  (loop for orientation from (current-orientation-of piece) below (max-orientations-of piece)
        do (setf (current-orientation-of piece) orientation)
        if (place-piece piece upper-left-position :board board :test-p t)
        do (return-from place-one-piece
             (place-piece piece upper-left-position :board board :print-p print-p))
        else
        do (progn
             (rotate piece)
             (when (= orientation 4)
               (flip piece)))))

(defun remove-piece (piece upper-left-position &key (board *board*) print-p)
  (place-piece piece upper-left-position :unset-p t :board board :print-p print-p))

(defun reset-board (&optional (board *board*))
  (let ((dimensions (array-dimensions board)))
    (loop for r below (first dimensions)
          do
          (loop for c below (second dimensions)
                do (setf (aref board r c) nil)))))

