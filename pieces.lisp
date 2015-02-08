(in-package :puzzlesolver)

(defclass piece ()
  ((shape :initarg :shape :reader shape-of)
   (max-orientations :initarg :max-orientations :reader max-orientations-of)
   (in-use :initform nil :accessor in-use-of)
   (rotation :initform :north :reader rotation-of)
   (mirrored :initform nil :reader mirrored-p)
   (current-shape :reader current-shape-of)
   (current-orientation :initform 0 :accessor current-orientation-of)))

(defmethod initialize-instance :after ((piece piece) &rest args)
  (%compute-current-shape piece))

(defmethod print-object ((piece piece) stream)
  (if *print-readably*
      (call-next-method)
    (let ((*standard-output* stream))
      (output piece))))

(defmethod %compute-current-shape ((piece piece))
  (let* ((shape (shape-of piece))
         (dimensions (array-dimensions shape))
         (rotation (rotation-of piece))
         (mirrored (mirrored-p piece))
         (target-shape (make-array (case rotation
                                     ((:north :south)
                                      dimensions)
                                     (t
                                      (reverse dimensions))))))
    (case mirrored
      ((nil)
       (case rotation
         (:north
          (loop for r below (first dimensions)
                for tr = r
                do
                (loop for c below (second dimensions)
                      for tc = c
                      do
                      (setf (aref target-shape tr tc) (aref shape r c)))))
         (:south
          (loop for r below (first dimensions)
                for tr downfrom (1- (first dimensions)) 
                do
                (loop for c below (second dimensions)
                      for tc downfrom (1- (second dimensions))
                      do
                      (setf (aref target-shape tr tc) (aref shape r c)))))
         (:east
          (loop for r below (first dimensions)
                for tc downfrom (1- (first dimensions))
                do
                (loop for c below (second dimensions)
                      for tr = c
                      do
                      (setf (aref target-shape tr tc) (aref shape r c)))))
         (:west
          (loop for r below (first dimensions)
                for tc = r
                do
                (loop for c below (second dimensions)
                      for tr downfrom (1- (second dimensions))
                      do
                      (setf (aref target-shape tr tc) (aref shape r c)))))))
      (t
       (case rotation
         (:north
          (loop for r below (first dimensions)
                for tr downfrom (1- (first dimensions))
                do
                (loop for c below (second dimensions)
                      for tc = c
                      do
                      (setf (aref target-shape tr tc) (aref shape r c)))))
         (:south
          (loop for r below (first dimensions)
                for tr = r
                do
                (loop for c below (second dimensions)
                      for tc downfrom (1- (second dimensions))
                      do
                      (setf (aref target-shape tr tc) (aref shape r c)))))
         (:east
          (loop for r below (first dimensions)
                for tc = r
                do
                (loop for c below (second dimensions)
                      for tr = c
                      do
                      (setf (aref target-shape tr tc) (aref shape r c)))))
         (:west
          (loop for r below (first dimensions)
                for tc downfrom (1- (first dimensions)) 
                do
                (loop for c below (second dimensions)
                      for tr downfrom (1- (second dimensions))
                      do
                      (setf (aref target-shape tr tc) (aref shape r c))))))))
    (setf (slot-value piece 'current-shape) target-shape)
    piece))

(defmethod rotate ((piece piece) &key (direction (next-direction piece)) %skip-compute)
  (setf (slot-value piece 'rotation) direction)
  (unless %skip-compute
    (%compute-current-shape piece)))

(defmethod next-direction ((piece piece))
  (next-direction (rotation-of piece)))

(defmethod next-direction ((direction t))
  (case direction
    (:north :east)
    (:east :south)
    (:south :west)
    (t :north)))

(defmethod flip ((piece piece) &key %skip-compute)
  (setf (slot-value piece 'mirrored)
        (not (mirrored-p piece)))
  (unless %skip-compute
    (%compute-current-shape piece)))

(defmethod inc-orientation ((piece piece))
  (incf (current-orientation-of piece))
  (rotate piece :%skip-compute t)
  (when (= (current-orientation-of piece) 4)
    (flip piece :%skip-compute t))
  (%compute-current-shape piece))

(defmethod output ((piece piece))
  (let* ((shape (current-shape-of piece))
         (dimensions (array-dimensions shape)))
    (loop for r below (first dimensions)
          do 
          (terpri)
          (loop for c below (second dimensions)
                if (aref shape r c)
                do (princ (aref shape r c))
                else
                do (princ " "))))
  (values))

(defmethod reset-piece ((piece piece))
  (setf (slot-value piece 'mirrored) nil)
  (setf (in-use-of piece) nil)
  (setf (current-orientation-of piece) 0)
  (rotate piece :direction :north))

(defun reset-pieces (&optional (pieces *pieces*))
  (loop for piece in pieces
        do (reset-piece piece)))
