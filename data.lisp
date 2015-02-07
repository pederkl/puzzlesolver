(in-package :puzzlesolver)

(defparameter *board*
  #2A((nil nil nil nil nil nil nil nil)
      (nil nil nil nil nil nil nil nil)
      (nil nil nil nil nil nil nil nil)
      (nil nil nil nil nil nil nil nil)
      (nil nil nil nil nil nil nil nil)
      (nil nil nil nil nil nil nil nil)
      (nil nil nil nil nil nil nil nil)
      (nil nil nil nil nil nil nil nil)))

(defparameter *pieces*
  (list
   (make-instance 'piece 
                  :shape #2A((a nil nil nil)
                             (a  a   a   a))
                  :max-orientations 8)
   (make-instance 'piece
                  :shape #2A((b b b b b))
                  :max-orientations 2)
   (make-instance 'piece
                  :shape #2A(( c nil nil)
                             ( c  c  c)
                             (nil c nil))
                  :max-orientations 8)
   (make-instance 'piece
                  :shape #2A(( d  d  d)
                             (nil d nil)
                             (nil d nil))
                  :max-orientations 4)
   (make-instance 'piece
                  :shape #2A(( e nil nil)
                             ( e  e  nil)
                             (nil e  e))
                  :max-orientations 4)
   (make-instance 'piece
                  :shape #2A(( f  f  f  f)
                             (nil f nil nil))
                  :max-orientations 4)
   (make-instance 'piece
                  :shape #2A((g g g)
                             (g g nil))
                  :max-orientations 8)
   (make-instance 'piece
                  :shape #2A((  h h nil nil)
                             (nil h h h))
                  :max-orientations 8)
   (make-instance 'piece
                  :shape #2A(( i  i nil)
                             (nil i nil)
                             (nil i i))
                  :max-orientations 8)
   (make-instance 'piece
                  :shape #2A((nil nil j)
                             (nil nil j)
                             ( j   j  j))
                  :max-orientations 4)
   (make-instance 'piece
                  :shape #2A((nil k nil)
                             ( k  k  k)
                             (nil k nil))
                  :max-orientations 1)
   (make-instance 'piece
                  :shape #2A((l l)
                             (l l))
                  :max-orientations 1)
   (make-instance 'piece
                  :shape #2A((m nil m)
                             (m  m  m))
                  :max-orientations 4)))
