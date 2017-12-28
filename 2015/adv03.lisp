(defmacro move (x y)
  `(ecase c
     (#\< (decf ,x))
     (#\> (incf ,x))
     (#\^ (incf ,y))
     (#\v (decf ,y))))

(defmacro visit (x y l)
  `(setf (gethash (cons ,x ,y) ,l) t))

(loop for c across (read-line *standard-input*)
      with visited1 = (make-hash-table :test 'equal)
      with visited2 = (make-hash-table :test 'equal)
      with x = 0 and y = 0
      with x2 = 0 and y2 = 0 and ox = 0 and oy = 0
      initially (visit 0 0 visited1) (visit 0 0 visited2)
      do (move x y) (visit x y visited1)
      do (rotatef x2 ox) (rotatef y2 oy)
      do (move x2 y2) (visit x2 y2 visited2)
      finally (format t "Part 1: ~d~%" (hash-table-count visited1))
      finally (format t "Part 2: ~d~%" (hash-table-count visited2)))
