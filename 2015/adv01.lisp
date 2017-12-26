(loop for c across (read-line *standard-input*)
      counting (= floor -1) into basement-visits
      counting (= basement-visits 0) into position
      summing (if (equal c #\() 1 -1) into floor
      finally (format t "Floor ~a~%Visited basement on step ~a~%" floor position))
