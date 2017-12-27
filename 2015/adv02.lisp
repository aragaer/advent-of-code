(defun get-num (l+p)
  (multiple-value-bind (r p)
      (parse-integer (car l+p) :junk-allowed t :start (cdr l+p))
    (setf (cdr l+p) (+ 1 p))
    r))

(defun need-paper (x y z)
  (loop for side in (list (* x y) (* y z) (* z x))
	minimizing side into min-side
	summing (* 2 side) into area
	finally (return (+ area min-side))))

(defun need-ribbon (x y z)
  (let* ((m (max x y z))
	 (smallest-perimeter (+ x y z (- m))))
    (+ (* 2 smallest-perimeter) (* x y z))))

(loop for line = (read-line *standard-input* nil :eof)
      until (eq line :eof)
      for l+p = (cons line 0)
      for (x y z) = (loop repeat 3 collect (get-num l+p))
      summing (need-paper x y z) into paper
      summing (need-ribbon x y z) into ribbon
      finally (format t "Total paper needed ~a~%" paper)
      finally (format t "Total ribbon needed ~a~%" ribbon))
