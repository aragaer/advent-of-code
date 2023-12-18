#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :str :silent t)

(defparameter *insns*
  (loop for line = (read-line *standard-input* nil nil)
        while line
        for (dir1 dist1 color) = (str:words line)
        for dist2 = (subseq color 2 7)
        for dir2 = (char "RDLU" (digit-char-p (char color 7)))
        collect (cons (char dir1 0) (parse-integer dist1)) into insns1
        collect (cons dir2 (parse-integer dist2 :radix 16)) into insns2
        finally (return (list insns1 insns2))))

(defun shoelace (points)
  (loop for (x1 . y1) in points
        for (x2 . y2) in (cdr points)
        sum (- (* x1 y2) (* x2 y1)) into result
        finally (destructuring-bind ((x1 . y1) (xn . yn))
                    (cons (car points) (last points))
                  (let ((add (- (* xn y1) (* x1 yn))))
                    (return (abs (/ (+ result add) 2)))))))

(defun pick (area perimeter)
  (+ area 1 (/ perimeter 2)))

(defun solve (insns)
  (destructuring-bind (perimeter . points)
      (loop for (dir . dist) in insns
            with x = 0
            with y = 0
            collect (cons x y) into points
            do (case dir
                 (#\R (incf x dist))
                 (#\D (decf y dist))
                 (#\L (decf x dist))
                 (#\U (incf y dist)))
            sum dist into perimeter
            finally (return (cons perimeter points)))
    (pick (shoelace points) perimeter)))

(format t "~{~a~%~}" (map 'list #'solve *insns*))
