#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :str :silent t)

(defun solve (numbers)
  (loop for ns = numbers then nns
        for nns = (map 'list #'- (cdr ns) ns)
        for s = 1 then (- s)
        sum (car (last ns)) into result1
        sum (* s (car ns)) into result2
        until (apply #'= ns)
        finally (return (cons result1 result2))))

(loop for line = (read-line *standard-input* nil nil)
      while line
      for numbers = (map 'list #'parse-integer (str:words line))
      for (r1 . r2) = (solve numbers)
      sum r1 into result1
      sum r2 into result2
      finally (format t "~a~%~a~%" result1 result2))
