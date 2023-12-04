#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :str :silent t)

(defun sum-lists (l1 l2)
  (loop for x = (pop l1)
        for y = (pop l2)
        while (or x y)
        collect (+ (or x 0) (or y 0))))

(loop for line = (read-line *standard-input* nil nil)
      while line
      with extras
      for count = (+ 1 (or (pop extras) 0))
      for (my wins) = (str:split " | " (cadr (str:split ": " line)))
      for won = (list-length
                 (intersection (str:words my) (str:words wins) :test #'string=))
      if (> won 0)
        sum (expt 2 (- won 1)) into res1
        and do (setq extras
                     (sum-lists extras (make-list won :initial-element count)))
      sum count into res2
      finally (format t "~a~%~a~%" res1 res2))
