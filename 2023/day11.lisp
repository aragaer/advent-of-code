#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(defvar *skipped-x* '())
(defvar *skipped-y* '())

(defvar *galaxies*
  (loop for line = (read-line *standard-input* nil nil)
        for y from 0
        while line
        for found = (loop for c across line
                          for x from 0
                          if (eq #\# c)
                            collect (cons x y))
        unless found
          do (push y *skipped-y*)
        append found))

(let ((xs (map 'list #'car *galaxies*)))
  (dotimes (x (apply #'max xs))
    (unless (find x xs)
      (push x *skipped-x*))))

(defun adjust (what skipped age)
  (+ what (* (- age 1) (count-if (lambda (sx) (< sx what)) skipped))))

(defun distance (g1 g2 age)
  (destructuring-bind ((x1 . y1) (x2 . y2)) `(,g1 ,g2)
    (let ((nx1 (adjust x1 *skipped-x* age))
          (nx2 (adjust x2 *skipped-x* age))
          (ny1 (adjust y1 *skipped-y* age))
          (ny2 (adjust y2 *skipped-y* age)))
      (+ (abs (- nx1 nx2))
         (abs (- ny1 ny2))))))

(loop for (g1 . rest) = *galaxies* then rest
      while rest
      with res1 = 0
      with res2 = 0
      do (dolist (g2 rest)
           (incf res1 (distance g1 g2 2))
           (incf res2 (distance g1 g2 1000000)))
      finally (format t "~a~%~a~%" res1 res2))
