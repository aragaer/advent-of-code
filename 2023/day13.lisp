#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(defun diff (x1 x2)
  (typecase x1
    (vector (apply #'+ (map 'list #'diff (coerce x1 'list) (coerce x2 'list))))
    (t (if (equal x1 x2) 0 1))))

(defun mirror (array n-smudges)
  (let ((ll (length array)))
    (loop for i from 1 to (- ll 1)
          if (= n-smudges
                (loop for j from 1 to (min i (- ll i))
                      sum (diff (aref array (- i j))
                                (aref array (+ i j -1)))))
            collect i)))

(defvar *data*
  (loop for line = (read-line *standard-input* nil nil)
        with lines
        for emptyp = (or (null line) (= 0 (length line)))
        if emptyp
          collect (coerce (reverse lines) 'vector)
          and do (setf lines '())
        unless emptyp
          do (push line lines)
        while line))

(defun transpose (array)
  (apply #'map 'vector #'vector (coerce array 'list)))

(defun calc (chunk smudges)
  (car (or (mirror (transpose chunk) smudges)
           (map 'list (lambda (x) (* x 100)) (mirror chunk smudges)))))

(loop for chunk in *data*
      sum (calc chunk 0) into res1
      sum (calc chunk 1) into res2
      finally (format t "~a~%~a~%" res1 res2))
