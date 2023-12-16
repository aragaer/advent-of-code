#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(defvar *data*
  (loop for line = (read-line *standard-input* nil nil)
        while line
        collect line into lines
        finally (return (make-array `(,(length lines) ,(length (car lines)))
                                    :initial-contents lines))))

(defvar *seen*)

(defun reset-state ()
  (setf *seen* (make-hash-table :test #'equal)))

(reset-state)

(defun mirror (direction mirror)
  (ecase mirror
    (#\\ (cons (cdr direction) (car direction)))
    (#\/ (cons (- (cdr direction)) (- (car direction))))))

(defun move (pos direction)
  (destructuring-bind (y . x) pos
    (cons (+ y (car direction)) (+ x (cdr direction)))))

(defun split (direction mirror pos)
  (or
   (ecase mirror
     (#\- (if (= 0 (cdr direction))
              (prog1 '(0 . 1)
                (energize (move pos '(0 . -1)) '(0 . -1)))))
     (#\| (if (= 0 (car direction))
              (prog1 '(1 . 0)
                (energize (move pos '(-1 . 0)) '(-1 . 0))))))
   direction))

(defun energize (pos direction)
  (loop while (array-in-bounds-p *data* (car pos) (cdr pos))
        for here = (aref *data* (car pos) (cdr pos))
        until (member direction (gethash pos *seen*) :test #'equal)
        do (push direction (gethash pos *seen*))
        do (ecase here
             (#\.) ;; do nothing
             ((#\\ #\/) (setf direction (mirror direction here)))
             ((#\| #\-) (setf direction (split direction here pos))))
        do (setf pos (move pos direction))))

(defun experiment (pos direction)
  (reset-state)
  (energize pos direction)
  (hash-table-count *seen*))

(format t "~a~%" (experiment '(0 . 0) '(0 . 1)))

(let ((starts (destructuring-bind (size-y size-x) (array-dimensions *data*)
                (append (loop for y from 0 to (- size-y 1)
                              collect `(,(cons y 0) (0 . 1))
                              collect `(,(cons y (- size-x 1)) (0 . -1)))
                        (loop for x from 0 to (- size-x 1)
                              collect `(,(cons 0 x) (1 . 0))
                              collect `(,(cons (- size-y 1) x) (-1 . 0)))))))
  (loop for (pos dir) in starts
        maximize (experiment pos dir) into result2
        finally (format t "~a~%" result2)))
