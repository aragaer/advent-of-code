#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :iterate :silent t)
(ql:quickload :split-sequence :silent t)

(defpackage adv06
  (:use :cl
	:iterate
	:split-sequence))

(in-package #:adv06)

(defun coord-pair (pair)
  (mapcar 'parse-integer (split-sequence #\, pair)))

(defun parse-line (line)
  (let ((parsed (split-sequence #\Space line)))
    (if (string= (first parsed) "turn")
      (setf parsed (rest parsed)))
    (list (first parsed)
	  (coord-pair (second parsed))
	  (coord-pair (fourth parsed)))))

(defun on-1 (l) (declare (ignore l)) 1)
(defun on-2 (l) (1+ l))
(defun off-1 (l) (declare (ignore l)) 0)
(defun off-2 (l) (max 0 (1- l)))
(defun toggle-1 (l) (- 1 l))
(defun toggle-2 (l) (+ l 2))

(defvar *grid* (make-array '(1000 1000) :initial-element '(0 0)))

(iter (for line = (read-line *standard-input* nil))
      (while line)
      (for (action (x1 y1) (x2 y2)) = (parse-line line))
      (for alter = (cond ((string= action "on") '(on-1 on-2))
			 ((string= action "off") '(off-1 off-2))
			 ((string= action "toggle") '(toggle-1 toggle-2))))
      (iter (for x from x1 to x2)
	    (iter (for y from y1 to y2)
		  (setf (aref *grid* x y) 
			(mapcar #'funcall alter (aref *grid* x y))))))

(iter (with lgrid = (make-array (array-total-size *grid*) :displaced-to *grid*))
      (for x from 0 below (array-total-size lgrid))
      (for (l1 l2) = (aref lgrid x))
      (sum l1 into result-1)
      (sum l2 into result-2)
      (finally (format t "Part 1: ~d~%Part 2: ~d~%" result-1 result-2)))
