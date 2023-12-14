#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :select :silent t)

(defvar *data*
  (loop for line = (read-line *standard-input* nil nil)
        while line
        collect line into lines
        finally (return (make-array `(,(length lines) ,(length (car lines)))
                                    :initial-contents lines))))

(defun format-data ()
  (coerce (make-array (list (array-total-size *data*)) :displaced-to *data*) 'string))

(defun roll (row)
  (let ((stop-at 0))
    (dotimes (x (length row))
      (case (aref row x)
        (#\O (setf (aref row x) #\.
                   (aref row stop-at) #\O
                   stop-at (1+ stop-at)))
        (#\# (setf stop-at (1+ x)))))
    row))
(defun rroll (row) (reverse (roll (reverse row))))

(defmacro tilt (direction)
  (destructuring-bind (dim func) (ecase direction
                                       (north '(1 roll))
                                       (west '(0 roll))
                                       (south '(1 rroll))
                                       (east '(0 rroll)))
    (let ((selector `(select:select *data* ,@(nth dim '((i t) (t i))))))
      `(dotimes (i (array-dimension *data* ,dim))
         (setf ,selector (,func ,selector))))))

(defun calc-result ()
  (let ((size-y (array-dimension *data* 1)))
    (loop for y from 0 to (- size-y 1)
          for value downfrom size-y
          sum (* value (count #\O (select:select *data* y t))))))

(tilt north)
(format t "~a~%" (calc-result))

(defun cycle ()
  (tilt north)
  (tilt west)
  (tilt south)
  (tilt east))

(defvar iters 1000000000)
(loop for state = (format-data)
      with seen = (make-hash-table :test #'equal)
      for c from 0 to iters
      for loop-start = (gethash state seen)
      until loop-start
      collect (calc-result) into states
      do (setf (gethash state seen) c)
      do (cycle)
      finally (let* ((loop-len (- c loop-start))
                     (iter (mod (- iters loop-start) loop-len)))
                (format t "~a~%" (nth (+ iter loop-start) states))))
