#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :str :silent t)

(defun read-ints ()
  (map 'list #'parse-integer (cdr (str:words (read-line)))))

(defun solve (time distance)
  (let ((val (floor (- time (sqrt (- (* time time) (* distance 4.0)))) 2)))
    (- time 1 val val)))

(defun squash (numbers) (parse-integer (format nil "~{~a~}" numbers)))

(defvar *times* (read-ints))
(defvar *distances* (read-ints))

(format t "~a~%~a~%"
        (apply #'* (map 'list #'solve *times* *distances*))
        (solve (squash *times*) (squash *distances*)))
