#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :str :silent t)

(defun get-digit (line substs)
  (loop for ptr = line then (subseq ptr 1)
        until (str:emptyp ptr)
        for res = (loop for (w . n) in substs
                        if (str:starts-with-p w ptr)
                          return n)
        if res
          return res))

(defvar substs0
  (loop for x from 1 to 9
        collect (cons (format nil "~a" x) x)))

(let ((digit-words '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine")))
  (defvar substs1
    (nconc
     (loop for word in digit-words
           for x = 1 then (+ x 1)
           collect (cons word x))
     substs0))
  (defvar substs2
    (nconc
     (loop for word in digit-words
           for x = 1 then (+ x 1)
           collect (cons (reverse word) x))
     substs0)))

(defun get-num (line substs1 substs2)
  (let ((fst (get-digit line substs1))
        (snd (get-digit (reverse line) substs2)))
    (+ (* 10 fst) snd)))

(defvar *lines*
  (loop for line = (read-line *standard-input* nil nil)
        while line
        collect line))

(loop for line in *lines*
      sum (get-num line substs0 substs0) into total1
      sum (get-num line substs1 substs2) into total2
      finally (format t "~a~%~a~%" total1 total2))
