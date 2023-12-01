#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :str :silent t)

(defun r (line reverse-p)
  (if reverse-p
      (reverse line)
      line))

(defun get-digit (line substs &optional reverse-p)
  (loop for ptr = (r line reverse-p) then (subseq ptr 1)
        until (str:emptyp ptr)
        thereis (loop for (w . n) in substs
                      if (str:starts-with-p (r w reverse-p) ptr)
                        return n)))

(defun get-num (line substs)
  (let ((fst (get-digit line substs))
        (snd (get-digit line substs :reverse)))
    (+ (* 10 fst) snd)))

(defvar substs0
  (loop for x from 1 to 9
        collect (cons (format nil "~a" x) x)))

(let ((digit-words '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine")))
  (defvar substs1
    (loop for word in digit-words
          for x from 1
          collect (cons word x))))
(nconc substs1 substs0)

(loop for line = (read-line *standard-input* nil nil)
      while line
      sum (get-num line substs0) into total1
      sum (get-num line substs1) into total2
      finally (format t "~a~%~a~%" total1 total2))
