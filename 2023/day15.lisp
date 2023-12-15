#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :str :silent t)

(defvar *boxes* (make-array '(256) :initial-element nil))

(defun hash-step (a b) (mod (* 17 (+ a b)) 256))
(defun hash (s) (reduce #'hash-step s :key #'char-code :initial-value 0))

(defun remove-lens (label _)
  (let ((box (hash label)))
    (setf (aref *boxes* box)
          (delete label (aref *boxes* box) :test #'string= :key #'car))))

(defun add-lens (label focal)
  (let* ((box (hash label))
         (existing (find label (aref *boxes* box) :test #'string= :key #'car)))
    (if existing
        (rplacd existing focal)
        (push (cons label focal) (aref *boxes* box)))))

(defun focusing-power ()
  (loop for b across *boxes*
        for bn from 1
        sum (loop for (_ . f) in (reverse b)
                  for slot from 1
                  sum (* slot (parse-integer f) bn))))

(loop for s in (str:split #\, (read-line))
      sum (hash s) into res1
      do (if (find #\= s)
             (apply #'add-lens (str:split #\= s))
             (apply #'remove-lens (str:split #\- s)))
      finally (format t "~a~%~a~%" res1 (focusing-power)))
