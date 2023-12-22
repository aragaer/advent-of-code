#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :str :silent t)

(defvar *map*)
(defvar *bricks*
  (loop for line = (read-line *standard-input* nil nil)
        while line
        for i from 1
        for (from to) = (map 'list (lambda (p)
                                     (map 'list #'parse-integer
                                          (str:split #\, p)))
                             (str:split #\~ line))
        maximize (car from) into x
        maximize (car to) into x
        maximize (cadr from) into y
        maximize (cadr to) into y
        maximize (caddr from) into z
        maximize (caddr to) into z
        collect (cons from to)
        finally (setf *map* (make-array `(,(1+ x) ,(1+ y) ,(1+ z))
                                        :initial-element 0))))

(loop for ((x1 y1 z1) . (x2 y2 z2)) in *bricks*
      for i from 1
      do (loop for x from x1 to x2
               do (loop for y from y1 to y2
                        do (loop for z from z1 to z2
                                 do (setf (aref *map* x y z) i)))))

(defun supported-by (brick)
  (destructuring-bind ((x1 y1 z1) . (x2 y2 z2)) brick
    (declare (ignore z2))
    (let (bricks-below)
      (if (= 0 z1)
          (setf bricks-below '(ground))
          (loop for x from x1 to x2
                do (loop for y from y1 to y2
                         for brick-below = (aref *map* x y (1- z1))
                         if (< 0 brick-below)
                           do (pushnew brick-below bricks-below))))
      bricks-below)))

(defun move-down (brick idx)
  (destructuring-bind ((x1 y1 z1) . (x2 y2 z2)) brick
    (if (= z1 z2)
        (loop for x from x1 to x2
              do (loop for y from y1 to y2
                       do (setf (aref *map* x y z1) 0
                                (aref *map* x y (1- z1)) idx)))
        (setf (aref *map* x1 y1 z2) 0
              (aref *map* x1 y1 (1- z1)) idx))
    (setf (nth 2 (car brick)) (1- z1)
          (nth 2 (cdr brick)) (1- z2))))

(loop while (loop for brick in *bricks*
                  for i from 1
                  unless (supported-by brick)
                    collect i
                    and do (move-down brick i)))

(defvar *supports* (make-hash-table))
(loop for brick in *bricks*
      for i from 1
      do (setf (gethash i *supports*) (supported-by brick)))

(defun count-falling (desintegrated)
  (let ((all-bricks (loop for brick in *bricks*
                          for i from 1
                          unless (= i desintegrated)
                            collect i))
        (falling `(,desintegrated)))
    (loop while (loop for i in (set-difference all-bricks falling)
                      if (subsetp (gethash i *supports*) falling)
                        collect i
                        and do (push i falling)))
    (1- (length falling))))

(loop for brick in *bricks*
      for i from 1
      with essential
      for supports = (gethash i *supports*)
      if (= 1 (length supports))
        do (pushnew (car supports) essential)
      finally (format t "~a~%~a~%"
                      (- (length *bricks*) (length essential) -1)
                      (loop for i in (remove 'ground essential)
                            sum (count-falling i))))
