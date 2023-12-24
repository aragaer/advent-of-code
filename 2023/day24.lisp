#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :str :silent t)

(defparameter *hail*
  (loop for line = (read-line *standard-input* nil nil)
        while line
        for i from 1
        for (start speed) = (map 'list (lambda (p)
                                         (map 'list #'parse-integer
                                              (str:split #\, p)))
                                 (str:split #\@ line))
        collect (cons start speed)))

(defparameter *test-area*
  (if (= 5 (length *hail*))
      '(7 . 27)
      '(200000000000000 . 400000000000000)))

(defun v+ (l1 l2) (map 'list #'+ l1 l2))
(defun v- (l1 l2) (map 'list #'- l1 l2))
(defun v* (l m) (map 'list (lambda (v) (* v m)) l))
(defun v. (l1 l2) (apply #'+ (map 'list #'* l1 l2)))
(defun vc (l1 l2)
  (destructuring-bind ((a1 a2 a3) . (b1 b2 b3)) (cons l1 l2)
      (list (- (* a2 b3) (* a3 b2))
            (- (* a3 b1) (* a1 b3))
            (- (* a1 b2) (* a2 b1)))))

(defun line-intersection (h1 h2)
  (destructuring-bind ((p1 . d1) . (p2 . d2)) (cons h1 h2)
    (let* ((n (vc d1 d2))
           (n1 (vc d1 n))
           (n2 (vc d2 n))
           (denom (v. d1 n2)))
      (when (/= 0 denom)
        (v+ p1 (v* d1 (/ (v. (v- p2 p1) n2) denom)))))))

(loop for (((x1 y1 z1) . (vx1 vy1 vz1)) . rest) = *hail* then rest
      while rest
      with (t1 . t2) = *test-area*
      for x2 = (+ x1 vx1)
      for y2 = (+ y1 vy1)
      sum (loop for ((x3 y3 z3) . (vx2 vy2 vz2)) in rest
                for x4 = (+ x3 vx2)
                for y4 = (+ y3 vy2)
                for nom = (- (* (- x1 x3) (- y1 y2)) (* (- y1 y3) (- x1 x2)))
                for denom = (- (* vx1 vy2) (* vy1 vx2))
                if (/= 0 denom)
                  count (let* ((u (/ nom denom))
                               (ix (+ x3 (* u vx2)))
                               (iy (+ y3 (* u vy2)))
                               (intersect-p (and (<= 0 u) (<= t1 ix t2) (<= t1 iy t2)
                                                 (<= 0 (* vx1 (- ix x1))))))
                          intersect-p))
        into res1
      finally (format t "~a~%" res1))

(defun collect-speeds (diff speed)
  (loop for v from -1000 to 1000
        if (and (/= v speed) (= 0 (mod diff (- v speed))))
          collect v))

;; assume that for every axis there are enough hails having same speed in that axis
;; this limits possibilities for stone speed on that axis
;; assume that there is just one possibility for each axis
(when (< 5 (length *hail*))
  (defvar *speeds* '((x) (y) (z)))
  (loop for ((p1 . v1) . rest) = *hail* then rest
        while rest
        do (loop for (p2 . v2) in rest
                 do (loop for (a x1 x2 s1 s2) in (map 'list #'list '(x y z) p1 p2 v1 v2)
                          if (and (= s1 s2) (< 100 (abs s1)))
                            do (let ((existing (assoc a *speeds*))
                                     (new (collect-speeds (abs (- x1 x2)) s1)))
                                 (rplacd existing
                                         (if (cdr existing)
                                             (intersection (cdr existing) new)
                                             new))))))
  ;; once speed is known adjust all hails by that speed
  ;; new reference -- stone is not moving, all hails go to that point
  (let* ((stone-speed (map 'list #'cadr *speeds*))
         (adj-hail (loop for (p . v) in (subseq *hail* 0 2)
                         collect (cons p (v- v stone-speed))))
         (mid-point (line-intersection (car adj-hail) (cadr adj-hail))))
    (format t "~a~%" (apply #'+ mid-point))))
