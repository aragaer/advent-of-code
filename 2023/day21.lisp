#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :alexandria :silent t)

(defvar *start*)
(defparameter *orig-map*
  (loop for line = (read-line *standard-input* nil nil)
        for y from 0
        while line
        collect (loop for c across line
                      for x from 0
                      if (eq c #\S)
                         do (setf c #\.
                                  *start* (cons x y))
                      collect c) into lines
        finally (return (make-array `(,(length lines) ,(length (car lines)))
                                    :initial-contents lines))))
(defparameter *size-x* (array-dimension *orig-map* 1))
(defparameter *size-y* (array-dimension *orig-map* 0))

(defparameter *step-count* (if (equal *start* '(5 . 5)) 6 64))

(defvar *map* (alexandria:copy-array *orig-map*))

(defun fill-map (map start &optional max-steps)
  (declare (optimize (speed 3)))
  (setf (aref map (car start) (cdr start)) 0)
  (loop for points = `(,(cons 0 start)) then (append points new-points)
        while points
        for (steps x . y) = (pop points)
        until (and max-steps (<= max-steps steps))
        for new-points = (loop for d in '((1 0) (-1 0) (0 1) (0 -1))
                               for (nx ny) = (map 'list #'+ `(,x ,y) d)
                               if (and (array-in-bounds-p map ny nx)
                                       (eq (aref map ny nx) #\.))
                                 do (setf (aref map ny nx) (1+ steps))
                                 and collect `(,(1+ steps) ,nx . ,ny))))

(defun count-reached (map steps)
  (loop for i from 0 to (1- (array-total-size map))
        for value = (row-major-aref map i)
        count (and (not (member value '(#\# #\.)))
                   (<= value steps)
                   (evenp (- steps value)))))

;; part 1
(fill-map *map* *start*)
(format t "~a~%" (count-reached *map* *step-count*))

(defparameter *to-fill-center*
  (loop for y from 0 to (1- *size-y*)
        maximize (loop for x from 0 to (1- *size-x*)
                       for v = (aref *map* y x)
                       unless (characterp v)
                         maximize v)))
(defparameter *part2-steps* 26501365)
(defparameter *reduced-steps*
  (min *part2-steps*
       (+ *to-fill-center*
          (mod (- *part2-steps* *to-fill-center*) (* 2 *size-x*)))))
(defparameter *starting-radius*
  (ceiling (- *reduced-steps* (floor *size-x* 2)) *size-x*))
(defparameter *interpolation-iters* 3)
(defparameter *ending-radius* (+ *starting-radius* (* 2 (1- *interpolation-iters*))))
(defparameter inflate-radius *ending-radius*)

(defvar *huge-map*
  (make-array (map 'list (lambda (s) (* s (1+ (* 2 inflate-radius))))
                   (array-dimensions *orig-map*))))

(dotimes (y (array-dimension *huge-map* 0))
  (dotimes (x (array-dimension *huge-map* 1))
    (setf (aref *huge-map* y x)
          (aref *orig-map* (mod y *size-y*) (mod x *size-x*)))))

(fill-map *huge-map*
          (cons (+ (car *start*) (* *size-x* inflate-radius))
                (+ (cdr *start*) (* *size-y* inflate-radius)))
          (+ *reduced-steps* (* *size-x* (1- *interpolation-iters*) 2)))

(defun solve (numbers)
  (loop for ns = numbers then nns
        for nns = (map 'list #'- (cdr ns) ns)
        collect (car ns) into coeffs
        until (apply #'= ns)
        finally (destructuring-bind (c2 c1 c0) coeffs
                  (let* ((a (/ c0 2))
                         (b (- c1 a))
                         (c c2))
                    (return `(,c ,b ,a))))))

(defun calc (x coeffs)
  (loop for mul = 1 then (* mul x)
        for coeff in coeffs
        sum (* mul coeff)))

;; part 2
(loop for i from 0 to (1- *interpolation-iters*)
      with mul = (* 2 *size-x*)
      for result = (count-reached *huge-map* (+ *reduced-steps* (* mul i)))
      collect result into intermediate
      finally (let ((need (/ (- *part2-steps* *reduced-steps*) mul))
                    (coeffs (solve intermediate)))
                (format t "~a~%"(calc need coeffs))))
