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

(defun fill-map (map start)
  (setf (aref map (car start) (cdr start)) 0)
  (loop for points = `(,(cons 0 start)) then (append points new-points)
        while points
        for (steps x . y) = (pop points)
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
                   (= 0 (mod (- steps value) 2)))))

;;; part1
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
  (let ((mul (max *size-x* *size-y*)))
    (min *part2-steps*
         (+ *to-fill-center*
            (mod (- *part2-steps* *to-fill-center*) mul)))))

(defparameter *starting-radius*
  (ceiling (- *reduced-steps* (floor *size-x* 2)) *size-x*))

(defparameter *ending-radius*
  (+ *starting-radius* (* 2 4)))

(defparameter inflate-radius *ending-radius*)

(defvar *huge-map*
  (make-array (map 'list (lambda (s) (* s (1+ (* 2 inflate-radius))))
                   (array-dimensions *orig-map*))))

(dotimes (y (array-dimension *huge-map* 0))
  (dotimes (x (array-dimension *huge-map* 1))
    (setf (aref *huge-map* y x)
          (aref *orig-map* (mod y *size-y*) (mod x *size-x*)))))

(defun print-map (map &optional steps range &key breaks)
  (let ((my-steps (or steps (array-total-size map)))
        (my-range (or range (cons (cons 0 (1- (array-dimension map 1)))
                                  (cons 0 (1- (array-dimension map 0)))))))
    (format t "x from ~a to ~a, y from ~a to ~a~%"
            (caar my-range) (cdar my-range) (cadr my-range) (cddr my-range))
    (format t "~{~{~a~}~%~}"
            (loop for y from (cadr my-range) to (cddr my-range)
                  if (and (cdr breaks) (= 0 (mod y (cdr breaks))))
                    collect nil
                  collect (loop for x from (caar my-range) to (cdar my-range)
                                for v = (aref map y x)
                                if (and (car breaks) (= 0 (mod x (car breaks))))
                                  collect #\Space
                                collect (cond
                                          ((characterp v) v)
                                          ((> v my-steps) #\.)
                                          ((oddp (- my-steps v)) #\_)
                                          (t #\v)))))))

(fill-map *huge-map* (cons (+ (car *start*) (* *size-x* inflate-radius))
                           (+ (cdr *start*) (* *size-y* inflate-radius))))

(defun solve (numbers &optional need-coeffs)
  (loop for ns = numbers then nns
        for pow from 0
        for nns = (map 'list #'- (cdr ns) ns)
        sum (car (last ns)) into result
        collect (car ns) into coeffs
        until (apply #'= ns)
        finally (return (if need-coeffs (reverse coeffs) result))))

(defun calc (x coeffs)
  (loop for mul = 1 then (* mul x)
        for coeff in coeffs
        sum (* mul coeff)))

(defparameter *iter-start* 2)

(loop for i from *starting-radius* to *ending-radius*
      with prediction
      with mul = (* 2 *size-x*)
      for result = (count-reached *huge-map*
                                  (+ (mod *part2-steps* mul)
                                     (* mul i)))
      collect result into intermediate
      until (equal prediction result)
      do (setf prediction (solve intermediate))
      finally (let ((need (floor *part2-steps* mul)))
                (destructuring-bind (c0 c1 c2) (solve intermediate t)
                  (let* ((a (/ c0 2))
                         (b (- c1 a))
                         (c c2))
                    (format t "~a~%"(calc (- need *starting-radius*) `(,c ,b ,a)))))))
