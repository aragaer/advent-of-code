#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :str :silent t)

(defvar *seed-data*
  (map 'list #'parse-integer
       (cdr (str:words (read-line *standard-input*)))))
(read-line *standard-input*)
(read-line *standard-input*)

(defvar *maps*
  (loop for line = (read-line *standard-input* nil nil)
        with result = '(())
        while line
        do (let ((words (str:words line)))
             (case (list-length words)
               (3 (push (map 'list #'parse-integer words) (car result)))
               (0 (push nil result))))
        finally (return (nreverse result))))

(defun transform (intervals map)
  (let ((all-points (sort
                     (remove-duplicates
                      (loop for (v l) in (append intervals (map 'list #'cdr map))
                            collect v
                            collect (+ v l)))
                      #'<)))
    (loop for (p1 p2) in (map 'list #'list all-points (cdr all-points))
          for change = (loop for (d s l) in map
                             if (<= s p1 p2 (+ s l))
                               do (return (- d s)))
          if (loop for (v l) in intervals thereis (<= v p1 p2 (+ v l)))
            collect `(,(+ p1 (or change 0)) ,(- p2 p1)))))

(defun solve (intervals)
  (apply #'min (map 'list #'car (reduce #'transform *maps* :initial-value intervals))))

(let ((singles (map 'list (lambda (s) `(,s 1)) *seed-data*))
      (intervals (loop for (s l) on *seed-data* by #'cddr
                       collect `(,s ,l))))
  (format t "~a~%~a~%" (solve singles) (solve intervals)))
