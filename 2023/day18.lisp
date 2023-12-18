#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :str :silent t)

(defvar *map* (make-hash-table))

(setf (gethash 0 *map*) t)

(defun dir->dir (c)
  (ecase (char c 0)
    (#\U #c(0 -1))
    (#\D #c(0 1))
    (#\R 1)
    (#\L -1)))

(loop for line = (read-line *standard-input* nil nil)
      with c = 0
      while line
      for (s-dir dist color) = (str:words line)
      for dir = (dir->dir s-dir)
      do (loop for i from 1 to (parse-integer dist)
               do (setf c (+ c dir)
                        (gethash c *map*) #\#)))

(defvar *min-x*)
(defvar *max-x*)
(defvar *min-y*)
(defvar *max-y*)

(loop for k being the hash-keys of *map*
      minimize (realpart k) into minx
      maximize (realpart k) into maxx
      minimize (imagpart k) into miny
      maximize (imagpart k) into maxy
      finally (setf *min-x* minx
                    *min-y* miny
                    *max-x* maxx
                    *max-y* maxy))

(defun print-map ()
  (loop for y from *min-y* to *max-y*
        do (format t "~{~a~}~%"
                   (loop for x from *min-x* to *max-x*
                         collect (gethash (complex x y) *map* #\.)))))

;(print-map)

;(format t "~a to ~a, ~a to ~a~%" *min-x* *max-x* *min-y* *max-y*)

(defun try-paint (x y)
  (loop for point = (complex x y) then (pop points)
        with visited = (list (complex x y))
        for new = (loop for d in '(1 -1 #c(0 1) #c(0 -1))
                        unless (gethash (+ point d) *map*)
                          collect (+ point d))
        for points = new then (if new (union new points) points)
        unless (reduce (lambda (r p)
                         (and r
                              (<= *min-x* (realpart p) *max-x*)
                              (<= *min-y* (imagpart p) *max-y*)))
                       new
                       :initial-value t)
          do (dolist (p visited)
               (remhash p *map*))
          and do (return nil)
        do (setf (gethash point *map*) #\v
                 visited (append new visited))
        while points
        finally (return t)))

(loop with sx = (floor (+ *min-x* *max-x*) 2)
      for y from *min-y* to *max-y*
      unless (gethash (complex sx y) *map*)
        do (if (try-paint sx y)
               (return)))

;(print-map)

(format t "~a~%" (hash-table-count *map*))
