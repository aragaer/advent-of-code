#!/usr/bin/env -S sbcl --noinform --script

(defconstant *data*
  (loop for line = (read-line *standard-input* nil nil)
        while line
        collect (map 'list #'digit-char-p line) into lines
        finally (return (make-array `(,(length lines) ,(length (car lines)))
                                    :initial-contents lines))))

(defconstant *final-position*
  (destructuring-bind (y x) (array-dimensions *data*)
    `(0 ,(1- x) ,(1- y) 0 0)))

(defconstant *turns*
  (loop for x in '(1 -1 #c(0 1) #c(0 -1))
        with res = (make-hash-table :size 4)
        do (setf (gethash x res) `(,(* x #c(0 1)) ,(* x #c(0 -1))))
        finally (return res)))

(defun solve (low high)
  (declare (optimize (speed 3)))
  (loop with result
        with results = (make-hash-table :test #'equal)
        for (heat-loss x y prev-dir prev-len) = *final-position* then (pop options)
        for dirs = `((,low . -1) (,low . #c(0 -1)))
          then (unless (and result (< result (+ heat-loss x y)))
                 (append (if (> high prev-len) `((1 . ,prev-dir)))
                         (map 'list
                              (lambda (turn) (cons low turn))
                              (gethash prev-dir *turns*))))
        for new = (loop for (dist . dir) in dirs
                        for nlen = (+ dist (if (= dir prev-dir) prev-len 0))
                        for nx = (+ x (* dist (realpart dir)))
                        for ny = (+ y (* dist (imagpart dir)))
                        for nh = (when (array-in-bounds-p *data* ny nx)
                                   (+
                                    (loop for i from 0 to (1- dist)
                                          for tx = x then (+ tx (realpart dir))
                                          for ty = y then (+ ty (imagpart dir))
                                          sum (aref *data* ty tx))
                                    heat-loss))
                        for key = (list nx ny dir nlen)
                        for best = (gethash key results (and result (+ result nx ny)))
                        if (and nh (or (null best) (< nh best)))
                          collect (cons nh key) into new
                          and do (setf (gethash key results) nh)
                        finally (return (sort new #'<
                                              :key (lambda (o)
                                                     (destructuring-bind (h x y . _) o
                                                       (+ h x y))))))
        for options = new then (nconc new options)
        if (and (= 0 x) (= 0 y) (or (not result) (< heat-loss result)))
          do (setf result heat-loss)
        while options
        finally (return result)))

(format t "~a~%~a~%" (solve 1 3) (solve 4 10))
