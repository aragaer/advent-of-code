#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(defvar *data*
  (loop for line = (read-line *standard-input* nil nil)
        while line
        collect (map 'list #'digit-char-p line) into lines
        finally (return (make-array `(,(length lines) ,(length (car lines)))
                                    :initial-contents lines))))

(defvar *final-position*
  (destructuring-bind (y x) (array-dimensions *data*)
    `(,(1- x) ,(1- y) 0 0 0)))

(defun solve (ultra-p)
  (loop with result
        with results = (make-hash-table :test #'equal)
        with (low high) = (if ultra-p '(4 10) '(1 3))
        for (x y prev-dir prev-len heat-loss) = *final-position* then (pop options)
        for new-options = (loop for dir in '(1 -1 #c(0 1) #c(0 -1))
                                for nlen = (if (= dir prev-dir) (1+ prev-len) 1)
                                for nx = (+ x (realpart dir))
                                for ny = (+ y (imagpart dir))
                                for new-heat-loss = (+ heat-loss (aref *data* y x))
                                for key = (list nx ny dir nlen)
                                for best = (gethash key results
                                                    (and result (+ result nx ny)))
                                if (and (/= dir (- prev-dir))
                                        (>= high nlen)
                                        (or (= dir prev-dir)
                                            (= 0 prev-dir)
                                            (>= prev-len low))
                                        (array-in-bounds-p *data* ny nx)
                                        (or (null best) (< new-heat-loss best)))
                                  collect (list nx ny dir nlen new-heat-loss)
                                  and do (setf (gethash key results) new-heat-loss))
        for options = new-options then (nconc new-options options)
        if result
          do (setf options (delete-if (lambda (o)
                                        (destructuring-bind (x y _ _ h) o
                                          (> h (+ x y result))))
                                      options))
        if (and (= 0 x) (= 0 y))
          do (setf result (if result
                              (min result heat-loss)
                              heat-loss))
        while options
        finally (return result)))

(format t "~a~%~a~%" (solve nil) (solve t))
