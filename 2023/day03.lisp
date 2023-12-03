#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(defun process-line (line)
  (let (storage)
    (defun process-char (char position)
      (cond
        ((eq char #\.)
         (prog1 (if storage `(,storage))
                 (setq storage nil)))
        ((digit-char-p char)
         (setq storage
               (if storage
                   `(,(+ (* (car storage) 10) (digit-char-p char))
                     ,(cadr storage) ,position)
                   `(,(digit-char-p char) ,position ,position)))
         nil)
        (t (prog1 (if storage
                       `((symbol ,position ,char) ,storage)
                       `((symbol ,position ,char)))
                   (setq storage nil)))))
    (loop for char in (concatenate 'list line ".")
          for position from 0
          nconc (process-char char position))))

(defvar *schematic* (make-hash-table :test 'equal))
(defvar *gears* (make-hash-table :test 'equal))
(defvar *numbers* '())

(loop for line = (read-line *standard-input* nil nil)
      while line
      for n from 0
      do (loop for (item pos extra) in (process-line line)
               do (if (eq item 'symbol)
                      (setf (gethash (cons n pos) *schematic*) extra)
                      (push `(,item ,n ,pos ,extra) *numbers*))))

(let (gx gy)
  (loop for (number line start end) in *numbers*
        for touch = (loop for x from (- line 1) to (+ line 1)
                            thereis (loop for y from (- start 1) to (+ end 1)
                                          do (setq gx x gy y)
                                            thereis (gethash (cons x y) *schematic*)))
        if touch
          sum number into total
        if (eq touch #\*)
          do (push number (gethash (cons gx gy) *gears*))
        finally (format t "~a~%" total)))

(loop for v being the hash-values in *gears*
      if (= 2 (list-length v))
        sum (apply #'* v) into total
      finally (format t "~a~%" total))
