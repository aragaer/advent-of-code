#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :str :silent t)

(defvar *cache* (make-hash-table :test #'equal))

(defun process (line)
  (destructuring-bind (data springs) (str:words line)
    `(,data ,(map 'list #'parse-integer (str:split #\, springs)))))

(defun solve-start-here (data springs)
  (destructuring-bind (spring . rest) springs
    (cond
      ((find #\. (subseq data 0 spring)) 0)
      ((and (> (length data) spring) (eq (char data spring) #\#)) 0)
      (rest (solve (subseq data (+ 1 spring)) rest))
      (t (solve (subseq data spring) '())))))

(defun solve (data springs)
  (or (gethash (cons data springs) *cache*)
      (setf (gethash (cons data springs) *cache*) (do-solve data springs))))

(defun do-solve (data springs)
  (setf data (str:trim-left data :char-bag "."))
  (cond
    ((< (length data) (apply #'+ -1 (length springs) springs)) 0)
    ((null springs) (if (find #\# data) 0 1))
    ((eq (char data 0) #\#) (solve-start-here data springs))
    ;; (eq (char data 0) #\?)
    (t (+ (solve-start-here data springs) (solve (subseq data 1) springs)))))

(defun solve2 (data springs)
  (loop for x from 1 to 5
        collect data into datas
        append springs into multi-springs
        finally (return (solve (str:join #\? datas) multi-springs))))

(loop for line = (read-line *standard-input* nil nil)
      while line
      for (data springs) = (process line)
      sum (solve data springs) into res1
      sum (solve2 data springs) into res2
      finally (format t "~a~%~a~%" res1 res2))
