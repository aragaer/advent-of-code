#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(defvar *data*
  (loop for line = (read-line *standard-input* nil nil)
        while line
        collect line into lines
        finally (return (make-array `(,(length (car lines)) ,(length lines))
                                    :initial-contents lines))))

(defvar *exits*)
(defvar *seen*)

(defun reset-state ()
  (setf *seen* (make-hash-table)
        *exits* nil))

(reset-state)

(defun mirror1 (dir) (complex (imagpart dir) (realpart dir)))
(defun mirror2 (dir) (mirror1 (- dir)))
(defun move (pos dir) (+ pos dir))

(defun split (direction mirror pos)
  (or
   (ecase mirror
     (#\- (when (complexp direction)
            (energize (move pos -1) -1)
            1))
     (#\| (unless (complexp direction)
            (energize (move pos #c(0 -1)) #c(0 -1))
            #c(0 1))))
   direction))

(defvar *cache* (make-hash-table :test #'equal))

(defun energize (pos direction)
  (loop for x = (realpart pos)
        for y = (imagpart pos)
        while (array-in-bounds-p *data* y x)
        until (member direction (gethash pos *seen*))
        for here = (aref *data* y x)
        do (push direction (gethash pos *seen*))
        do (ecase here
             (#\.) ;; do nothing
             (#\\ (setf direction (mirror1 direction)))
             (#\/ (setf direction (mirror2 direction)))
             ((#\| #\-) (setf direction (split direction here pos))))
        do (setf pos (move pos direction))
        finally (unless (array-in-bounds-p *data* y x)
                  (push (cons (move pos (- direction)) (- direction)) *exits*))))

(defun experiment (pos direction)
  (or (gethash (cons pos direction) *cache*)
      (progn
        (reset-state)
        (energize pos direction)
        (let ((result (hash-table-count *seen*)))
          (dolist (key *exits* result)
            (setf (gethash key *cache*) result))))))

(format t "~a~%" (experiment 0 1))

(let ((starts (destructuring-bind (size-x size-y) (array-dimensions *data*)
                (append (loop for y from 0 to (- size-y 1)
                              collect `(,(complex 0 y) 1)
                              collect `(,(complex (- size-x 1) y) -1))
                        (loop for x from 0 to (- size-x 1)
                              collect `(,x #c(0 1))
                              collect `(,(complex x (- size-y 1)) #c(0 -1)))))))
  (loop for (pos dir) in starts
        maximize (experiment pos dir) into result2
        finally (format t "~a~%" result2)))
