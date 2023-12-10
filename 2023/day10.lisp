#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :str :silent t)

(defvar *nodes* (make-hash-table :test #'equal))
(defvar *conns* '((#\| :n :s)
                  (#\- :w :e)
                  (#\L :n :e)
                  (#\J :n :w)
                  (#\7 :w :s)
                  (#\F :e :s)
                  (#\.)
                  (#\S :n :s :e :w)))
(defvar *states* (make-hash-table :test #'equal))
(defvar *start*)

(defun move (location direction)
  (destructuring-bind (y . x) location
    (ecase direction
      (:n (cons (- y 1) x))
      (:e (cons y (+ x 1)))
      (:w (cons y (- x 1)))
      (:s (cons (+ y 1) x)))))

(defun flip (direction)
  (ecase direction
    (:n :s)
    (:e :w)
    (:s :n)
    (:w :e)
    (:inside :outside)
    (:outside :inside)))

(defvar *size-x*)
(defvar *size-y*)

(loop for line = (read-line *standard-input* nil nil)
      for y from 0
      while line
      do (setf *size-x* (length line))
      do (loop for c across line
               for x from 0
               for key = (cons y x)
               do (setf (gethash key *nodes*) (cdr (assoc c *conns*)))
               if (eq c #\S)
                 do (setf *start* key))
      finally (setf *size-y* y))

(setf (gethash *start* *nodes*)
      (remove-if-not
       (lambda (direction)
         (let ((node (gethash (move *start* direction) *nodes*))
               (need (flip direction)))
           (member need node)))
       '(:n :s :e :w)))

(loop for steps from 1
      for location = *start* then new-location
      for come-from = (flip (car (gethash *start* *nodes*))) then (flip go-to)
      for go-to = (find-if-not (lambda (x) (eq x come-from))
                               (gethash location *nodes*))
      for new-location = (move location go-to)
      do (setf (gethash new-location *states*) :loop)
      until (equal new-location *start*)
      finally (format t "~a~%" (/ steps 2)))

(defun touches (directions)
  (or (subsetp '(:n :w) directions)
      (subsetp '(:s :e) directions)))

(dotimes (d (+ *size-x* *size-y*))
  (let ((state :outside))
    (dotimes (y (+ d 1))
      (let ((pos (cons y (- d y))))
        (if (eq :loop (gethash pos *states*))
            (unless (touches (gethash pos *nodes*))
              (setf state (flip state)))
            (setf (gethash pos *states*) state))))))

(loop for v being the hash-values of *states*
      count (eq v :inside) into result
      finally (format t "~a~%" result))
