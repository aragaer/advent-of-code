#!/usr/bin/env -S sbcl --noinform --script

(defparameter *map*
  (loop for line = (read-line *standard-input* nil nil)
        while line
        collect line into lines
        finally (return (make-array `(,(length lines) ,(length (car lines)))
                                    :initial-contents lines))))

(defparameter *size-x* (array-dimension *map* 1))
(defparameter *size-y* (array-dimension *map* 0))


(defun point->idx (y x) (+ (* y *size-y*) x))
(defun idx->point (idx) (cons (floor idx *size-y*) (mod idx *size-y*)))

(defparameter *start*
  (loop for x from 1 to (- *size-x* 2)
        if (eq #\. (aref *map* 0 x))
          do (return (point->idx 0 x))))

(defparameter *end*
  (loop for x from 1 to (- *size-x* 2)
        with y = (1- *size-y*)
        if (eq #\. (aref *map* y x))
          do (return (point->idx y x))))

(defun char->dirs (char)
  (case char
    (#\. '((0 1) (0 -1) (1 0) (-1 0)))
    (#\v '((1 0)))
    (#\^ '((-1 0)))
    (#\< '((0 -1)))
    (#\> '((0 1)))))

(defun make-graph (ignore-slopes-p)
  (loop for y from 0 to (- *size-y* 1)
        with graph = (make-hash-table)
        with slopes = nil
        do (loop for x from 1 to (- *size-x* 2)
                 for point = `(,y ,x)
                 for idx = (point->idx y x)
                 for here = (aref *map* y x)
                 if (and ignore-slopes-p (not (eq here #\#)))
                   do (setf here #\.)
                 if (member here '(#\> #\^ #\v #\<))
                   do (push idx slopes)
                 do (loop for d in (char->dirs here)
                          for np = (map 'list #'+ point d)
                          for ni = (apply #'point->idx np)
                          if (apply #'array-in-bounds-p *map* np)
                            unless (eq #\# (apply #'aref *map* np))
                              do (push (cons ni 1) (gethash idx graph))))
        finally (remhash *end* graph)
        finally (loop for slope in slopes
                      for (fall-to . _) = (gethash slope graph)
                      do (delete slope (gethash fall-to graph) :key #'car))
        finally (return (values graph slopes))))

(defparameter *graph* nil)
(defparameter *slopes* nil)

(defun simplify (graph)
  "Remove dead-ends and squash pass-throughs"
  (loop for deleted = nil
        do (loop for point being the hash-keys of graph using (hash-value neighs)
                 for is-slope-p = (member point *slopes*)
                 if (and is-slope-p (not neighs))
                   do (loop for neigh being the hash-keys of graph using (hash-value nn)
                            for points-here = (assoc point nn)
                            if points-here
                              do (delete point (gethash neigh graph) :key #'car))
                   and do (remhash point graph)
                   and do (push point deleted)
                 if (and (not is-slope-p)
                         (= (length neighs) 1)
                         (not (member point `(,*start* ,*end*))))
                   do (delete point (gethash (caar neighs) graph) :key #'car)
                   and do (remhash point graph)
                   and do (push point deleted)
                 if (= (length neighs) 2)
                   do (destructuring-bind ((pt1 . d1) (pt2 . d2)) neighs
                        (let ((n1 (gethash pt1 graph))
                              (n2 (gethash pt2 graph)))
                          (when (assoc point n1)
                            (setf (gethash pt1 graph)
                                  (cons (cons pt2 (+ d1 d2))
                                        (remove point n1 :key #'car))))
                          (when (assoc point n2)
                            (setf (gethash pt2 graph)
                                  (cons (cons pt1 (+ d1 d2))
                                        (remove point n2 :key #'car))))))
                   and do (remhash point graph)
                   and do (push point deleted))
        while deleted))

(defun max-path (visited dont-fix-dead-ends)
  (declare (optimize (speed 3)))
  (loop for (next . d) in (gethash (car visited) *graph*)
        for m = 0
        unless (member next visited)
          do (let ((result (max-path (cons next visited) dont-fix-dead-ends)))
               (setf m (if (and (= 0 result) (/= next *end*))
                           (if dont-fix-dead-ends d -1)
                           (+ d result))))
          and maximize m))

(defun solve (&optional ignore-slopes dont-fix-dead-ends)
  (multiple-value-bind (g s) (make-graph ignore-slopes)
    (setf *graph* g
          *slopes* s))
  (simplify *graph*)
  (max-path `(,*start*) dont-fix-dead-ends))

(format t "~a~%~a~%" (solve) (solve 'ignore-slopes))
