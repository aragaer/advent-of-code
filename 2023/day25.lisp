#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :str :silent t)

(defparameter *output-graphviz-p* nil)

(when *output-graphviz-p*
  (format t "graph ~aday25~a {~%" #\" #\"))

(defvar *all-components* nil)
(defparameter *connections*
  (loop for line = (read-line *standard-input* nil nil)
        with conns = (make-hash-table)
        while line
        for (name1 . rest) = (str:words line)
        for name = (subseq name1 0 (1- (length name1)))
        for (n1 . ns) = (map 'list #'read-from-string (cons name rest))
        for current = (gethash n1 conns)
        do (setf (gethash n1 conns) (union current ns))
        do (loop for other in ns
                 do (pushnew n1 (gethash other conns)))
        do (setf *all-components* (union *all-components* (cons n1 ns)))
        if *output-graphviz-p*
          do (loop for other in ns
                   do (format t "  ~a -- ~a~%" n1 other))
        finally (return conns)))

(when *output-graphviz-p*
  (format t "}~%"))

(defparameter *tear-down*
  (if (= 15 (length *all-components*))
      '((hfx . pzl) (bvb . cmg) (nvd . jqt))
      '((sfm . vmt) (vph . mfc) (fql . rmg))))

(unless *output-graphviz-p*
  (loop for (n1 . n2) in *tear-down*
        do (setf (gethash n1 *connections*) (remove n2 (gethash n1 *connections*))
                 (gethash n2 *connections*) (remove n1 (gethash n2 *connections*))))
  (let ((components (copy-seq *all-components*))
        visited)
    (loop with queue = (subseq components 0 1)
          while queue
          for component = (pop queue)
          unless (member component visited)
            do (setf visited (cons component visited)
                     components (remove component components)
                     queue (union queue (gethash component *connections*))))
    (format t "~a~%" (* (length visited) (length components)))))
