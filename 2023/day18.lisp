#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :str :silent t)

(defparameter *insns*
  (loop for line = (read-line *standard-input* nil nil)
        while line
        for (dir1 dist1 color) = (str:words line)
        for dist2 = (subseq color 2 7)
        for dir2 = (char "RDLU" (digit-char-p (char color 7)))
        collect (cons (char dir1 0) (parse-integer dist1)) into insns1
        collect (cons dir2 (parse-integer dist2 :radix 16)) into insns2
        finally (return (list insns1 insns2))))

(defun solve (insns)
  (let (prev-dir prev-height)
    (destructuring-bind (y pos neg)
        (loop for (dir . dist) in insns
              with y = 0
              with direction-at-deepest
              if (eq dir #\U)
                do (decf y dist)
              if (eq dir #\D)
                do (incf y dist)
              if (member dir '(#\R #\L))
                do (setf prev-dir dir
                         prev-height y)
              maximize y into max-dip
              if (= y max-dip)
                do (setf direction-at-deepest dir)
              finally (return (cons max-dip
                                    (if (eq direction-at-deepest #\L)
                                        '(#\R #\L) '(#\L #\R)))))
      (loop for (dir . dist) in insns
            with result = 0
            if (eq dir pos)
              do (incf result (* (1+ y) (1+ dist)))
            if (eq dir neg)
              do (incf result (* (- y) (1+ dist)))
            if (eq dir #\U)
              do (incf y dist)
            if (eq dir #\D)
              do (decf y dist)
            if (member dir '(#\R #\L))
              do (let ((between (1- (abs (- prev-height y))))
                       (below (1+ (min y prev-height))))
                   (when (eq dir prev-dir)
                     (setf result (funcall (if (eq dir pos) #'- #'+) result below)))
                   (when (eq (if (< y prev-height) prev-dir dir) neg)
                     (incf result between)))
              and do (setf prev-dir dir
                           prev-height y)
            finally (return result)))))

(format t "~{~a~%~}" (map 'list #'solve *insns*))
