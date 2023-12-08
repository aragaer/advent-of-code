#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :str :silent t)

(defvar *nodes* (make-hash-table))
(defvar *insns*
  (let ((line (read-line)))
    (apply #'vector
           (loop for c across line
                 collect (if (eq #\L c) #'car #'cdr)))))
(defvar *linsns* (length *insns*))

(read-line)

(loop for line = (read-line *standard-input* nil nil)
      while line
      for (s _ d1_ d2_) = (str:words line)
      for (ns nd1 nd2) = (map 'list #'intern `(,s ,(subseq d1_ 1 4) ,(subseq d2_ 0 3)))
      do (setf (gethash ns *nodes*) (cons nd1 nd2)))

(defun move (step from)
  (funcall (aref *insns* (mod step *linsns*)) (gethash from *nodes*)))

(if (gethash 'aaa *nodes*)
    (loop for step from 0
          for node = 'aaa then new-node
          for new-node = (move step node)
          until (eq new-node 'zzz)
          finally (format t "~a~%" (+ 1 step)))
    (format t "No node AAA, skip part 1~%"))

(defun last-char (sym)
  (car (last (coerce (symbol-name sym) 'list))))

(defvar *seen* (make-hash-table))

(defun seen-p (start node step)
  (let ((pos (mod step *linsns*))
        (seen (or (gethash start *seen*)
                  (setf (gethash start *seen*)
                        (make-hash-table :test #'equal)))))
    (let* ((key (cons pos node))
           (value (gethash key seen)))
      (unless value
        (setf (gethash key seen) step))
      value)))

(defvar *loops* '())

(dolist (n (loop for node being the hash-keys of *nodes*
                 if (eq (last-char node) #\A)
                   collect node))
  (loop for step from 0
        for node = n then new-node
        for new-node = (move step node)
        for seen = (seen-p n new-node step)
        until seen
        finally
           (let ((loop-len (- step seen))
                 (finish (loop for es from step
                               for en = node then nen
                               for nen = (move es en)
                               if (eq #\Z (last-char en))
                                 do (return es))))
             (push `(:loop-len ,loop-len :loop-start ,seen :finish ,finish) *loops*))))

(defun inv (a b)
  "Get inverse of a mod b using extended euclid algorithm"
  (let ((r a) (nr b)
        (s 1) (ns 0)
        (p 0) (np 1))
    (loop until (= 0 nr)
          for q = (floor r nr)
          for n = `(,(- r (* nr q)) ,(- s (* ns q)) ,(- p (* np q)))
          do (setf r nr s ns p np)
          do (setf nr (car n) ns (cadr n) np (caddr n))
          finally (return (mod (+ b s) b)))))

(if (= 1 (length *loops*))
    (format t "Single loop, skip part 2~%")
  (multiple-value-bind (M G ms)
      (loop for i in *loops*
            collect (getf i :loop-len) into lengths
            maximize (getf i :loop-start) into max-start
            finally (return (let ((G (apply #'gcd lengths)))
                              (values
                               (apply #'* (map 'list (lambda (l) (/ l G)) lengths))
                               G max-start))))
    (loop for i in *loops*
          for al = (/ (getf i :loop-len) G)
          for mi = (/ M al)
          for mi1 = (inv mi al)
          for finish = (getf i :finish)
          for step-count = (floor finish G)
          collect (mod finish G) into pre-steps
          sum (* step-count mi mi1) into result
          finally (let ((iters (+ (mod (- result 1) M) 1))
                        (pre-step (car pre-steps)))
                    (unless (apply #'= pre-steps)
                      (format t "pre-steps are not equal: ~a~%" pre-steps))
                    (format t "~a~%" (+ (* iters G) pre-step))))))
