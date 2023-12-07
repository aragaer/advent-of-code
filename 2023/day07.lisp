#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(defvar *ranker* (make-hash-table))
(defparameter *jocker-wildcard* 1000)
(defun make-rank (lst)
  (loop for item in (coerce lst 'list)
        for n from 0
        do (setf (gethash item *ranker*) n)))
(make-rank "AKQJT98765432")
(make-rank '(five-of-a-kind
             four-of-a-kind
             full-house
             three-of-a-kind
             two-pair
             one-pair
             high-card))
(defun wildcards-p ()
  (= (gethash #\J *ranker*) *jocker-wildcard*))
(defun enable-wildcards ()
  (setf (gethash #\J *ranker*) *jocker-wildcard*))

(defun apply-wildcards (counts buckets)
  (let ((jockers (gethash #\J counts)))
    (when (and (wildcards-p) jockers (> 5 jockers))
      (setf (gethash jockers buckets) (remove #\J (gethash jockers buckets)))
      (let* ((best (loop for key being the hash-keys of buckets
                         if (gethash key buckets)
                           maximize key))
             (card (pop (gethash best buckets))))
        (push card (gethash (+ best jockers) buckets))))
    buckets))

(defun eval-hand (hand)
  (let* ((counts (loop for c across hand
                       with counts = (make-hash-table)
                       do (incf (gethash c counts 0))
                       finally (return counts)))
         (buckets (loop for card being the hash-keys in counts using (hash-value count)
                   with buckets = (make-hash-table)
                   do (push card (gethash count buckets nil))
                   finally (return (apply-wildcards counts buckets))))
         (best (loop for key being the hash-keys of buckets
                     if (gethash key buckets) maximize key))
         (pairs (gethash 2 buckets)))
    (cons
     (case best
       (5 'five-of-a-kind)
       (4 'four-of-a-kind)
       (3 (if pairs 'full-house 'three-of-a-kind))
       (2 (if (= 2 (list-length pairs)) 'two-pair 'one-pair))
       (t 'high-card))
     (coerce hand 'list))))

(defun compare-hands (hand1 hand2)
  (loop for c1 in hand1
        for c2 in hand2
        for v1 = (gethash c1 *ranker*)
        for v2 = (gethash c2 *ranker*)
        do (cond
             ((< v1 v2) (return nil))
             ((> v1 v2) (return t)))))

(defvar *players*
  (loop for line = (read-line *standard-input* nil nil)
        while line
        for draw = (subseq line 0 5)
        for bid = (parse-integer (subseq line 6))
        collect (cons bid draw)))

(dotimes (_ 2)
  (loop for (bid) in (sort
                       (map 'list
                            (lambda (player)
                              (cons (car player)
                                    (eval-hand (cdr player))))
                            *players*)
                       #'compare-hands :key #'cdr)
        for rank from 1
        sum (* rank bid) into result
        finally (format t "~a~%" result))
  (enable-wildcards))
