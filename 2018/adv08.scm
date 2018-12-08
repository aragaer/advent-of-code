(load "common.scm")

(define (read-node)
  (let* ((child-count (read))
         (meta-count (read))
         (children (loop for i from 1 to child-count
                         collect (read-node))))
    (cons children
          (loop for i from 1 to meta-count
                collect (read)))))

(define root (read-node))

(define (meta-sum node)
  (let ((children (first node))
        (meta (cdr node)))
    (+ (reduce + 0 meta)
       (reduce + 0 (map meta-sum children)))))

(define (value node)
  (let ((children (list->vector (first node)))
        (meta (cdr node)))
    (reduce + 0
            (map
             (if (vector-empty? children)
                 identity
                 (lambda (idx)
                   (if (or (= 0 idx)
                           (< (vector-length children) idx))
                       0
                       (value (vector-ref children (- idx 1))))))
             meta))))

(printf "~a\n~a\n" (meta-sum root) (value root))
