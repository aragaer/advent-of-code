(load "common.scm")

(define *depth* 3879)
(define *target-x* 8)
(define *target-y* 713)

(define *test* #f)
(define *debug* #f)
(define *verbose* (and *test* #f))

(when *test*
  (set! *depth* 510)
  (set! *target-x* 10)
  (set! *target-y* 10))

(define *geo-indices* (make-hash-table))

(define (get-erosion-level-at coord)
  (modulo (+ (hash-table-ref *geo-indices* coord) *depth*) 20183))

(hash-table-set! *geo-indices* 0 0)
(for-each (lambda (x)
            (hash-table-set! *geo-indices* x (* x 16807)))
          (range 1 1000))

(for-each (lambda (y)
            (hash-table-set! *geo-indices* (* y +i) (* y 48271)))
          (range 1 1000))

(for-each (lambda (xy)
            (let ((coord (apply make-rectangular xy)))
              (hash-table-set! *geo-indices* coord
                               (modulo (* (get-erosion-level-at (- coord 1))
                                          (get-erosion-level-at (- coord +i))) 20183))))
          (cartesian-product (range 1 1000) (range 1 1000)))

(hash-table-set! *geo-indices* (make-rectangular *target-x* *target-y*) 0)

(define *types* (make-hash-table))

(for-each (lambda (xy)
            (let ((coord (apply make-rectangular xy)))
              (hash-table-set! *types* coord
                               (modulo (get-erosion-level-at coord) 3))))
          (cartesian-product (range 0 1000) (range 0 1000)))

(if *verbose*
    (for-each (lambda (y)
                (for-each (lambda (x)
                            (let ((coord (make-rectangular x y)))
                              (display (nth '(#\. #\= #\|) (hash-table-ref *types* coord)))))
                          (range 0 (+ 1 *target-x*)))
                (newline))
              (range 0 (+ 1 *target-y*))))

(print (reduce + 0
               (map (lambda (xy)
                      (let ((coord (apply make-rectangular xy)))
                        (hash-table-ref *types* coord)))
                    (cartesian-product (range 0 (+ 1 *target-x*)) (range 0 (+ 1 *target-y*))))))

(define *minutes-to-reach* (make-hash-table #:test equal?))

(hash-table-set! *minutes-to-reach* (cons 0 'torch) 0)

(define (get-all-reachable minutes)
  (filter-map (lambda (kv)
                (and (= (cdr kv) minutes) (car kv)))
              (hash-table->alist *minutes-to-reach*)))

(define *tools* '(torch climbing-gear neither))

(define (is-valid-state? coord tool)
  (and (<= 0 (real-part coord)) (<= 0 (imag-part coord))
       (case (hash-table-ref *types* coord)
         ((0) (member tool '(torch climbing-gear)))
         ((1) (member tool '(neither climbing-gear)))
         ((2) (member tool '(torch neither))))))

(define (maybe-set coord tool minutes)
  (if (is-valid-state? coord tool)
      (hash-table-update! *minutes-to-reach* (cons coord tool)
                          (lambda (value)
                            (min value minutes))
                          (constantly minutes))))

(define (print-state-time state . minutes)
  (print "state " state " takes " (if (null? minutes)
                                      (hash-table-ref *minutes-to-reach* state)
                                      (car minutes)) " minutes to reach"))

(loop for minutes from 0
      for reachable = (get-all-reachable minutes)
      until (>= minutes (hash-table-ref/default *minutes-to-reach*
                                                (cons (make-rectangular *target-x* *target-y*) 'torch)
                                                10000))
      do (for-each (lambda (state)
                     (let ((coord (car state))
                           (tool (cdr state)))
                       (for-each (lambda (new-tool)
                                   (maybe-set coord new-tool (+ 7 minutes)))
                                 *tools*)
                       (for-each (lambda (change)
                                   (maybe-set (+ coord change) tool (+ 1 minutes)))
                                 '(1 -1 -i +i))))
                   reachable)
      if *debug* do (print minutes " " (length reachable))
      if *verbose* do (for-each print-state-time reachable))

(if *verbose*
    (hash-table-walk *minutes-to-reach*
                     print-state-time))

(print (hash-table-ref *minutes-to-reach* (cons (make-rectangular *target-x* *target-y*) 'torch)))
