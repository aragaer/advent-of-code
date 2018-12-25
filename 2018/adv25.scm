(load "common.scm")

(define *all-stars*
  (map (lambda (line)
         (map string->number (string-split-fields "," line #:infix)))
       (read-lines)))

(define connected
  (let* ((dist (lambda args (reduce + 0 (map abs args))))
         (r 3)
         (wiggle-space (lambda args
                         (let ((wiggle (- r (apply dist args))))
                           (range (- wiggle) (+ 1 wiggle))))))
    (append-map (lambda (x)
                  (append-map (lambda (y)
                                (append-map (lambda (z)
                                              (map (lambda (t) (list x y z t))
                                                   (wiggle-space x y z)))
                                            (wiggle-space x y)))
                              (wiggle-space x)))
                (wiggle-space))))
(define (around star) (map (lambda (offt) (map + star offt)) connected))

(define starmap (alist->hash-table (map list *all-stars*)))
(define (take-from-map star)
  (and (hash-table-exists? starmap star)
       (hash-table-delete! starmap star)
       star))

(loop for stars = *all-stars* then (lset-difference equal? stars constellation)
      until (null? stars)
      for count from 0
      for constellation = (loop with Q = (list->queue (take stars 1))
                                initially (take-from-map (first stars))
                                until (queue-empty? Q)
                                for star = (queue-remove! Q)
                                do (queue-push-back-list! Q (filter-map take-from-map (around star)))
                                collect star)
      finally (print count))
