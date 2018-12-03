(load "common.scm")

(define fabric (make-hash-table))
(define claimed (make-hash-table))
(define valid (make-hash-table))

(define regex (regexp "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)"))

(define (match-claim line)
  (apply values (map string->number (cdr (string-match regex line)))))

(loop for line in (read-lines)
      for claim-is-valid = #t
      do (let-values (((n x y w h) (match-claim line)))
           (hash-table-set! valid n #t)
           (loop for coord in (cartesian-product (range x (+ x w)) (range y (+ y h)))
                 do (count-into coord fabric)
                 do (hash-table-update!/default claimed coord
                                                (lambda (old)
                                                  (when old
                                                    (hash-table-set! valid old #f)
                                                    (set! claim-is-valid #f))
                                                  n) #f)
                 finally (hash-table-set! valid n claim-is-valid))))

(print (hash-table-fold fabric (lambda (k v total) (+ total (if (< 1 v) 1 0))) 0))
(hash-table-for-each valid (lambda (k v)
                             (if v (print k))))
