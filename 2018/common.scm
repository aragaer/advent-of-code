(require-extension srfi-1)  ; circular-list
(require-extension srfi-13)  ; string->list
(require-extension srfi-69)  ; hash-table
(require-extension regex)

(use loop)
(use format)
(use list-comprehensions)

(define (cartesian-product . lists)
  (fold-right (lambda (xs ys)
                (append-map (lambda (x)
                              (map (lambda (y)
                                     (cons x y))
                                   ys))
                            xs))
              '(())
              lists))

(define (count-into c t)
  (hash-table-update!/default t c add1 0) t)
