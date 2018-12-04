(require-extension srfi-1)  ; circular-list
(require-extension srfi-13)  ; string->list
(require-extension srfi-69)  ; hash-table
(require-extension regex)

(use loop)
(use format)
(use list-comprehensions)
(use vector-lib)

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

(define (sum-into c t value)
  (hash-table-update!/default t c (lambda (old) (+ old value)) 0) t)

(define (vector-max vector)
  (vector-fold
   (lambda (i state l)
     (if (> l (cdr state))
         (cons i l)
         state)) (cons 0 0) vector))
