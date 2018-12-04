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

(define-syntax inc!
  (syntax-rules ()
    ((inc! var)
     (set! var (+ var 1)))))

(define (count-into c t)
  (hash-table-update!/default t c add1 0) t)

(define ((vector-inc-at! vector) index)
 (inc! (vector-ref vector index)))

(define ((hash-table-add-to! t) c value)
  (hash-table-update!/default t c (lambda (old) (+ old value)) 0) t)

(define (vector-max vector)
  (vector-fold
   (lambda (i state l)
     (if (> l (cdr state))
         (cons i l)
         state)) (cons 0 0) vector))
