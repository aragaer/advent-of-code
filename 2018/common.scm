(require-extension srfi-1)  ; circular-list
(require-extension srfi-13)  ; string->list
(require-extension srfi-69)  ; hash-table
(require-extension regex)

(use loop)
(use format)
(use list-comprehensions)
(use vector-lib)
(use miscmacros)
(use lazy-lists)

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

(define ((vector-inc-at! vector) index)
 (inc! (vector-ref vector index)))

(define ((hash-table-add-to! t) c value)
  (hash-table-update!/default t c (lambda (old) (+ old value)) 0) t)

(define ((hash-table-collect-to! t) c value)
  (hash-table-update!/default t c (lambda (list) (cons value list)) '()) t)

(define (vector-max vector)
  (vector-fold
   (lambda (i state l)
     (if (> l (cdr state))
         (cons i l)
         state)) (cons 0 0) vector))

(define ((match regex) line) (apply values (cdr (string-search regex line))))

(define (assoc/default key alist . default) (or (assoc key alist) (cons key (and default '()))))
(define (assoc-value/default . args) (cdr (apply assoc/default args)))

(define string->char (o car string->list))
(define ((p func) . args) (apply values (map func args)))
