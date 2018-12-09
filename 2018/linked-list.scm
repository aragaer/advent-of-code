(define (linked-list-insert-after! item new-item)
  (linked-list-link! new-item (linked-list-next item))
  (linked-list-link! item new-item))

(define (linked-list-remove! item)
  (let ((next (linked-list-next item))
        (prev (linked-list-prev item)))
    (linked-list-link! prev next)))

(define (linked-list-link! item1 item2)
  (set-car! (cdr item1) item2)
  (set-cdr! (cdr item2) item1))

(define linked-list-next cadr)
(define linked-list-prev cddr)
(define linked-list-value car)

(define (make-linked-list-item value)
  (list value '()))
