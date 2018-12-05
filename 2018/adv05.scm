(load "common.scm")

(define *polymer* (string->list (read-line)))

(define (annihilate polymer)
  (length (fold (lambda (c state)
                   (if (and (not (null? state))
                            (not (char=? c (car state)))
                            (char-ci=? c (car state)))
                       (cdr state)
                       (cons c state))) '() polymer)))

(printf "Result: ~a\n" (annihilate *polymer*))

(define ((=ci? k) c) (char-ci=? k c))
(define (unique-ci list)
  (hash-table-keys (alist->hash-table
                    (map (lambda (c) (cons (char-downcase c) #f)) list))))

(printf "Result: ~a\n"
        (reduce min (length *polymer*)
                (map (lambda (c) (annihilate (remove (=ci? c) *polymer*)))
                     (unique-ci *polymer*))))
