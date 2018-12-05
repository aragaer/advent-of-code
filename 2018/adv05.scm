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

(printf "Result: ~a\n"
        (let ((all-chars (make-hash-table)))
          (for-each (o (add-to-set all-chars) char-downcase) *polymer*)
          (reduce min (length *polymer*)
                  (map (lambda (c) (annihilate (remove (=ci? c) *polymer*)))
                       (hash-table-keys all-chars)))))
