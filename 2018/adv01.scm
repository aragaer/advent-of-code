(require-extension srfi-69)

(define data (list (read)))
(define tail data)

(define advance-tail
  (lambda (tail value)
    (set-cdr! tail (list value))
    (cdr tail)))

(do ((value (read) (read)))
    ((eof-object? value))
  (set! tail (advance-tail tail value)))

(printf "~a\n" (foldl + 0 data))

(set-cdr! tail data)

(define seen (alist->hash-table '((0 . #t))))
(define seen?
  (lambda (freq)
    (if (hash-table-exists? seen freq)
        (abort freq)
        (hash-table-set! seen freq #t))))

(define register
  (lambda (freq value)
    (seen? (+ freq value))
    (+ freq value)))

(handle-exceptions exn
                   (printf "~a\n" exn)
                   (foldl register 0 data))
