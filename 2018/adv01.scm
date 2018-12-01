(require-extension srfi-69)

(define data (list (read)))
(define tail data)

(do ((value (read) (read)))
    ((eof-object? value))
  (set-cdr! tail (list value))
  (set! tail (cdr tail)))

(printf "~a\n" (foldl + 0 data))

(set-cdr! tail data)

(define seen (make-hash-table))

(handle-exceptions exn
                   (printf "~a\n" exn)
                   (foldl
                    (lambda (freq value)
                      (hash-table-set! seen freq #t)
                      (set! freq (+ freq value))
                      (if (hash-table-exists? seen freq)
                          (abort freq)
                          freq))
                    0 data))
