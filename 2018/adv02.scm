(load "common.scm")

(define (count-into c t)
  (hash-table-update!/default t c add1 0) t)

(define *all-words*
  (loop for line = (read-line)
        until (eof-object? line)
        for word = (string->list line)
        for table = (hash-table->alist (fold count-into (make-hash-table) word))
        collect line
        count (rassoc 2 table) into twos
        count (rassoc 3 table) into threes
        finally (print (* twos threes))))

(define seen (make-hash-table))
(define ((collision table) item)
  (let ((result (hash-table-exists? table item)))
    (hash-table-set! table item #t)
    (and result item)))

(define (subwords word)
  (loop for c in (string->list word)
        for x from 0
        collect (string-replace word " " x (+ 1 x))))

(print (string-delete #\space (find (collision seen) (append-map subwords *all-words*))))
