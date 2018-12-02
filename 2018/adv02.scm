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

(define ((collision table) item)
  (let ((result (hash-table-exists? table item)))
    (hash-table-set! table item #t)
    (and result item)))

(define ((replace-at string substring) index)
  (string-replace string substring index
                  (+ (string-length substring) index)))

(define (subwords word)
  (map (replace-at word " ") (range (string-length word))))

(let ((seen (make-hash-table)))
  (print (string-delete #\space (find (collision seen) (append-map subwords *all-words*)))))
