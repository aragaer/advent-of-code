(load "common.scm")

(define (count-into c t)
  (hash-table-update!/default t c add1 0) t)

(define *all-words*
  (loop for line = (read-line)
        until (eof-object? line)
        for word = (string->list line)
        for table = (hash-table->alist (fold count-into (make-hash-table) word))
        collect word
        count (rassoc 2 table) into twos
        count (rassoc 3 table) into threes
        finally (print (* twos threes))))

(define (levenstein w1 w2)
  (let ((same (map eq? w1 w2)))
    (define (pass a b) (and a b))
    (values (count not same) (filter-map pass same w1))))

(define ((close-to w1) w2)
  (let-values (((distance similar) (levenstein w1 w2)))
    (and (= 1 distance) (print (list->string similar)))))

(loop for it = *all-words* then other-words
      for word = (first it)
      for other-words = (list-tail it 1)
      until (any (close-to word) other-words)
      until (null? other-words))
