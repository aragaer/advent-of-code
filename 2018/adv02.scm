(require-extension srfi-13)  ; string->list
(require-extension srfi-69)  ; hash-table
(use loop)
(use format)

(define-inline (count c t)
  (hash-table-update!/default t c add1 0) t)

(define ids
  (loop for line = (read-line)
        until (eof-object? line)
        for word = (string->list line)
        for table = (hash-table->alist (fold count (make-hash-table) word))
        collect word
        count (rassoc 2 table) into twos
        count (rassoc 3 table) into threes
        finally (print (* twos threes))))

(define common "")

(define levenstein
  (lambda (w1 w2)
    (loop for c1 in w1
          for c2 in w2
          when (equal? c1 c2) collect c1 into same
          else count #t
          finally (set! common (or same '())))))

(loop for word in ids
      for position from 1
      for res = (loop for word2 in (drop ids position)
                      for lev = (levenstein word word2)
                      until (= 1 lev)
                      finally (return (cons word2 lev)))
      while res
      for word2 = (car res)
      for lev = (cdr res)
      until (= 1 lev)
      finally (format #t "~{~c~} ~{~c~} ~a ~{~c~}~%"
                      word word2 lev common))
