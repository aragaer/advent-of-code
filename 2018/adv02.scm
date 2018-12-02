(require-extension srfi-13)  ; string->list
(require-extension srfi-69)  ; hash-table
(use loop)

(define ids '())

(define count-chars
  (lambda (word)
    (let* ((table (make-hash-table))
           (count (lambda (c) (hash-table-update!/default table c add1 0))))
      (for-each count (string->list word))
      (hash-table->alist table))))

(loop for line = (read-line)
      until (eof-object? line)
      for table = (count-chars line)
      collect (string->list line) into lines
      count (rassoc 2 table) into twos
      count (rassoc 3 table) into threes
      finally (print (* twos threes))
      finally (set! ids lines))

(define common "")

(define levenstein
  (lambda (w1 w2)
    (loop for c1 in w1
          for c2 in w2
          when (equal? c1 c2)
            collect c1 into same
          else
            count #t
          finally (set! common (if same (list->string same) "")))))

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
      finally (printf "~a ~a ~a ~a\n"
                      (list->string word)
                      (list->string word2)
                      lev common))
