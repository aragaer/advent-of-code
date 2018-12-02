(require-extension srfi-69)  ; hash-table
(require-extension srfi-13)  ; string->list
(use loop)

(define ids '())

(define has-num?
  (lambda (num table)
    (foldl (lambda (f c) (or f (= num c))) #f (hash-table-values table))))

(define count-chars
  (lambda (word)
    (loop for char in (string->list word)
          with table = (make-hash-table)
          do (hash-table-set! table char (+ 1 (hash-table-ref/default table char 0)))
          finally (return table))))

(loop for line = (read-line)
      until (eof-object? line)
      for table = (count-chars line)
      collect line into lines
      count (has-num? 2 table) into twos
      count (has-num? 3 table) into threes
      finally (print (* twos threes))
      finally (set! ids lines))

(define common "")

(define levenstein
  (lambda (w1 w2)
    (loop for c1 in (string->list w1)
          for c2 in (string->list w2)
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
      finally (printf "~a ~a ~a ~a\n" word word2 lev common))
