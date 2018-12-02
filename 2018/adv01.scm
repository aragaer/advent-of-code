(load "common.scm")

(define data
  (loop for value = (read)
        until (eof-object? value)
        collect value
        sum value into result
        finally (print result)))

(loop for value in (apply circular-list data)
      with seen = (alist->hash-table '((0 . #t)))
      sum value into freq
      until (hash-table-exists? seen freq)
      do (hash-table-set! seen freq #t)
      finally (print freq))
