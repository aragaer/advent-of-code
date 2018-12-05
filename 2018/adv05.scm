(load "common.scm")

(define *polymer* (string->list (read-line)))

(define (annihilate polymer)
  ((o sub1 length) (fold (lambda (c stack)
                           (if (and (char-ci=? c (car stack))
                                    (not (char=? c (car stack))))
                               (cdr stack) (cons c stack)))
                         '(\#space) polymer)))

(printf "Result1: ~a\n" (annihilate *polymer*))
(printf "Result2: ~a\n"
        (reduce min (length *polymer*)
                (map (lambda (c) (annihilate (delete c *polymer* char-ci=?)))
                     (delete-duplicates *polymer* char-ci=?))))
