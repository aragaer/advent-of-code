(load "common.scm")

(define rules (make-vector 32 #f))

(define (char->bool char) (eq? char #\#))
(define (bool->char bool) (if bool #\# #\.))

(define pots (map char->bool ((o cddr (drop-until #\:) string->list) (read-line))))

(read-line)

(define (window->num state)
  (apply + (filter-map fand state '(1 2 4 8 16))))

(for-each
 (o
  (lambda (line)
    (vector-set! rules (window->num (take line 5)) (last line)))
  (m char->bool)
  string->list)
 (read-lines))

(define (print-pots)
  (format #t "~{~a~}~%" (map bool->char (append '(#f) pots))))

(print-pots)

(loop with leftmost = 0
      for generation from 1 to 1000
      do (let-values (((head rest) (span not (sliding-map
                                              (lambda state
                                                (vector-ref rules (window->num state)))
                                              (append '(#f #f #f) pots '(#f #f #f))
                                              5))))
           (set! pots rest)
           (inc! leftmost (- (length head) 1)))
      do (when (= 0 (modulo generation 100))
           (print generation " " leftmost)
           (print-pots))
      finally (loop for s in pots
                    for p from leftmost
                    if s sum p into result1
                    if s sum (+ 50000000000 (- generation) p) into result2
                    finally (printf "~a\n~a\n" result1 result2)))
