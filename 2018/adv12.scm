(load "common.scm")

(define rules (make-vector 32 #f))

(define (char->bool char) (eq? char #\#))
(define (bool->char bool) (if bool #\# #\.))

(define pots
  (let ((line (drop (string->list (read-line)) 15)))
    (map char->bool line)))

(read-line)

(define (window->num state)
  (reduce + 0 (map (lambda (p v) (if p v 0)) state '(1 2 4 8 16))))

(for-each
 (lambda (line)
   (let ((state (take line 5))
         (result (char->bool (last line))))
     (vector-set! rules (window->num (map char->bool state)) result)))
 (map string->list (read-lines)))

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
      finally (print (loop for s in pots
                           for p from leftmost
                           if s sum p))
      finally (print (loop for s in pots
                           for p from (+ 50000000000 (- generation) leftmost)
                           if s sum p)))
