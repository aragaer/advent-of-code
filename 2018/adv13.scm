(load "common.scm")
(define *debug* #f)

(define direction-up +i)
(define direction-down -i)
(define direction-left -1)
(define direction-right 1)

(define (turn-up-right direction)
  (+ (imag-part direction) (* +i (real-part direction))))

(define (turn-up-left direction)
  (- (turn-up-right direction)))

(define (turn-left direction) (* +i direction))
(define (turn-right direction) (* -i direction))
(define turn-keep identity)

(define ((turn char) direction next-turn)
  (case char
    ((#\-) (values direction next-turn))
    ((#\|) (values direction next-turn))
    ((#\\) (values (turn-up-left direction) next-turn))
    ((#\/) (values (turn-up-right direction) next-turn))
    ((#\+) (values ((car next-turn) direction) (cdr next-turn)))))

(define (make-cart position direction)
  (list position direction (circular-list turn-left turn-keep turn-right)))

(when *debug*
    (define (debug-turn direction char)
      (format #t "From direction ~a at char ~a turn to direction ~a~%"
              direction char ((turn char) direction #f)))
    (debug-turn +i #\\))

(define carts '())

(define tracks
  (alist->hash-table
   (loop for y = 0 then (+ y -i)
         for line = (read-line)
         until (eof-object? line)
         append (map (lambda (x char)
                        (let ((position (+ x y)))
                          (cons position
                                (case char
                                  ((#\v)
                                   (push! (make-cart position direction-down) carts)
                                   (turn #\|))
                                  ((#\^)
                                   (push! (make-cart position direction-up) carts)
                                   (turn #\|))
                                  ((#\<)
                                   (push! (make-cart position direction-left) carts)
                                   (turn #\-))
                                  ((#\>)
                                   (push! (make-cart position direction-right) carts)
                                   (turn #\-))
                                  (else (turn char))))))
                      (range 0 (string-length line)) (string->list line)))))

(define (get-track-at position)
  (hash-table-ref tracks position))

(define (pos->string pos)
  (format #f "~a,~a" (real-part pos) (- (imag-part pos))))

(define (collision-handler cart1 cart2)
  (format #t "Collision at ~a~%" (pos->string (first cart1)))
  (set! carts (delete cart1 (delete cart2 carts))))

(define (move cart)
  (let* ((pos (first cart))
         (new-pos (+ pos (second cart))))
    (if *debug*
        (printf "Cart from ~a moved to ~a~%" (pos->string pos) (pos->string new-pos)))
    (when (member cart carts)
      (set-car! cart new-pos)
      (if* (find (lambda (other)
                   (= new-pos (first other)))
                 (delete cart carts))
          (collision-handler cart it))
      (let-values (((direction next-turn)
                    ((get-track-at new-pos) (second cart) (third cart))))
        (set-cdr! cart (list direction next-turn))))))

(define (complex-cmp n1 n2)
  (let ((x1 (real-part n1))
        (x2 (real-part n2))
        (y1 (imag-part n1))
        (y2 (imag-part n2)))
    (if (= x1 x2)
        (> y1 y2)
        (< x1 x2))))

(loop do (for-each move (sort carts
                              (lambda (cart1 cart2)
                                (complex-cmp (first cart1) (first cart2)))))
      while (< 1 (length carts))
      finally (let ((pos (caar carts)))
                (printf "Last cart at ~a\n" (pos->string pos))))
