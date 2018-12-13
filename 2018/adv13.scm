(load "common.scm")
(define *debug* #f)

(define (turn-up-right direction)
  (+ (imag-part direction) (* +i (real-part direction))))

(define (turn-up-left direction)
  (- (turn-up-right direction)))

(define (turn-left direction) (* +i direction))
(define (turn-right direction) (* -i direction))
(define turn-straight identity)

(define position-of first)
(define direction-of second)

(define (char->direction char)
  (if* (assoc char '((#\v . -i) (#\^ . +i) (#\< . -1) (#\> . +1)))
       (cdr it)
       #f))

(define (turn-func char)
  (case char
    ((#\\) (lambda (cart) (set! (direction-of cart) (turn-up-left (direction-of cart)))))
    ((#\/) (lambda (cart) (set! (direction-of cart) (turn-up-right (direction-of cart)))))
    ((#\+) (lambda (cart) (let ((next-turn (third cart)))
                            (set-cdr! cart (list ((car next-turn) (direction-of cart))
                                                 (cdr next-turn))))))
    (else identity)))

(define (make-cart position direction)
  (list position direction (circular-list turn-left turn-straight turn-right)))

(when *debug*
    (define (debug-turn direction char)
      (format #t "From direction ~a at char ~a turn to direction ~a~%"
              direction char ((turn-func char) direction #f)))
    (debug-turn +i #\\))

(define carts '())

(define tracks
  (alist->hash-table
   (loop for y = 0 then (+ y -i)
         for line = (read-line)
         until (eof-object? line)
         append (map (lambda (x char)
                       (let ((position (+ x y)))
                         (if* (char->direction char)
                              (push! (make-cart position it) carts))
                         (cons position (turn-func char))))
                      (range 0 (string-length line)) (string->list line)))))

(define (track-func-at position)
  (hash-table-ref tracks position))

(define (pos->string pos)
  (format #f "~a,~a" (real-part pos) (- (imag-part pos))))

(define (collision-handler cart1 cart2)
  (format #t "Collision at ~a~%" (pos->string (position-of cart1)))
  (set! carts (delete cart1 (delete cart2 carts))))

(define (move cart)
  (let* ((pos (position-of cart))
         (new-pos (+ pos (direction-of cart))))
    (if *debug*
        (printf "Cart from ~a moved to ~a~%" (pos->string pos) (pos->string new-pos)))
    (when (member cart carts)
      (set-car! cart new-pos)
      (if* (find (lambda (other)
                   (= new-pos (position-of other)))
                 (delete cart carts))
           (collision-handler cart it))
      ((track-func-at new-pos) cart))))

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
                                (complex-cmp (position-of cart1) (position-of cart2)))))
      while (< 1 (length carts))
      finally (if (pair? carts)
                  (printf "Last cart at ~a\n" (pos->string (caar carts)))
                  (print "No carts left")))
