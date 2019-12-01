(load "common.scm")

(define *debug* #t)

(define data-regex "([xy])=(\\d+),\\s+[xy]=(\\d+)\\.\\.(\\d+)")

(define *all-data* (read-lines))
(define *land* (make-hash-table))
(define *left* 2000)
(define *right* 0)
(define *top* 2000)
(define *bottom* 0)

(define *displayed-char*
  (alist->hash-table
   '((sand . #\.)
     (clay . #\#)
     (spring . #\+)
     (flow . #\|)
     (water . #\~))))

(loop for line in *all-data*
      for tokens = (cdr (string-match data-regex line))
      for nums = (map string->number (cdr tokens))
      for num1 = (first nums)
      for num2 = (second nums)
      for num3 = (third nums)
      for left = (if (string= (first tokens) "x") num1 num2)
      for right = (if (string= (first tokens) "x") num1 num3)
      for top = (if (string= (first tokens) "y") num1 num2)
      for bottom = (if (string= (first tokens) "y") num1 num3)
      for clay = (map (lambda (xy) (apply make-rectangular xy))
                      (cartesian-product (range left (+ 1 right)) (range top (+ 1 bottom))))
      minimize left into l
      maximize right into r
      minimize top into t
      maximize bottom into b
      do (for-each (lambda (spot) (hash-table-set! *land* spot 'clay)) clay)
      finally (begin
                (set! *left* (- l 1))
                (set! *right* (+ r 1))
                (set! *top* t)
                (set! *bottom* b)))

(hash-table-set! *land* 500 'spring)
;(hash-table-set! *land* 500+i 'flow)

(define *real-top* *top*)
(if (< 0 *top*) (set! *top* 0))

(define (thing-at x y)
  (hash-table-ref/default *land* (make-rectangular x y) 'sand))

(define (flow-to x y)
  (hash-table-set! *land* (make-rectangular x y) 'flow))

(define (flood-at x y)
  (hash-table-set! *land* (make-rectangular x y) 'water))

(define (trickle-from x y)
  (if *debug* (print "Trickle from " x ":" y))
  (loop for py from (+ 1 y) to *bottom*
        for thing = (thing-at x py)
        while (eq? thing 'sand)
        do (flow-to x py)
        finally (if (not (eq? thing 'sand)) (pool-from x (- py 1)) #f)))

(define (pool-from x y)
  (when *debug*
    (newline)
    (print "Pool from " x ":" y)
    (print-state))
  (if (< y *bottom*)
      (let ((left-edge
             (loop for px downfrom (- x 1)
                   for thing = (thing-at px y)
                   for thing-below = (thing-at px (+ 1 y))
                   for will-pool = (member thing '(sand flow))
                   while will-pool
                   unless (eq? thing-below 'flow) do (flow-to px y)
                   while (member thing-below '(clay water))
                   finally (return px)))
            (right-edge
             (loop for px from (+ x 1)
                   for thing = (thing-at px y)
                   for thing-below = (thing-at px (+ 1 y))
                   for will-pool = (member thing '(sand flow))
                   while will-pool
                   unless (eq? thing-below 'flow) do (flow-to px y)
                   while (member thing-below '(clay water))
                   finally (return px))))
        (if *debug* (print left-edge " " (thing-at left-edge y) ", " right-edge " " (thing-at right-edge y)))
        (if (or (eq? (thing-at left-edge y) 'flow)
                (eq? (thing-at right-edge y) 'flow))
            (begin
              (if (eq? (thing-at left-edge y) 'flow)
                  (trickle-from left-edge y))
              (if (eq? (thing-at right-edge y) 'flow)
                  (trickle-from right-edge y)))
            (let ((left-to-right (range (+ 1 left-edge) right-edge)))
              (for-each (lambda (px) (flood-at px y)) left-to-right)
              (pool-from (find (lambda (px) (eq? (thing-at px (- y 1)) 'flow)) left-to-right) (- y 1)))))
      (if *debug* (print "Flowing out at " x ":" y))))

(define (print-state)
  (for-each
   (lambda (y)
     (print (list->string (map (lambda (thing)
                                 (hash-table-ref *displayed-char* thing))
                               (map (lambda (x) (thing-at x y))
                                    (range *left* (+ 1 *right*)))))
            " " y))
   (range *top* (+ 1 *bottom*))))

(trickle-from 500 0)

(if *debug* (print-state))

(print (hash-table-fold
        *land*
        (lambda (k v s)
          (+ s
             (if (and (>= (imag-part k) *real-top*)
                      (member v '(water flow)))
                 1 0)))
        0))

(print (hash-table-fold
        *land*
        (lambda (k v s)
          (+ s
             (if (and (>= (imag-part k) *real-top*)
                      (eq? v 'water))
                 1 0)))
        0))
