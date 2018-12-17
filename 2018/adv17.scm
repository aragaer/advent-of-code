(load "common.scm")

(define *debug* #f)

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
(hash-table-set! *land* 500+i 'flow)

(define *real-top* *top*)
(if (< 0 *top*)
    (set! *top* 0))

(print *left* ":" *top* " - " *right* ":" *bottom*)

(define (thing-at x y)
  (hash-table-ref/default *land* (make-rectangular x y) 'sand))

(define (flow-to x y)
  (hash-table-set! *land* (make-rectangular x y) 'flow))

(define (flood-at x y)
  (hash-table-set! *land* (make-rectangular x y) 'water))

(define (trickle-from x y)
  (loop for py from (+ 1 y) to *bottom*
        for thing = (thing-at x py)
        while (eq? thing 'sand)
        do (flow-to x py)
        finally (if (not (eq? thing 'sand)) (pool-from x (- py 1)))))

(define (pool-from x y)
  (let ((wall-on-left
         (loop for px from (- x 1) downto *left*
               for thing = (thing-at px y)
               for thing-below = (thing-at px (+ 1 y))
               for will-pool = (member thing '(sand flow))
               while will-pool
               unless (eq? thing-below 'flow) do (flow-to px y)
               while (member thing-below '(clay water))
               finally (return (if will-pool #f (+ px 1)))))
        (wall-on-right
         (loop for px from (+ x 1) to *right*
               for thing = (thing-at px y)
               for thing-below = (thing-at px (+ 1 y))
               for will-pool = (member thing '(sand flow))
               while will-pool
               unless (eq? thing-below 'flow) do (flow-to px y)
               while (member thing-below '(clay water))
               finally (return (if will-pool #f (- px 1))))))
    (if (and wall-on-left wall-on-right)
        (begin
          (for-each (lambda (px) (flood-at px y)) (range wall-on-left (+ 1 wall-on-right)))
          (for-each (lambda (px)
                      (if (eq? (thing-at px (- y 1)) 'flow)
                          (pool-from px (- y 1))))
                    (range wall-on-left (+ 1 wall-on-right))))
        #f)))

(define (print-state)
  (for-each
   (lambda (y)
     (print (list->string (map (lambda (thing)
                                 (hash-table-ref *displayed-char* thing))
                               (map (lambda (x) (thing-at x y))
                                    (range *left* (+ 1 *right*)))))
            " " y))
   (range *top* (+ 1 *bottom*))))

(define (step)
  (loop for y in (reverse (range *top* *bottom*))
        sum (loop for x from *left* to *right*
                    for thing = (thing-at x y)
                    for thing-below = (thing-at x (+ 1 y))
                    for should-process = (and (eq? thing 'flow) (eq? thing-below 'sand))
                    count should-process
                    if should-process
                    do (trickle-from x y))))

(loop for s-n from 1
      do (print s-n)
      if *debug* do (begin (print-state) (newline))
      while (< 0 (step)))

(print-state)

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
