(load "common.scm")

(define top 1000)
(define left 1000)
(define bottom 0)
(define right 0)

(define points
  (loop for x = (read)
        until (eof-object? x)
        for comma = (read-char)
        for y = (read)
        do (set! left (min x left))
        do (set! right (max x right))
        do (set! top (min y top))
        do (set! bottom (max y bottom))
        collect (list x y)))

(define unlimited 1000000)

(define ((distance x1 y1) x2 y2)
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(define (distance2 pt1 pt2)
  (reduce + 0 (map (compose abs -) pt1 pt2)))

(define perfect-range 10000)

(define areas (make-hash-table))
(define perfect 0)

(loop for pt in (cartesian-product (range left (+ 1 right)) (range top (+ 1 bottom)))
      for distances = (sort (map (lambda (pt2) (cons (distance2 pt pt2) pt2)) points)
                            (lambda (d1 d2) (< (first d1) (first d2))))
      for closest = (cdr (first distances))
      if (< (first (first distances)) (first (second distances)))
      do (if (or (= (first pt) left)
                 (= (first pt) right)
                 (= (second pt) top)
                 (= (second pt) bottom))
             (hash-table-set! areas (cons (first closest) (second closest)) unlimited)
             (count-into (cons (first closest) (second closest)) areas))
      if (> perfect-range (reduce + 0 (map first distances)))
      do (inc! perfect))

(print (hash-table-fold areas (lambda (k v s) (if (> unlimited v) (max v s) s)) 0))
(print perfect)
