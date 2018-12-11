(load "common.scm")

(define (power-level grid-sn x y)
  (let ((rack-id (+ x 10)))
    (- (fx/ (fxmod (* (+ (* rack-id y) grid-sn) rack-id) 1000) 100) 5)))

(define (cell grid-sn)
  (map (lambda (x)
         (map (lambda (y)
                (power-level grid-sn x y))
              (range 1 301)))
       (range 1 301)))

(define (sliding-lists my-list count)
  (unfold (lambda (x) (= count (first x)))
          (lambda (x) (second x))
          (lambda (x) (list (+ (first x) 1) (cdr (second x))))
          (list 0 my-list)))

(define (sliding-map func list window)
  (apply map func (sliding-lists list window)))

(define (max-power new old)
  (if (> (first new)
         (first old))
      new
      old))

(define ((sliding-square grid) window)
  (let ((sliding (sliding-map (lambda args (apply map + args))
                              (map (lambda (rack) (sliding-map + rack window)) grid)
                              window)))
    (Fold-left max-power '(0 0 0)
               (list->List (append-map (lambda (col x)
                                         (zip col (circular-list x) (range 1 1000) (circular-list window)))
                                       sliding (range 1 1000))))))

(define (largest-power-location grid-sn)
  (let ((grid (cell grid-sn)))
    (Fold-left max-power
               (list (caar grid) 1 1 1)
               (Map (sliding-square grid) (Range 1 301)))))

;(print (largest-power-location 18))
;(print (largest-power-location 42))
(print (largest-power-location 1718))
