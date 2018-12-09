(load "common.scm")

(define regex "(\\d+) players; last marble is worth (\\d+) points.*")

(define (insert-after marble new-marble-score)
  (let ((next (next marble))
        (new-marble (make-marble new-marble-score)))
;    (printf "Inserting ~a between ~a and ~a\n" new-marble-score (car marble) (car next))
    (set-next marble new-marble)
    (set-next new-marble next)
    (set-prev next new-marble)
    (set-prev new-marble marble)
    new-marble))

(define (remove marble)
  (let ((next (next marble))
        (prev (prev marble)))
;    (printf "Removing ~a from between ~a and ~a\n" (car marble) (car prev) (car next))
    (set-next prev next)
    (set-prev next prev)
    next))

(define (print-all marble count)
  (loop for i from 1 to count
        do (display (car marble))
        do (display #\space)
        do (set! marble (next marble))
        finally (newline)))

(define (print-all-reverse marble count)
  (loop for i from 1 to count
        do (display (car marble))
        do (display #\space)
        do (set! marble (prev marble))
        finally (newline)))

(define (next marble)
  (car (cdr marble)))

(define (prev marble)
  (cdr (cdr marble)))

(define (set-next marble next)
  (set-car! (cdr marble) next))

(define (set-prev marble prev)
  (set-cdr! (cdr marble) prev))

(define (make-marble value)
  (list value '()))

(define (take-7-back marble)
  (repeat* 7 (set! marble (prev marble)))
  marble)

(define (game players last-marble)
  (let* ((current (make-marble 0))
         (scores (make-vector players 0))
         (count 1))
    (set-next current current)
    (set-prev current current)
    (loop for current-marble from 1 to (* last-marble 100)
          for current-player = 0 then (modulo (+ 1 current-player) players)
          do (if (= 0 (modulo current-marble 23))
                 (begin (inc! (vector-ref scores current-player) current-marble)
                        (let ((taken (take-7-back current)))
                          (inc! (vector-ref scores current-player) (car taken))
                          (set! current (remove taken)))
                        (dec! count))
                 (begin (set! current (insert-after (next current) current-marble))
                        (inc! count)))
          finally (return (apply max (vector->list scores))))))


(for-each (compose print game (p string->number) (match regex)) (read-lines))
