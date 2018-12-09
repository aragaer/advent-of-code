(load "common.scm")
(load "linked-list.scm")

(define regex "(\\d+) players; last marble is worth (\\d+) points.*")

(define (take-7-back marble)
  (repeat* 7 (set! marble (linked-list-prev marble)))
  marble)

(define (game-turn current new-value)
  (if (= 0 (modulo new-value 23))
      (let ((taken (take-7-back current)))
        (linked-list-remove taken)
        (values (+ new-value (linked-list-value taken)) (linked-list-next taken)))
      (let ((new-marble (make-linked-list-item new-value)))
        (linked-list-insert-after (linked-list-next current) new-marble)
        (values 0 new-marble))))

(define (game players last-marble)
  (loop for current-marble from 1 to last-marble
        for current-player = 0 then (modulo (+ 1 current-player) players)
        with scores = (make-vector players 0)
        with current = (make-linked-list-item 0)
        initially (linked-list-link current current)
        do (let-values (((score new-current) (game-turn current current-marble)))
             (inc! (vector-ref scores current-player) score)
             (set! current new-current))
        finally (return (apply max (vector->list scores)))))

(for-each (compose print game (p string->number) (match regex)) (read-lines))
