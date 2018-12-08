(load "common.scm")

(define *ordering*
  (let ((regex "Step (\\w) must be finished before step (\\w) can begin\\."))
    (define (collect k-v alist)
      (cons (append k-v (assoc-value/default (car k-v) alist)) alist))
    (delete-duplicates
     (fold collect '()
           (map (compose list (p string->char) (match regex)) (read-lines)))
     (compose eq? (p car)))))

(define *all-steps* (sort (delete-duplicates (flatten *ordering*)) char<?))

(define ((can-do-step? steps-done) step)
  (lset<= eq? (assoc-value/default step *ordering*) steps-done))

(loop for steps-to-do = *all-steps* then (delete step steps-to-do)
      for steps-done = '() then (cons step steps-done)
      while (< (length steps-done) (length *all-steps*))
      for step = (find (can-do-step? steps-done) steps-to-do)
      do (display step)
      finally (newline))

(define elves 5)
(define base-time 60)
(define step-times (butlast (fold (lambda (step times)
                                    (alist-cons step (+ 1 (cdar times)) times))
                                  `((#\space . ,base-time)) *all-steps*)))
(define (step-time step)
  (assoc-value/default step step-times))

(let ((steps-to-do *all-steps*) (steps-done '()) (queues '()))
  (define ((step-is-done-by? second) i) (= (car i) second))
  (loop for second from 0
        do (let-values (((done queued) (partition (step-is-done-by? second) queues)))
             (set! steps-done (append (map cdr done) steps-done))
             (set! queues queued))
        while (< (length steps-done) (length *all-steps*))
        do (loop while (< (length queues) elves)
                 for step in (filter (can-do-step? steps-done) steps-to-do)
                 do (push! (cons (+ second (step-time step)) step) queues)
                 do (set! steps-to-do (delete step steps-to-do)))
        finally (print second)))
