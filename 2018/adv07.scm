(load "common.scm")

(define regex "Step (\\w) must be finished before step (\\w) can begin\\.")

(define *all-steps* '())
(define ordering (make-hash-table))

(for-each
 (lambda (line)
   (let* ((parsed (string-search regex line))
          (req (second parsed))
          (step (third parsed)))
     (set! *all-steps* (cons req *all-steps*))
     (set! *all-steps* (cons step *all-steps*))
     (hash-table-update!/default ordering step
                                 (lambda (old) (cons req old))
                                 '())))
 (read-lines))

(set! *all-steps* (sort (delete-duplicates *all-steps*) string<))

(loop with steps-to-do = *all-steps*
      with steps-done = '()
      while (pair? steps-to-do)
      for step = (find (lambda (step)
                         (lset<= string=
                                 (hash-table-ref/default ordering step '())
                                 steps-done))
                       steps-to-do)
      do (display step)
      do (set! steps-to-do (remove (lambda (s) (string= s step)) steps-to-do))
      do (set! steps-done (cons step steps-done))
      finally (newline))

(define elves 5)
(define (step-time step)
  (+ 61 (list-index (lambda (s) (string= step s)) *all-steps*)))

(print
 (loop for second from 0
       for steps-done = '() then (append steps-done
                                         (filter-map (lambda (i)
                                                       (and (= (car i) second) (cdr i))) queues))
       for queues = '() then (remove (lambda (i) (= (car i) second)) queues)
       with steps-to-do = *all-steps*
       while (< (length steps-done) (length *all-steps*))
       for steps-available = (filter (lambda (step)
                                       (lset<= string=
                                               (hash-table-ref/default ordering step '())
                                               steps-done))
                                     steps-to-do)
       ;do (printf "~a ~a ~a\n" second queues steps-done)
       do (loop while (< (length queues) elves)
                for step in steps-available
                ;do (printf "Elf ~a starts step ~a at time ~a\n" (length queues) step second)
                do (set! queues (cons (cons (+ second (step-time step)) step) queues))
                do (set! steps-to-do (remove (lambda (s) (string= s step)) steps-to-do)))
       finally (return second)))
