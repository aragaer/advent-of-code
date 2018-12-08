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

(define ((can-do-step steps-done) step)
  (lset<= string=
          (hash-table-ref/default ordering step '())
          steps-done))

(loop for steps-to-do = *all-steps* then (remove (lambda (s) (string= s step)) steps-to-do)
      for steps-done = '() then (cons step steps-done)
      while (pair? steps-to-do)
      for step = (find (can-do-step steps-done) steps-to-do)
      do (display step)
      finally (newline))

(define elves 5)
(define (step-time step)
  (+ 61 (list-index (lambda (s) (string= step s)) *all-steps*)))

(define ((step-is-done-by second) i)
  (and (= (car i) second) (cdr i)))

(print
 (loop for second from 0
       with steps-to-do = *all-steps*
       for steps-done = '() then (append steps-done (filter-map (step-is-done-by second) queues))
       for queues = '() then (remove (step-is-done-by second) queues)
       while (< (length steps-done) (length *all-steps*))
       ;do (printf "~a ~a ~a\n" second queues steps-done)
       do (loop while (< (length queues) elves)
                for step in (filter (can-do-step steps-done) steps-to-do)
                ;do (printf "Elf ~a starts step ~a at time ~a\n" (length queues) step second)
                do (set! queues (cons (cons (+ second (step-time step)) step) queues))
                do (set! steps-to-do (remove (lambda (s) (string= s step)) steps-to-do)))
       finally (return second)))
