(load "common.scm")

(define lines (sort (read-lines) string<))

(define begin-regex "\\[.*\\] Guard #(\\d+) begins shift")
(define sleep-regex "\\[.*?:(\\d\\d)\\] falls asleep")
(define wake-regex "\\[.*?:(\\d\\d)\\] wakes up")

(define asleep-times (make-hash-table))
(define asleep-minutes-count (make-hash-table))

(do ((it lines (cdr it)) (current-guard '()))
    ((null? it))
  (let* ((line (car it))
         (shift (string-match begin-regex line))
         (sleep (string-match sleep-regex line)))
    (cond (shift
           (set! current-guard (string->number (second shift)))
           (if (not (hash-table-exists? asleep-minutes-count current-guard))
               (hash-table-set! asleep-minutes-count current-guard (make-vector 60 0))))
          (sleep
           (let ((sleep-at (string->number (second sleep)))
                 (wake-at (string->number (second (string-match wake-regex (cadr it)))))
                 (minutes (hash-table-ref asleep-minutes-count current-guard)))
             (set! it (cdr it))
             (for-each (lambda (m)
                         (vector-set! minutes m (+ 1 (vector-ref minutes m))))
                       (range sleep-at wake-at))
             (sum-into current-guard asleep-times (- wake-at sleep-at))))
          (else
           (abort line)))))

(let* ((most-sleepy (hash-table-fold asleep-times
                                     (lambda (k v state)
                                       (if (> v (cdr state))
                                           (cons k v)
                                           state))
                                     (cons 0 0)))
       (the-minute (vector-max (hash-table-ref asleep-minutes-count (car most-sleepy)))))
  (printf "Guard ~a is most sleepy - ~a minutes total\n" (car most-sleepy) (cdr most-sleepy))
  (printf "Slept most at minute ~a - ~a times\n" (car the-minute) (cdr the-minute))
  (printf "Result: ~a\n" (* (car most-sleepy) (car the-minute))))

(let ((res2 (hash-table-fold asleep-minutes-count
                             (lambda (k v state)
                               (let ((the-minute (vector-max v)))
                                 (if (> (cdr the-minute) (second state))
                                     (list k (cdr the-minute) (car the-minute))
                                     state)))
                             '(0 0 0))))
  (apply printf "Guard ~a sleeps ~a times at minute ~a\n" res2)
  (printf "Result 2: ~a\n" (* (first res2) (third res2))))
