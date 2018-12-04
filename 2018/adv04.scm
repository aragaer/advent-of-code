(load "common.scm")

(define lines (sort (read-lines) string<))

(define begin-regex "Guard #(\\d+) begins shift")
(define sleep-regex "\\[.*?:(\\d\\d)\\] [wf]")

(define sleep-data (make-hash-table))

(define ((extract-num regex) line)
  (let ((match (string-search regex line)))
    (and match (string->number (second match)))))

(loop for it = lines then (if begin-shift (cdr it) (cddr it))
      until (null? it)
      for begin-shift = ((extract-num begin-regex) (first it))
      for current-guard = begin-shift then (or begin-shift current-guard)
      for sleep-range = (map (extract-num sleep-regex) (take it 2))
      unless begin-shift
      do (hash-table-update!/default sleep-data current-guard
                                     (lambda (v) (for-each (vector-inc-at! v) (apply range sleep-range)) v)
                                     (make-vector 60 0)))

(let* ((most-sleepy (hash-table-fold sleep-data
                                     (lambda (k v state)
                                       (let ((sleep-time (reduce + 0 (vector->list v))))
                                         (if (> sleep-time (second state))
                                             (list k sleep-time)
                                             state)))
                                     '(0 0)))
       (the-minute (vector-max (hash-table-ref sleep-data (car most-sleepy)))))
  (printf "Guard ~a is most sleepy - ~a minutes total\n" (first most-sleepy) (second most-sleepy))
  (printf "Slept most at minute ~a - ~a times\n" (car the-minute) (cdr the-minute))
  (printf "Result: ~a\n" (* (car most-sleepy) (car the-minute))))

(let ((res2 (hash-table-fold sleep-data
                             (lambda (k v state)
                               (let ((the-minute (vector-max v)))
                                 (if (> (cdr the-minute) (second state))
                                     (list k (cdr the-minute) (car the-minute))
                                     state)))
                             '(0 0 0))))
  (apply printf "Guard ~a sleeps ~a times at minute ~a\n" res2)
  (printf "Result 2: ~a\n" (* (first res2) (third res2))))
