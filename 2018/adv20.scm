(load "common.scm")

(define *debug* #f)
(define *verbose* (and *debug* #t))

(define (extract-options rex)
  (let* ((result '())
         (new-it (loop with depth = 1
                       with current = '()
                       for it = rex then (cdr it)
                       for char = (car it)
                       if (eq? char #\() do (inc! depth)
                       if (eq? char #\)) do (dec! depth)
                       until (= 0 depth)
                       do (if (and (= 1 depth ) (eq? char #\|))
                              (begin
                                (push! (reverse current) result)
                                (set! current '()))
                              (push! char current))
                       finally (push! (reverse current) result)
                       finally (return it))))
    (values result (cdr new-it))))

(define *too-far* 1000)

(define (walk rex dist-so-far)
  (if *debug* (print "Walking " rex " from " dist-so-far))
  (loop for it = rex then (cdr it)
        with dist = dist-so-far
        with result = 0
        until (null? it)
        for char = (car it)
        until (eq? char #\$)
        if (member char '(#\W #\N #\E #\S))
        do (begin
             (inc! dist)
             (if (<= *too-far* dist) (inc! result)))
        if (eq? char #\()
        do (let-values (((options new-it) (extract-options (cdr it))))
             (if *verbose* (print "Got options: " options " from " (cdr it)))
             (if (member '() options)
                 (begin
                   (inc! result
                         (fold (lambda (option total)
                                 (let ((walked (walk (take option (/ (length option) 2)) dist)))
                                   (+ total (first walked))))
                               0 (cdr options)))
                   (set! it (cons 0 new-it)))
                 (begin
                   (inc! result
                         (fold (lambda (option total)
                                 (let ((walked (walk option dist)))
                                   (+ total (first (walk new-it (+ dist (second walked)))) (first walked))))
                               0 options))
                   (if *debug* (print "Returning1 " result " " dist " for " rex))
                   (return (list result dist)))))
        finally (if *debug* (print "Returning2 " result " . " dist " for " rex))
        finally (return (list result dist))))

(define (greedy rex)
  (loop for it = rex then (cdr it)
        with result = 0
        until (null? it)
        for char = (car it)
        until (eq? char #\$)
        if (member char '(#\W #\N #\E #\S))
        do (inc! result)
        if (eq? char #\()
        do (let-values (((options new-it) (extract-options (cdr it))))
             (if (member '() options)
                 (inc! result (apply max (greedy new-it) (map (lambda (opt) (/ (greedy opt) 2)) options)))
                 (inc! result (+ (greedy new-it) (apply max (map greedy options)))))
             (return result))
        finally (return result)))

(loop for line = (read-line)
      until (eof-object? line)
      for rex = (cdr (string->list line))
      do (print (greedy rex))
      do (print (first (walk rex 0))))
