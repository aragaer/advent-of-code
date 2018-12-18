(load "common.scm")

(define *debug* #f)

(define (pad area char)
  (let ((width (length (first area))))
    (append (list ((repeat (+ 2 width)) char))
            (map (lambda (line) (append (list char) line (list char))) area)
            (list ((repeat (+ 2 width)) char)))))

(define *all-lines* (read-lines))
(define *area* (map string->list *all-lines*))

(define (count-char char list)
  (count (lambda (c) (eq? c char)) list))

(define (transform plot)
  (let ((flat (apply append plot))
        (thing (second (second plot))))
    (case thing
      ((#\.) (if (<= 3 (count-char #\| flat)) #\| #\.))
      ((#\|) (if (<= 3 (count-char #\# flat)) #\# #\|))
      ((#\#) (if (and (<= 2 (count-char #\# flat)) (member #\| flat)) #\# #\.)))))

(define (step)
  (set! *area*
        (loop for it = (pad *area* #\.) then (cdr it)
              until (null? (cddr it))
              for line1 = (first it)
              for line2 = (second it)
              for line3 = (third it)
              collect (loop for it1 = line1 then (cdr it1)
                            for it2 = line2 then (cdr it2)
                            for it3 = line3 then (cdr it3)
                            until (null? (cddr it1))
                            collect (transform (list (take it1 3)
                                                     (take it2 3)
                                                     (take it3 3)))))))

(define (score)
  (let ((flat (apply append *area*)))
    (* (count-char #\| flat) (count-char #\# flat))))

(loop for minutes from 0 to 9
      if *debug* do (format #t "after ~a~%~{~{~a~}~%~}~%" minutes *area*)
      do (step)
      finally (if *debug* (format #t "after ~a~%~{~{~a~}~%~}~%" minutes *area*)))

(print (score))

(set! *area* (map string->list *all-lines*))

(loop for minutes from 0 to 500 do (step)
      finally (if *debug* (format #t "after ~a~%~{~{~a~}~%~}~%" minutes *area*)))

(define *loop-period* 0)
(define *need-more-steps* 0)

(loop for minutes from 501
      with images = (make-hash-table)
      for this-image = (format #f "~a" *area*)
      until (hash-table-exists? images this-image)
      do (hash-table-set! images this-image minutes)
      do (step)
      finally (let ((last-seen (hash-table-ref images this-image)))
                (if *debug* (format #t "Seen this on minute ~a and again on minute ~a~%" last-seen minutes))
                (set! *loop-period* (- minutes last-seen))
                (set! *need-more-steps* (- 1000000000 minutes))))

(when *debug*
  (format #t "Loop period is ~a~%" *loop-period*)
  (format #t "before loop ~a~%~{~{~a~}~%~}~%" (score) *area*)
  (loop for minutes from *loop-period* downto 1 do (step))
  (format #t "after loop ~a~%~{~{~a~}~%~}~%" (score) *area*)
  (format #t "after ~a result will be the same as after ~a~%" *need-more-steps* (modulo *need-more-steps* *loop-period*)))

(loop for more-minutes from 1 to (modulo *need-more-steps* *loop-period*)
      do (step)
      finally (if *debug* (format #t "~%~{~{~a~}~%~}~%" *area*))
      finally (print (score)))
