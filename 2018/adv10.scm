(load "common.scm")

(define *stars* '())
(define star-regex "position=<\\s*(-?\\d+),\\s*(-?\\d+)> velocity=<\\s*(-?\\d+),\\s*(-?\\d+)>")
(define *step* 0)

(let ((file (read-line)))
  (with-input-from-file file
    (lambda ()
      (loop for line = (read-line)
            until (eof-object? line)
            for star = ((compose list (p string->number) (match star-regex)) line)
            do (set! *stars* (cons star *stars*))))))

(define (move steps)
  (inc! *step* steps)
  (set! *stars*
        (map (lambda (star)
               (list (+ (first star) (* steps (third star)))
                     (+ (second star) (* steps (fourth star)))
                     (third star)
                     (fourth star)))
             *stars*)))

(define (print-fragment)
  (print *step*)
  (let ((left (apply min (map first *stars*)))
        (top (apply min (map second *stars*)))
        (bottom (apply max (map second *stars*))))
    (printf "~a - ~a\n" top bottom)
    (loop for y from top to (+ 10 top)
          for res = (make-vector 80 #\space)
          do (loop for star in *stars*
                   for x = (first star)
                   if (and (= (second star) y)
                           (> (+ 80 left) x))
                     do (vector-set! res (- x left) #\#))
          do (format #t "~{~a~}~%" (vector->list res)))))

(print-fragment)

(loop for command = (read-line)
      until (eof-object? command)
      for step = 0
      if (string-prefix? "f" command)
        do (set! step (string->number (substring/shared command 1)))
      if (string-prefix? "b" command)
        do (set! step (- (string->number (substring/shared command 1))))
      do (move step)
      do (print-fragment))
