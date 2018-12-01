(require-extension srfi-1)

(define rdata '())
(define result 0)

(do ((value (read) (read)))
    ((eof-object? value))
  (set! rdata (cons value rdata))
  (set! result (+ result value)))

(printf "~a\n" result)

(define data '())

(do ((it rdata (cdr it))) ((null? it))
  (set! data (cons (car it) data)))

(define freq 0)
(define done #f)
(define seen '())

(do () (done)
  (do ((it data (cdr it)))
      ((or (null? it) done))
    (set! seen (cons freq seen))
    (set! freq (+ freq (car it)))
    (set! done (member freq seen))))

(printf "~a\n" freq)
