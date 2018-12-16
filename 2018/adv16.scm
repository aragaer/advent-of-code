(load "common.scm")

(define register-regex "(Before|After): +\\[(\\d+), (\\d+), (\\d+), (\\d+)\\]")
(define instruction-regex "(\\d+) (\\d+) (\\d+) (\\d+)")

(define *all-lines* (read-lines))
(define *all-opcodes* '())

(define-syntax opcode
  (syntax-rules ()
    ((opcode name func o1t o2t)
     (push! (lambda (regs args)
              (let ((op1 (if (eq? o1t 'r) (nth regs (first args)) (first args)))
                    (op2 (if (eq? o2t 'r) (nth regs (second args)) (second args))))
                (map (lambda (i val)
                       (if (= i (third args)) (func op1 op2) val)) '(0 1 2 3) regs)))
            *all-opcodes*))))

(define (just-first a b) a)

(opcode i-addr + 'r 'r)
(opcode i-addi + 'r 'i)
(opcode i-mulr * 'r 'r)
(opcode i-muli * 'r 'i)
(opcode i-banr bitwise-and 'r 'r)
(opcode i-bani bitwise-and 'r 'i)
(opcode i-borr bitwise-ior 'r 'r)
(opcode i-bori bitwise-ior 'r 'i)
(opcode i-setr just-first 'r 'i)
(opcode i-seti just-first 'i 'i)
(opcode i-gtir i> 'i 'r)
(opcode i-gtri i> 'r 'i)
(opcode i-gtrr i> 'r 'r)
(opcode i-eqir i= 'i 'r)
(opcode i-eqri i= 'r 'i)
(opcode i-eqrr i= 'r 'r)

(define *opcodes* (make-vector 16))
(define *prog* '())

(define *candidates* (make-vector 16 '()))

(loop for it = *all-lines* then (if (null? it) it (cdr it))
      until (null? it)
      for line = (car it)
      for match = (string-match register-regex line)
      while match
      for before = (map string->number (cddr match))
      for op = (map string->number (cdr (string-match instruction-regex (cadr it))))
      for after = (map string->number (cddr (string-match register-regex (caddr it))))
      for matching = (filter (lambda (ins)
                               (list= = after (ins before (cdr op)))) *all-opcodes*)
      do (set! it (cdddr it))
      do (vector-set! *candidates* (car op)
                      (lset-union eq? (vector-ref *candidates* (car op)) matching))
      count (<= 3 (length matching)) into result
      finally (print result)
      finally (set! *prog* (cddr it)))

(loop for alone = (vector-index (lambda (l) (= 1 (length l))) *candidates*)
      while alone
      for alone-op = (first (vector-ref *candidates* alone))
      do (vector-set! *opcodes* alone alone-op)
      do (vector-map! (lambda (i l) (delete! alone-op l eq?)) *candidates*))

(loop for ins-line in *prog*
      with regs = '(0 0 0 0)
      for ins = (map string->number (cdr (string-match instruction-regex ins-line)))
      for op = (vector-ref *opcodes* (first ins))
      do (set! regs (op regs (cdr ins)))
      finally (print (first regs)))
