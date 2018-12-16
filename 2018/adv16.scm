(load "common.scm")

(define register-regex "(Before|After): +\\[(\\d+), (\\d+), (\\d+), (\\d+)\\]")
(define instruction-regex "(\\d+) (\\d+) (\\d+) (\\d+)")

(define *all-lines* (read-lines))
(define *all-opcodes* '())

(define-syntax opcode
  (syntax-rules ()
    ((opcode name func o1t o2t)
     (let ((name (lambda (regs args)
                   (let ((op1 (if (eq? o1t 'r) (nth regs (first args)) (first args)))
                         (op2 (if (eq? o2t 'r) (nth regs (second args)) (second args))))
                     (map (lambda (i val)
                            (if (= i (third args)) (func op1 op2) val)) '(0 1 2 3) regs)))))
       (push! name *all-opcodes*)))))

(opcode addr + 'r 'r)
(opcode addi + 'r 'i)
(opcode mulr * 'r 'r)
(opcode muli * 'r 'i)
(opcode banr bitwise-and 'r 'r)
(opcode bani bitwise-and 'r 'i)
(opcode borr bitwise-ior 'r 'r)
(opcode bori bitwise-ior 'r 'i)
(opcode setr (lambda (a b) a) 'r 'i)
(opcode seti (lambda (a b) a) 'i 'i)
(opcode gtir i> 'i 'r)
(opcode gtri i> 'r 'i)
(opcode gtrr i> 'r 'r)
(opcode eqir i= 'i 'r)
(opcode eqri i= 'r 'i)
(opcode eqrr i= 'r 'r)

(define *opcodes* (make-vector 16 '()))
(define *prog* '())

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
      do (vector-set! *opcodes* (car op)
                      (lset-union eq? (vector-ref *opcodes* (car op)) matching))
      count (<= 3 (length matching)) into result
      finally (print result)
      finally (set! *prog* (cddr it)))

(loop for alone = (vector-index (lambda (l) (and (pair? l) (= 1 (length l)))) *opcodes*)
      while alone
      for alone-op = (first (vector-ref *opcodes* alone))
      do (vector-set! *opcodes* alone alone-op)
      do (vector-map! (lambda (i l) (if (pair? l) (delete! alone-op l eq?) l)) *opcodes*))

(loop for ins-line in *prog*
      with regs = '(0 0 0 0)
      for ins = (map string->number (cdr (string-match instruction-regex ins-line)))
      for op = (vector-ref *opcodes* (first ins))
      do (set! regs (op regs (cdr ins)))
      finally (print (first regs)))
