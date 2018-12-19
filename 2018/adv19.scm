(load "common.scm")

(define (replace list index new-value)
  (map (lambda (i value)
         (if (= i index) new-value value)) '(0 1 2 3 4 5) list))

(define-syntax opcode
  (syntax-rules ()
    ((opcode name func o1t o2t)
     (define ((name args) regs)
       (let ((op1 (if (eq? o1t 'r) (nth regs (first args)) (first args)))
             (op2 (if (eq? o2t 'r) (nth regs (second args)) (second args))))
         (map (lambda (i val)
                (if (= i (third args)) (func op1 op2) val)) '(0 1 2 3 4 5) regs))))))

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
(define (nop args) identity)

(define *ip-reg*
  (let ((ip-reg-line (read-line))
        (ip-reg-regex "#ip (\\d+)"))
    (string->number (second (string-match ip-reg-regex ip-reg-line)))))
(print *ip-reg*)

(define *prog*
  (list->vector
   (loop for op = (read)
         until (eof-object? op)
         for args = (map (lambda (i) (read)) (range 0 3))
         do (read-line)
         collect ((eval op) args))))

(if #f
    (loop with ip = 0
          while (and (<= 0 ip) (> (vector-length *prog*) ip))
          with regs = '(0 0 0 0 0 0)
          do (set! regs (replace regs *ip-reg* ip))
          do (format #t "ip=~a [~{~a~^, ~}] " ip regs)
          do (set! regs ((vector-ref *prog* ip) regs))
          do (format #t "[~{~a~^, ~}]~%" regs)
          do (set! ip (+ 1 (nth regs *ip-reg*)))
          finally (print (first regs))))

(loop with ip = 0
      until (= 1 ip) ; at this point reg4 contains the value to factor
      while (and (<= 0 ip) (> (vector-length *prog*) ip))
      with regs = '(0 0 0 0 0 0)
      do (set! regs (replace regs *ip-reg* ip))
      do (format #t "ip=~a [~{~a~^, ~}] " ip regs)
      do (set! regs ((vector-ref *prog* ip) regs))
      do (format #t "[~{~a~^, ~}]~%" regs)
      do (set! ip (+ 1 (nth regs *ip-reg*)))
      finally (print (loop for i from 1 to (fifth regs)
                           if (= 0 (modulo (fifth regs) i)) sum i)))
(loop with ip = 0
      until (= 1 ip) ; at this point reg4 contains the value to factor
      while (and (<= 0 ip) (> (vector-length *prog*) ip))
      with regs = '(1 0 0 0 0 0)
      do (set! regs (replace regs *ip-reg* ip))
      do (format #t "ip=~a [~{~a~^, ~}] " ip regs)
      do (set! regs ((vector-ref *prog* ip) regs))
      do (format #t "[~{~a~^, ~}]~%" regs)
      do (set! ip (+ 1 (nth regs *ip-reg*)))
      finally (print (loop for i from 1 to (fifth regs)
                           if (= 0 (modulo (fifth regs) i)) sum i)))
