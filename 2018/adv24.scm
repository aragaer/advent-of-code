(load "common.scm")

(define *debug* #f)
(define *verbose* (and *debug* #f))

(define *binary-search* #f)

(define group-regex
  "(\\d+) units each with (\\d+) hit points (\\((.*?)\\))? ?with an attack that does (\\d+) (\\w+) damage at initiative (\\d+)")
(define imm-weak-regex "(immune|weak) to (.*?);?")

(define (make-group army count hit-points weaknesses immunities damage attack-type initiative)
  `((army . ,army)
    (count . ,count)
    (hit-points . ,hit-points)
    (weaknesses . ,weaknesses)
    (immunities . ,immunities)
    (damage . ,damage)
    (attack-type . ,attack-type)
    (initiative . ,initiative)))

(define *lines* (read-lines))

(define (army boost)
  (loop with army
        for number from 0
        for line in *lines*
        for match = (string-match group-regex line)
        if (not match)
        do (if* (string-index line #\:)
                (begin
                  (set! army (substring/shared line 0 it))
                  (set! number 0)))
        if match collect (let ((cnt (string->number (second match)))
                               (hp (string->number (third match)))
                               (imm-weak (fifth match))
                               (damage (string->number (sixth match)))
                               (attack-type (seventh match))
                               (initiative (string->number (eighth match)))
                               (weaknesses '())
                               (immunities '()))
                           (if imm-weak
                               (loop for token in (string-split-fields "; " imm-weak #:infix)
                                     for x = (string-split-fields " to |, " token #:infix)
                                     do (if (string= (first x) "weak")
                                            (set! weaknesses (cdr x))
                                            (set! immunities (cdr x)))))
                           (if (string= army "Immune System") (inc! damage boost))
                           (make-group army cnt hp weaknesses immunities damage attack-type initiative))))

(define (effective-power group)
  (* (assoc-value 'count group) (assoc-value 'damage group)))

(define (targeting-order-cmp group1 group2)
  (let ((ep1 (effective-power group1))
        (ep2 (effective-power group2)))
    (if (= ep1 ep2)
        (> (assoc-value 'initiative group1) (assoc-value 'initiative group2))
        (> ep1 ep2))))

(define (attacking-order-cmp group1 group2)
  (> (assoc-value 'initiative group1) (assoc-value 'initiative group2)))

(define (damage-multiplier attacker defender)
  (let ((at (assoc-value 'attack-type attacker)))
    (cond
     ((member at (assoc-value 'immunities defender)) 0)
     ((member at (assoc-value 'weaknesses defender)) 2)
     (#t 1))))

(define (pick-target group targets)
  (let ((me (assoc-value 'army group)))
    (if*
     (reduce (lambda (target best)
               (let ((dm (car target))
                     (bdm (car best)))
                 (if (or (> dm bdm) (and (= dm bdm) (targeting-order-cmp (cdr target) (cdr best))))
                     target
                     best)))
             #f
             (filter-map (lambda (target)
                           (let ((dm (damage-multiplier group target)))
                             (and (< 0 dm) (cons dm target))))
                         (if (string= me "Immune System") (infection targets) (immune-system targets))))
     (cdr it)
     #f)))

(define (attack attacker defender)
  (if *verbose*
      (printf "~a attacks" (assoc-value 'initiative attacker)))
  (let* ((damage (effective-power attacker))
         (dm (damage-multiplier attacker defender))
         (units-killed (min (quotient (* damage dm) (assoc-value 'hit-points defender)) (assoc-value 'count defender))))
    (dec! (cdr (assoc 'count defender)) units-killed)
    (if *verbose* (print " dealing " (* damage dm) " damage, killing " units-killed " units"))
    units-killed))

(define (immune-system groups)
  (filter (lambda (group) (string= (assoc-value 'army group) "Immune System")) groups))

(define (infection groups)
  (filter (lambda (group) (string= (assoc-value 'army group) "Infection")) groups))

(define (desc-army name army)
  (print army)
  (for-each (lambda (g)
              (print "Group " (assoc-value 'initiative g) " contains " (assoc-value 'count g) " units"))
            (army)))

(define (fight boost)
  (loop with groups = (army boost)
        for round from 1
        for targets = (list-copy groups)
        for pairs = '()
        for killed = 0
        do (when *verbose*
             (print '-------------------------------')
             (print "Round " round)
             (desc-army "Immune System:" (immune-system groups))
             (desc-army  "Infection:" (infection groups))
             (newline))
        do (for-each (lambda (group)
                       (let ((target (pick-target group targets)))
                         (when target
                           (if *verbose*
                               (print (assoc-value 'army group) " " (assoc-value 'initiative group)
                                      " (" (effective-power group) ")"
                                      " selects " (assoc-value 'initiative target)))
                           (set! targets (delete! target targets))
                           (push! (cons group target) pairs))))
                     (sort groups targeting-order-cmp))
        do (for-each (lambda (attacker)
                       (if (< 0 (assoc-value 'count attacker))
                           (if* (assoc attacker pairs)
                                (inc! killed (attack attacker (cdr it))))))
                     (sort groups attacking-order-cmp))
        do (set! groups (remove (lambda (group)
                                  (>= 0 (assoc-value 'count group))) groups))
        if *debug* do (print round " " (total-result (immune-system groups)) " " (total-result (infection groups)))
        until (or (null? (immune-system groups)) (null? (infection groups)) (= 0 killed))
        finally (return groups)))

(define (total-result result)
  (fold (lambda (group total)
          (+ total (assoc-value 'count group))) 0 result))

(define (win? boost)
  (let ((result (fight boost)))
    (and (null? (infection result)) (total-result result))))

(print (total-result (fight 0)))

(if *binary-search*
    ; gives incorrect result
    (loop with mb
          with Mb
          initially (loop for m = 1 then M
                          for M = 1 then (* M 2)
                          until (win? M)
                          finally (set! mb m)
                          finally (set! Mb M))
          for mid = (/ (+ mb Mb) 2)
          do (print (- Mb mb))
          do (if (win? mid)
                 (set! Mb mid)
                 (set! mb mid))
          until (= Mb (+ 1 mb))
          finally (print "Loses with " mb ", but wins with " Mb ": " (win? Mb)))
    (loop for boost from 1
          for result = (win? boost)
          until result
          finally (print result)))
