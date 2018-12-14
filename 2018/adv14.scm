(load "common.scm")

(define *all-recipes* '(3 7))
(define *first-elf-recipe* *all-recipes*)
(define *second-elf-recipe* (cdr *all-recipes*))

(define (print-recipes)
  (pair-for-each
   (lambda (recipe)
     (if (equal? recipe *first-elf-recipe*)
         (printf "(~a)" (car recipe))
         (if (equal? recipe *second-elf-recipe*)
             (printf "[~a]" (car recipe))
             (printf " ~a " (car recipe)))))
   *all-recipes*)
  (newline))

(define *limit* 909441)

(define (move-elves)
  (set! *first-elf-recipe* (drop *first-elf-recipe* (+ 1 (car *first-elf-recipe*))))
  (set! *second-elf-recipe* (drop *second-elf-recipe* (+ 1 (car *second-elf-recipe*)))))

(define (calc-recipes)
  (let ((new-score (+ (car *first-elf-recipe*) (car *second-elf-recipe*))))
    (if (> new-score 9)
        (list (quotient new-score 10) (remainder new-score 10))
        (list new-score))))

(loop while (> (+ *limit* 10) added)
      for added = 0 then (+ added new-len)
      with last-recipe = (last-pair *all-recipes*)
      for new-score = (+ (car *first-elf-recipe*) (car *second-elf-recipe*))
      for new-recipes = (calc-recipes)
      for new-len = (length new-recipes)
      do (begin
           (set-cdr! last-recipe new-recipes)
           (set! last-recipe (last-pair new-recipes))
           (set-cdr! last-recipe *all-recipes*)
           (move-elves))
      if (= 0 (modulo added 100000)) do (print added)
      finally (format #t "~{~a~}~%" (take (drop *all-recipes* *limit*) 10)))

(set! *all-recipes* '(3 7))
(set! *first-elf-recipe* *all-recipes*)
(set! *second-elf-recipe* (cdr *all-recipes*))

(define *search-for* (map (o string->number ->string) (string->list "909441")))
(define *search-len* (length *search-for*))

(set! result (- (length *all-recipes*) *search-len*))

(loop until (< 0 result)
      with last-recipe = (last-pair *all-recipes*)
      for new-recipes = (calc-recipes)
      for new-len = (length new-recipes)
      do (begin
           (set-cdr! last-recipe new-recipes)
           (set! last-recipe (last-pair new-recipes))
           (set-cdr! last-recipe *all-recipes*)
           (move-elves))
      do (inc! result new-len)
      finally (set-cdr! last-recipe '()))

(loop with last-recipe = (last-pair *all-recipes*)
      for new-recipes = (calc-recipes)
      for new-len = (length new-recipes)
      for position = (take-right *all-recipes* *search-len*) then (drop position new-len)
      with found = #f
      do (begin
           (set-cdr! last-recipe new-recipes)
           (set! last-recipe (last-pair new-recipes))
           (set-cdr! last-recipe *all-recipes*)
           (move-elves))
      do (set! found (find (lambda (pos)
                             (every = *search-for* (drop position pos)))
                           (range 1 (+ new-len 1))))
      until found
      do (inc! result new-len)
      if (= 0 (modulo result 100000)) do (print result)
      finally (print (+ result found) " " (take (drop position found) *search-len*)))
