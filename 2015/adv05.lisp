(defun nice-1 (word)
  (and (<= 3 (loop for vowel across "aeiou" summing (count vowel word)))
       (loop for c1 across word
	     for c2 across (subseq word 1)
	     thereis (eql c1 c2))
       (loop for forbidden in '("ab" "cd" "pq" "xy")
	     never (search forbidden word))))

(defun nice-2 (word)
  (and (loop for c1 across word
	     for c2 across (subseq word 2)
	     thereis (eql c1 c2))
       (loop for p from 0 to (- (length word) 2)
	     thereis (search (subseq word p (+ p 2)) (subseq word (+ p 2))))))

(loop for line = (read-line *standard-input* nil :eof)
      until (eq line :eof)
      counting (nice-1 line) into result-1
      counting (nice-2 line) into result-2
      finally (format t "Part 1: ~d~%Part 2: ~d~%" result-1 result-2))
