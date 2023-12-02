#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :str :silent t)

(defun parse-draw (draw)
  (loop for (i w) in (map 'list #'str:words (str:split ", " draw))
        for c = (parse-integer i)
        if (string= w "red")
          sum c into r
        if (string= w "green")
          sum c into g
        if (string= w "blue")
          sum c into b
        finally (return (list r g b))))

(defun validate-draw (draw limit)
  (loop for x in draw
        for y in limit
        never (> x y)))

(defun validate-game (game limit)
  (loop for draw in game
        always (validate-draw draw limit)))

(defun game-power (game)
  (loop for (r g b) in game
        maximize r into rm
        maximize g into gm
        maximize b into bm
        finally (return (* rm gm bm))))

(defun read-game (line)
  (map 'list #'parse-draw
       (str:split "; "
                  (cadr (str:split ": " line)))))

(loop for line = (read-line *standard-input* nil nil)
      while line
      for game = (read-game line)
      for id from 1
      if (validate-game game '(12 13 14))
        sum id into res1
      sum (game-power game) into res2
      finally (format t "~a~%~a~%" res1 res2))
