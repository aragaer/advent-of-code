#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :str :silent t)

(defvar *seeds*
  (map 'list #'parse-integer
       (cdr (str:words (read-line *standard-input*)))))
(read-line *standard-input*)
(read-line *standard-input*)

(defvar *maps*
  (loop for line = (read-line *standard-input* nil nil)
        for x from 1
        with intervals
        with result
        while line
        do (let ((words (str:words line)))
             (case (list-length words)
               (3
                 (let* ((numbers (map 'list #'parse-integer words))
                        (dst (car numbers))
                        (src (cadr numbers))
                        (len (caddr numbers)))
                   (push `(,src ,(+ src len) ,(- dst src)) intervals)))
               (0
                (push (sort intervals #'< :key #'car) result)
                (setq intervals nil))))
        finally (progn
                  (push (sort intervals #'< :key #'car) result)
                  (return (nreverse result)))))

(defun transform (value map)
  (loop for (start end change) in map
        if (< value start)
          return value
        if (< value end)
          return (+ value change)
        finally (return value)))

(defun interval-change (start end map)
  (loop for (rule-start rule-end rule-change) in map
        if (<= rule-start start end rule-end)
          do (return rule-change)))

(defun merge-maps (map1 map2 &optional in-first-only)
  (let* ((altered1 (loop for (s e c) in map1
                         collect `(,(+ s c) ,(+ e c) ,c)))
         (all-points (remove-duplicates
                      (sort (append
                             (loop for (s e _) in altered1
                                   collect s
                                   collect e)
                             (loop for (s e _) in map2
                                   collect s
                                   collect e))
                            #'<))))
    (loop for s in all-points
          for e in (cdr all-points)
          for in-first = (interval-change s e altered1)
          for in-second = (interval-change s e map2)
          if (or in-first (not in-first-only))
            collect (let ((ns (if in-first (- s in-first) s))
                          (ne (if in-first (- e in-first) e))
                          (nc (+ (or in-first 0) (or in-second 0))))
                      `(,ns ,ne, nc)))))

(let ((merged-map
        (sort
         (loop for map in *maps*
               for result = map then (merge-maps result map)
               finally (return result))
         #'< :key #'car)))
  (format t "~a~%~a~%"
          (loop for seed in *seeds*
                minimize (transform seed merged-map))
          (loop for (start len) on *seeds* by #'cddr
                for end = (+ start len -1)
                for transformed = (merge-maps `((,start ,end 0)) merged-map t)
                for res = (loop for (s _ c) in transformed minimize (+ s c))
                minimize res)))
