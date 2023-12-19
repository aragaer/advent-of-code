#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :alexandria :silent t)
(ql:quickload :cl-ppcre :silent t)
(ql:quickload :str :silent t)

(defmacro parse (string pattern &rest body)
  `(multiple-value-bind (_ it) (cl-ppcre:scan-to-strings ,pattern ,string) ,@body))

(defun process-rule (rule)
  (parse rule "(([xmas])([<>])([0-9]+):)?(.*)"
         (cons (aref it 4) (when (aref it 0)
                             `(,(char (aref it 1) 0)
                               ,(if (eq (char (aref it 2) 0) #\<) #'< #'>)
                               ,(parse-integer (aref it 3)))))))

(defparameter *flows*
  (loop for line = (read-line *standard-input* nil nil)
        with result = (make-hash-table :test #'equal)
        until (str:emptyp line)
        do (parse line "([a-z]+){(.*)}"
                  (setf (gethash (aref it 0) result)
                        (map 'list #'process-rule (str:split #\, (aref it 1)))))
        finally (return result)))

(defun param->idx (param)
  (ecase param (#\x 0) (#\m 1) (#\a 2) (#\s 3)))

(defun split-range (min max test v)
  (cond
    ((and (funcall test min v) (funcall test max v)) `((,min . ,max) nil))
    ((and (not (funcall test min v)) (not (funcall test max v))) `(nil (,min . ,max)))
    ((equal test #'<) `((,min . ,(1- v)) (,v . ,max)))
    (t `((,(1+ v) . ,max) (,min . ,v)))))

(defun process-range (range cond)
  (destructuring-bind (param test value) cond
    (let ((idx (param->idx param)))
      (map 'list
           (lambda (res)
             (when res
               (let ((copy (alexandria:copy-array range)))
                 (setf (aref copy idx 0) (car res)
                       (aref copy idx 1) (cdr res))
                 copy)))
           (split-range (aref range idx 0) (aref range idx 1) test value)))))

(defun range-flow (flow range)
  (loop for (target . cond) in (gethash flow *flows*)
        for my-range = range then fail
        for (pass fail) = (if cond
                              (process-range my-range cond)
                              (list my-range))
        if pass
          collect (cons target pass)
        while fail))

(defun satisfy-p (part cond)
  (destructuring-bind (param test value) cond
    (funcall test (nth (param->idx param) part) value)))

(defun try-part (part)
  (loop for flow = "in" then new-flow
        for flow-rules = (gethash flow *flows*)
        for new-flow = (loop for (target . cond) in flow-rules
                             if (or (not cond) (satisfy-p part cond))
                               do (return target))
        until (member new-flow '("A" "R") :test #'string=)
        finally (return (string= new-flow "A"))))

(defun part1 ()
  (loop for line = (read-line *standard-input* nil nil)
        while line
        for part = (parse line "{x=(.+?),m=(.+?),a=(.+?),s=(.+?)}"
                          (map 'list #'parse-integer it))
        if (try-part part)
          sum (apply #'+ part)))

(defun range->count (range)
  (reduce (lambda (a i)
            (* a (- (aref range i 1) (aref range i 0) -1)))
          '(0 1 2 3) :initial-value 1))

(defparameter *initial-range*
  (make-array '(4 2) :initial-contents (map 'list (lambda (_) '(1 4000)) "xmas")))

(defun part2 ()
  (loop with queue = `(("in" . ,*initial-range*))
        while queue
        for (flow . range) = (pop queue)
        sum (loop for (tgt . new-range) in (range-flow flow range)
                  if (string= tgt "A")
                    sum (range->count new-range)
                  unless (member tgt '("A" "R") :test #'string=)
                    do (push (cons tgt new-range) queue))))

(format t "~a~%~a~%" (part1) (part2))
