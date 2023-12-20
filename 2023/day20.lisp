#!/usr/bin/env -S sbcl --noinform --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :alexandria :silent t)
(ql:quickload :str :silent t)

(defparameter *nodes*
  (loop for line = (read-line *standard-input* nil nil)
        with result = (make-hash-table :test #'equal)
        until (str:emptyp line)
        for (this-node rest) = (str:split " -> " line)
        for (node-type name) = (if (string= this-node "broadcaster")
                                   '(broadcast "broadcaster")
                                   `(,(ecase (char this-node 0)
                                        (#\& 'conjunction)
                                        (#\% 'flip-flop))
                                     ,(subseq this-node 1)))
        do (setf (gethash name result)
                 (cons node-type (str:split ", " rest)))
        finally (return result)))

(defun get-inputs (target)
  (loop for k being the hash-keys of *nodes* using (hash-value v)
        if (member target (cdr v) :test #'string=)
          collect k))

(defvar *states* (make-hash-table :test #'equal))

(loop for name being the hash-keys of *nodes* using (hash-value v)
      if (eq (car v) 'conjunction)
        do (setf (gethash name *states*)
                 (let ((this-inputs (make-hash-table :test #'equal)))
                   (dolist (input (get-inputs name) this-inputs)
                     (setf (gethash input this-inputs) 'low)))))

(defun process-pulse (source name pulse)
  (alexandria:if-let ((node-data (gethash name *nodes*)))
    (destructuring-bind (node-type . outputs) node-data
      (let ((new-pulse
              (ecase node-type
                (broadcast pulse)
                (flip-flop (when (eq pulse 'low)
                             (let ((old-state (gethash name *states*)))
                               (setf (gethash name *states*) (not old-state))
                               (if old-state 'low 'high))))
                (conjunction (let ((state (gethash name *states*)))
                               (setf (gethash source state) pulse)
                               (loop for v being the hash-values of state
                                     if (eq v 'low)
                                       do (return 'high)
                                     finally (return 'low)))))))
        (and new-pulse (map 'list (lambda (o)
                                    `(,name ,o . ,new-pulse))
                            outputs))))))

(defun press-button ()
  (loop for pulses = '(("button" "broadcaster" . low)) then (append pulses new-pulses)
        while pulses
        with highs = 0
        with lows = 0
        for (source target . pulse) = (pop pulses)
        for new-pulses = (process-pulse source target pulse)
        if (and (eq pulse 'low) (string= target "rx"))
          do (setf (gethash "rx" *states*) t)
        do (if (eq pulse 'high)
               (incf highs)
               (incf lows))
        finally (return (cons highs lows))))

;; part 1
(loop for i from 1 to 1000
      for (hs . ls) = (press-button)
      sum hs into highs
      sum ls into lows
      finally (format t "~a~%" (* highs lows)))

;; part 2
(loop for node in (cdr (gethash "broadcaster" *nodes*))
      for bits = nil
      do (loop for bit = node then next-bit
               for next-bit = (loop for next in (cdr (gethash bit *nodes*))
                                    for next-type = (car (gethash next *nodes*))
                                    count (eq next-type 'conjunction) into bit-set-p
                                    if (eq next-type 'flip-flop)
                                      collect next into found
                                    finally (push bit-set-p bits)
                                    finally (return (car found)))
               while next-bit)
      collect (reduce (lambda (a b) (+ (* 2 a) b)) bits :initial-value 0) into loops
      finally (format t "~a~%" (apply #'lcm loops)))
