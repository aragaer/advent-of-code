#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :alexandria :silent t)
(ql:quickload :iterate :silent t)
(ql:quickload :split-sequence :silent t)

(defpackage adv07
  (:use :cl
	:alexandria
	:iterate
	:split-sequence))

(in-package #:adv07)

(defvar *circuit* (make-hash-table :test 'equal))

(iter (for line = (read-line *standard-input* nil))
      (while line)
      (for (input (result)) = (split-sequence "->" (split-sequence #\Space line) :test 'string=))
      (setf (gethash result *circuit*) input))

(defvar *circuit2* (copy-hash-table *circuit*))

(declaim (ftype (function (list hash-table) t) calculate))

(defun get-number (term circuit)
  (if-let ((value (gethash term circuit)))
    (if (listp value)
	(setf (gethash term circuit)
	      (calculate value circuit))
      value)
    (parse-integer term)))

(defun shl (x bits)
  (mod (logand (ash x bits) 65535) 65536))
 
(defun shr (x bits)
  (mod (logand (ash x (- bits)) 65535) 65536))

(defvar *ops* '(AND logand OR logior LSHIFT shl RSHIFT shr))

(defun calculate (formula circuit)
  (ecase (length formula)
    (1 (get-number (first formula) circuit))
    (2 (lognot (get-number (second formula) circuit)))
    (3 (destructuring-bind (a1 op a2) formula
	 (setf a1 (get-number a1 circuit))
	 (setf a2 (get-number a2 circuit))
	 (funcall (getf *ops* (intern op)) a1 a2)))))

(format t "~a~%" (setf (gethash "b" *circuit2*) (calculate '("a") *circuit*)))
(format t "~a~%" (calculate '("a") *circuit2*))
