;;;; entrypoint.lisp — Entrypoint for Webmachine Example

;;;; Webmachine (https://github.com/melusina-org/cl-webmachine)
;;;; This file is part of Webmachine.
;;;;
;;;; Copyright © 2018–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.webmachine/example)

(defun create-users ()
  (dolist (user-spec 
	   '((:name "Administrator" :email "charlie@melusina.invalid" :password "*" :administrator t)
	     (:name "User" :email "joe@melusina.invalid" :password "*" :administrator nil)))
    (apply #'add-user user-spec)))


(defun start ()
  (server:start
   (list
    (resource 'about)
    (resource 'view-home)
    (resource 'features)
    (resource 'system-health)
    (resource 'example.css)
    (make-user-administration-resource)
    (make-user-resource))
   :swank t))

(defun stop ()
  (server:stop))

(defun toplevel (&optional argv)
  "The toplevel form for the Webmachine example program."
  (labels
      ((usage (&optional (exit-code 0))
	 (format t "Usage: webmacchine-example~% A simple web application.~%")
	 (uiop:quit exit-code))
       (idle-loop ()
	 (loop :do (sleep 300)))
       (run-server ()
	 (restart-case
	     (progn
	       (start)
	       (idle-loop))
	   (abort ()
	     :report
	     (lambda (stream) (write-string "Abort system operation." stream))
	     (uiop:quit))))
       (entrypoint (&optional argv)
	 (multiple-value-bind (options free-args)
	     (if argv
		 (unix-opts:get-opts argv)
		 (unix-opts:get-opts))
	   (declare (ignore options))
	   (cond
	     ((= 0 (length free-args))
	      (run-server))
	     (t
	      (usage 64))))))
    (entrypoint argv)))

;;;; End of file `entrypoint.lisp'
