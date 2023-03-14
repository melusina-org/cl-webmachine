;;;; acceptor.lisp — Acceptor for Webmachine

;;;; Webmachine (https://github.com/melusina-org/cl-webmachine)
;;;; This file is part of Webmachine.
;;;;
;;;; Copyright © 2018–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.webmachine)


;;;
;;; Acceptor
;;;

(defclass acceptor (hunchentoot:acceptor)
  ((resources
    :initarg :resources
    :documentation "The RESOURCES served by the server.")
   (server-name
    :initarg :server-name
    :documentation "The server name to return in Server headers."))
  (:default-initargs
   :request-class 'request
   :reply-class 'reply
   :document-root nil
   :server-name "Webmachine"
   :resources nil
   :error-template-directory nil)
  (:documentation
   "This is the acceptor of the Webmachine framework."))

(defun make-acceptor (&rest initargs
		      &key resources server-name message-log-destination access-log-destination
			   listen-backlog write-timeout read-timeout persistent-conntections-p
			   input-chunking-p output-chunking-p taskmaster
			   reply-class request class name address port)
  "Make a Webmachine acceptor."
  (declare (ignore resources server-name message-log-destination access-log-destination
		   listen-backlog write-timeout read-timeout persistent-conntections-p
		   input-chunking-p output-chunking-p taskmaster
		   reply-class request class name address port))
  (apply #'make-instance 'acceptor initargs))

(defmethod describe-object ((instance acceptor) stream)
  (format stream "~&~A is a Webmachine acceptor of type ~A."
	  instance (type-of instance))
  (format stream "~&  Status: ~A"
	  (if (hunchentoot:started-p instance) "Started" "Halted"))
  (format stream "~&  SSL: ~A"
	  (hunchentoot:acceptor-ssl-p instance))
  (when (hunchentoot:acceptor-ssl-p instance)
    (format stream "~&  SSL Certificate File: ~A"
	    (hunchentoot:acceptor-ssl-certificate-file instance))
    (format stream "~&  SSL Private Key File: ~A"
	    (hunchentoot:acceptor-ssl-privatekey-file instance)))
  (format stream "~&  Port: ~A"
	  (hunchentoot:acceptor-port instance))
  (format stream "~&  Address: ~A"
	  (hunchentoot:acceptor-address instance))
  (format stream "~&  Requests in Progress: ~A"
	  (hunchentoot:acceptor-requests-in-progress instance))
  (format stream "~&  Listen Backlog: ~A (~A% full)"
	  (hunchentoot:acceptor-listen-backlog instance)
	  (round (/ (* 100 (hunchentoot:acceptor-requests-in-progress instance))
		    (hunchentoot:acceptor-listen-backlog instance))))
  (format stream "~&  Read Timeout: ~A"
	  (hunchentoot:acceptor-read-timeout instance))
  (format stream "~&  Write Timeout: ~A"
	  (hunchentoot:acceptor-write-timeout instance))
  (format stream "~&  Output Chunking: ~A"
	  (hunchentoot:acceptor-output-chunking-p instance))
  (format stream "~&  Input Chunking: ~A"
	  (hunchentoot:acceptor-input-chunking-p instance))
  (format stream "~&  Persistent Connections: ~A"
	  (hunchentoot:acceptor-persistent-connections-p instance))
  (values))

#-:hunchentoot-no-ssl
(defclass ssl-acceptor (acceptor hunchentoot:ssl-acceptor)
  ()
  (:documentation
   "This is an acceptor that accepts SSL connections."))

(defmethod hunchentoot::acceptor-server-name ((instance acceptor))
  (slot-value instance 'server-name))

(defmethod hunchentoot:acceptor-dispatch-request ((instance acceptor) request)
  "The ``Webmachine'' dispatcher selects service to handle a request."
  ;; The function has basic error handling which could be improved on by
  ;; using HANDLER-BIND unstead of HANDLER-CASE. That's however slightly
  ;; more technical than HANDLER-CASE because the mix of interactive restarts,
  ;; multi-threads environment and how Hunchentoot itself handles errors.
  (flet ((resource-handle-request-p (resource request)
           (multiple-value-bind (handle-p path-parameters)
               (match-path (slot-value resource 'path)
                           (slot-value request 'hunchentoot:script-name))
             (when handle-p
               (setf (slot-value request 'path-parameters) path-parameters)
               (values t)))))
    (handler-case
        (loop :for resource :in (slot-value instance 'resources)
              :when (resource-handle-request-p resource request)
              :do (return (resource-handle-request resource request hunchentoot::*reply*))
              :finally (http-error 404))
      (http-condition (c)
        (setf (hunchentoot:return-code*) (http-condition-status-code c))
        (setf (hunchentoot:content-type*) "text/plain; charset=utf-8")
	(hunchentoot:abort-request-handler (http-condition-short-description c))))))

;;;; End of file `acceptor.lisp'
