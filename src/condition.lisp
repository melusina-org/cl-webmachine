;;;; condition.lisp — Conditions for Webmachine

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

(defun status-code-short-description (status-code)
  "The short description associated with STATUS-CODE."
  (case status-code
    (100 "Continue")
    (101 "Switching Protocols")
    (200 "OK")
    (201 "Created")
    (202 "Accepted")
    (203 "Non-Authoritative Information")
    (204 "No Content")
    (205 "Reset Content")
    (206 "Partial Content")
    (300 "Multiple Choices")
    (301 "Moved Permanently")
    (302 "Found")
    (303 "See Other")
    (304 "Not Modified")
    (305 "Use Proxy")
    (306 "(Unused)")
    (307 "Temporary Redirect")
    (400 "Bad Request")
    (401 "Unauthorized")
    (402 "Payment Required")
    (403 "Forbidden")
    (404 "Not Found")
    (405 "Method Not Allowed")
    (406 "Not Acceptable")
    (407 "Proxy Authentication Required")
    (408 "Request Timeout")
    (409 "Conflict")
    (410 "Gone")
    (411 "Length Required")
    (412 "Precondition Failed")
    (413 "Request Entity Too Large")
    (414 "Request-URI Too Long")
    (415 "Unsupported Media Type")
    (416 "Requested Range Not Satisfiable")
    (417 "Expectation Failed")
    (500 "Internal Server Error")
    (501 "Not Implemented")
    (502 "Bad Gateway")
    (503 "Service Unavailable")
    (504 "Gateway Timeout")
    (505 "HTTP Version Not Supported")
    (t "")))

(define-condition http-condition (simple-condition)
  ((status-code
    :initarg :status-code
    :initform (error "A HTTP-Condition reqiures a Status Code.")
    :reader http-condition-status-code))
  (:default-initargs
   :format-control "HTTP Condition")
  (:documentation
   "A HTTP-CONDITION is signalled upon protocol errors.
A protocol error is transmitted to the client and the server
can continue normal operation."))

(define-condition http-error (http-condition simple-error) ()
  (:default-initargs
   :status-code 500
   :format-control "Internal Server Error")
  (:documentation
   "A HTTP-ERROR is signalled upon processing errors.
It indicates to the client that the server encountered an unexpected
condition that prevented it from fulfilling the request."))

(defun http-error (&optional (status-code 500) short-description &rest arguments)
  "Signal a HTTP-ERROR with the given STATUS-CODE and SHORT-DESCRIPTION."
  (error 'http-error
	 :status-code status-code
	 :format-control
	 (or short-description
	     (status-code-short-description status-code))
	 :format-arguments arguments))

(defmethod print-object ((object http-condition) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A: ~?" (http-condition-status-code object)
            (simple-condition-format-control object)
            (simple-condition-format-arguments object))))

(defun http-condition-short-description (http-condition)
  "The short description of HTTP-CONDITION."
  (declare (type http-condition http-condition))
  (format nil "~?"
	  (simple-condition-format-control http-condition)
          (simple-condition-format-arguments http-condition)))

;;;; End of file `condition.lisp'
