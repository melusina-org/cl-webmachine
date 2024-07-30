;;;; utilities.lisp — Utilities for Webmachine tests

;;;; Webmachine (https://github.com/melusina-org/cl-webmachine)
;;;; This file is part of Webmachine.
;;;;
;;;; Copyright © 2018–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:webmachine/testsuite)


;;;;
;;;; Testsuite Acceptor
;;;;

(defparameter *testsuite-acceptor* nil
  "The acceptor used in the testsuite.")

(defun testsuite-acceptor/random-unprivileged-port ()
  (let ((first-unprivilieged-port 1024)
	(first-invalid-port 65536))
    (+ first-unprivilieged-port
       (random (- first-invalid-port first-unprivilieged-port)))))

(defun testsuite-acceptor/server-name ()
  (format nil "Webmachine ~A" confidence:*testsuite-id*))

(defmacro with-testsuite-acceptor ((&rest resources) &body body)
    `(let ((*testsuite-acceptor*
	       (webmachine:make-acceptor
		:resources (list ,@resources)
		:server-name (testsuite-acceptor/server-name)
		:port (testsuite-acceptor/random-unprivileged-port)
		:address "127.0.0.1"))
	   (webmachine::*resource-repository*
	     (make-hash-table)))
       (hunchentoot:start *testsuite-acceptor*)
       (unwind-protect (progn ,@body)
	 (hunchentoot:stop *testsuite-acceptor*))))


;;;;
;;;; Assertions for HTTP Replies
;;;;

(defclass http-reply ()
  ((body :initarg :body)
   (status-code :initarg :status-code)
   (headers :initarg :headers)
   (uri :initarg :uri)
   (stream :initarg :stream)
   (close :initarg :close)
   (reason-phrase :initarg :reason-phrase))
  (:documentation
   "Model HTTP-REPLIES as used by Drakma."))

(defun http-request (acceptor uri-path
                     &rest args
                     &key (protocol :http/1.1)
                          (method :get)
                          content
                          content-type
                          content-length
                          range
                          cookie-jar
                          basic-authorization
                          parameters
                          external-format-out
                          accept
                          additional-headers
                          real-host)
  "Send a HTTP request to an ACCEPTOR and returns its reply.
This uses `DRAKMA:HTTP-REQUEST' to perform the request."
  (declare (ignore protocol method content content-type content-length
		   range cookie-jar basic-authorization parameters
		   external-format-out additional-headers real-host))
  (let ((drakma:*text-content-types*
          '(("text" . nil)
            ("application" . "json")))
	(http-reply
	  (make-instance 'http-reply))
	(actual-url
	  (if (hunchentoot:started-p acceptor)
	      (format nil "http://localhost:~A~A"
		      (hunchentoot:acceptor-port acceptor) uri-path)
	      (error "Cannot send a HTTP request to a halted acceptor."))))
    (with-slots (body status-code headers uri stream close) http-reply
      (setf (values body status-code headers uri stream close)
            (apply 'drakma:http-request actual-url args)))
    (values http-reply)))


;;;
;;; Analyse HTTP Requests and Replies
;;;

(defparameter *http-reply* nil
  "The HTTP-REPLY under scrutiny.")

(defmacro with-http-reply ((uri-path
			    &rest args
			    &key (protocol :http/1.1)
				 (method :get)
				 content
				 content-type
				 content-length
				 range
				 cookie-jar
				 basic-authorization
				 parameters
				 external-format-out
				 accept
				 additional-headers
				 real-host)
			   &body body-forms)
  (declare (ignore protocol method content content-type content-length
		   range cookie-jar basic-authorization parameters
		   external-format-out accept additional-headers real-host))
  `(let ((*http-reply*
	   (http-request *testsuite-acceptor* ,uri-path ,@args)))
     ,@body-forms))

(defun header-value (header-name reply)
  (cdr (assoc header-name (slot-value reply 'headers) :test #'string-equal)))

(defun header-match (value regexp)
  (when value
    (cl-ppcre:scan regexp value)))

(defun write-header-value (stream header-name reply)
  (let ((header-value (header-value header-name reply)))
    (if header-value
        (format stream "~%The header ~A is set to~%~%  ~S" header-name header-value)
        (format stream "~%The header ~A is not set." header-name))))

(define-assertion assert-http-header-undefined (header-name &optional (reply *http-reply*))
  "The assertion (ASSERT-HTTP-HEADER-UNDEFINED HEADER-NAME) is true iff
the header with the name HEADER-NAME is undefined in the last *HTTP-REPLY*."
  :report (lambda (stream)
            (format stream
"The optional argument REPLY can be used to test the header of another reply than
the last *HTTP-REPLY*.~%")
            (write-header-value stream header-name reply))
  (not (header-value header-name reply)))


(define-assertion assert-http-header-match (header-name regexp &optional (reply *http-reply*))
  "The assertion (ASSERT-HTTP-HEADER-MATCH HEADER-NAME REGEXP) is true iff
the header with the name HEADER-NAME is defined in the last *HTTP-REPLY* and
matches the given REGEXP.

The optional argument REPLY can be used to test the header of another reply than
the last *HTTP-REPLY*."
  :report (lambda (stream)
            (format stream "The regular expression used is

  ~S~%" regexp)
            (write-header-value stream header-name reply))
  (header-match (header-value header-name reply) regexp))

(define-assertion assert-http-header-charset (charset &optional (reply *http-reply*))
  "The assertion (ASSERT-HTTP-HEADER-CHARSET CHARSET) is true iff the
header with the name Content-Type is defined in the last *HTTP-REPLY*
and features a charset declaration validating CHARSET.

Currently, only the :utf-8 charset is supported.

The optional argument REPLY can be used to test the header of another
reply than the last *HTTP-REPLY*."
  :report (lambda (stream)
            (format stream "The expected CHARSET is

  ~S

and the CHARSET received in the last response is~%~%  " charset)
	    (write-header-value stream :content-type reply))
  (let ((regexp (ecase charset
                  (:utf-8 ";\\s*charset=(?i)utf-8"))))
    (header-match (header-value :content-type reply) regexp)))


(define-assertion assert-http-body (regexp &optional (reply *http-reply*))
"The assertion (ASSERT-HTTP-BODY REGEXP) is true iff the body response
in the last *HTTP-REPLY* matches the given REGEXP.

The optional argument REPLY can be used to test the header of another
reply than the last *HTTP-REPLY*."
  :report (lambda (stream)
            (format stream
"~&~%  The regular expression used is

  ~S

  and the last response body is

  ~S~%~%" regexp (slot-value reply 'body)))
  (cl-ppcre:scan regexp (slot-value reply 'body)))

(define-assertion assert-http-status (status &optional (reply *http-reply*))
"The assertion (ASSERT-HTTP-STATUS STATUS) is true iff the status response
in the last *HTTP-REPLY* equals the given STATUS or is in the list STATUS.

The optional argument REPLY can be used to test the header of another
reply than the last *HTTP-REPLY*."
  :report (lambda (stream)
            (format stream
"~&~%  The status used is

  ~S

  and the last response status is

  ~S~%~%" status  (slot-value reply 'status-code)))
  (if (listp status)
      (position (slot-value reply 'status-code) status)
      (= (slot-value reply 'status-code) status)))


;;;;
;;;; Testing STRING-MATCH
;;;;

(define-testcase testsuite-string-match ()
  (assert-t (webmachine::string-match "" ""))
  (assert-t (webmachine::string-match "a" "a"))
  (assert-t (webmachine::string-match "a*" "a"))
  (assert-t (webmachine::string-match "a*" "ab"))
  (assert-t (webmachine::string-match "a*" "abc"))
  (assert-t (webmachine::string-match "a?" "ab"))
  (assert-t (webmachine::string-match "a*a" "aba"))
  (assert-t (webmachine::string-match "a*a" "abca"))
  (assert-t (webmachine::string-match "a?a" "aba"))
  (assert-nil (webmachine::string-match "*a" "b"))
  (let ((pattern
	  "*The parameter STRING1 is expected to have type STRING but actually has type*")
	(text
	  #.(concatenate 'string
			 "The assertion (ASSERT-STRING= STRING1 STRING2) is true, iff STRING1 "
			 "and STRING2" '(#\Newline) "satisfy the STRING= predicate." '(#\Newline)
			 "This assertion supports the same keyword parameters as STRING=."
			 '(#\Newline) "The parameter STRING1 is expected to have type STRING "
			 "but actually has type BOOLEAN.")))
    (assert-t (webmachine::string-match pattern text))))

(define-testcase testsuite-utilities ()
  (testsuite-string-match))

;;;; End of file `utilities.lisp'
