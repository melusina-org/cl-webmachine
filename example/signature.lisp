;;;; signature.lisp — Signature for Web Apps and APIs

;;;; Webmachine (https://github.com/melusina-org/cl-webmachine)
;;;; This file is part of Webmachine.
;;;;
;;;; Copyright © 2018–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.webmachine/signature
  (:use #:cl #:parenscript)
  (:local-nicknames
   (#:webmachine #:org.melusina.webmachine)
   (#:who #:cl-who))
  (:export
   #:verify-hmac-authorization-trait
   #:find-signature-key
   #:compute-signature
   #:testsuite-javascript
   #:signature-javascript
   #:testsuite-signature-javascript))

(in-package #:org.melusina.webmachine/signature)

(defparameter *signature-algorithm*
  '(:external-format :utf-8
    :mac :hmac
    :digest :sha256
    :name "MELUSINA1"
    :request-date-time :request-date-time)
  "Parameters used in signature algorithms.")

(defun write-signature-algorithm (stream)
  "Write signature algorithm associated with *SIGNATURE-ALGORITHM* to STREAM."
  (loop :for detail :in '(:name :mac :digest)
	:for separator = "" :then "-"
	:do (write-string separator stream)
	:do (write-string (string-upcase (getf *signature-algorithm* detail)) stream)))

(defun write-authorization-scope (stream &key request-date (region "global") service identity)
  (flet ((scope-signature-method ()
	   (string-downcase
	    (concatenate 'string (getf *signature-algorithm* :name) "_request")))
	 (scope-request-day (request-date)
	   (subseq request-date 0 8))
	 (scope-details (&rest details)
	   (if identity
	       (cons identity details)
	       details)))
    (loop :for detail :in (scope-details
			   (scope-request-day request-date)
			   (string-downcase region)
			   (string-downcase service)
			   (scope-signature-method))
	  :for separator = "" :then "/"
	  :do (write-string separator stream)
	  :do (write-string detail stream))))

(defun ensure-octets (data)
  "Ensure that data is rendered as octets."
  (if (stringp data)
      (flexi-streams:string-to-octets
       data
       :external-format (getf *signature-algorithm* :external-format))
      data))

(defun hashed-payload (payload)
  "Compute the hash of a PAYLOAD."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    (getf *signature-algorithm* :digest)
    (ensure-octets payload))))

(defun canonical-headers (signed-headers)
  "Sort SIGNED-HEADERS."
  (flet ((canonical-header (assoc)
	   (destructuring-bind (name . value) assoc
	     (cons (string-downcase name)
		   (string-trim '(#\Space) value)))))
    (sort (mapcar #'canonical-header signed-headers) #'string<=
	  :key #'car)))

(defun create-canonical-request (&key request-method (canonical-uri "/") (canonical-query-string "")
				      signed-headers hashed-payload)
  "Create a canonical request for signing."
  (flet ((write-request-method (request-method stream)
	   (write-line
	    (webmachine:request-method-name
	     (webmachine:find-request-method request-method))
	    stream))
	 (write-header (name value stream)
	   (write-string name stream)
	   (write-char #\Colon stream)
	   (write-string value stream)
	   (write-char #\Newline stream)))
    (setf signed-headers
	  (canonical-headers signed-headers))
    (with-output-to-string (stream)
      (write-request-method request-method stream)
      (write-line canonical-uri stream)
      (write-line canonical-query-string stream)
      (loop :for (name . value) :in signed-headers
	    :do (write-header name value stream))
      (write-char #\Newline stream)
      (loop :for (name . _) :in signed-headers
	    :for prefix = "" :then ";"
	    :do (write-string prefix stream)
	    :do (write-string name stream))
      (write-char #\Newline stream)
      (write-string hashed-payload stream))))

(defun create-string-to-sign (&key request-date (region "global") service hashed-canonical-request)
  "Create a final string for signing."
  (with-output-to-string (stream)
    (write-signature-algorithm stream)
    (write-char #\Newline stream)
    (write-line request-date stream)
    (write-authorization-scope stream :request-date request-date :region region :service service)
    (write-char #\Newline stream)
    (write-string hashed-canonical-request stream)))

(defun compute-signature (&key key request-date (region "global") service string-to-sign)
  "Compute the cryptographic signature for a request scope."
  (assert (eq :hmac (getf *signature-algorithm* :mac)))
  (flet ((signature-request-day (request-date)
	   (subseq request-date 0 8))
	 (signature-method ()
	   (string-downcase
	    (concatenate 'string (getf *signature-algorithm* :name) "_request")))
	 (hmac (key data)
	   (let ((hmac
		   (ironclad:make-hmac (ensure-octets key) (getf *signature-algorithm* :digest))))
	     (ironclad:update-hmac hmac (ensure-octets data))
	     (ironclad:hmac-digest hmac)))
	 (initial-value (key)
	   (concatenate 'string (string-upcase (getf *signature-algorithm* :name)) key))
	 (hexadecimal (octets)
	   (ironclad:byte-array-to-hex-string octets)))
    (hexadecimal
     (reduce #'hmac (list (signature-request-day request-date)
			  (string-downcase region)
			  (string-downcase service)
			  (signature-method)
			  string-to-sign)
	     :initial-value (initial-value key)))))

(defun authorization (&key identity request-date (region "global") service signed-headers signature)
  "The authorization header."
  (setf signed-headers (canonical-headers signed-headers))
  (with-output-to-string (stream)
    (write-signature-algorithm stream)
    (write-string " Credential=" stream)
    (write-authorization-scope stream
     :identity identity :request-date request-date :service service :region region)
    (write-string ", Signed-Headers=" stream)
    (loop :for (name . _) :in signed-headers
	  :for separator = "" :then ";"
	  :do (write-string separator stream)
	  :do (write-string name stream))
    (write-string ", Signature=" stream)
    (write-string signature stream)))

(defun parse-authorization (authorization &key (start 0) (end (length authorization)))
  "Parse AUTHORIZATION-HEADER.
It returns the algorithm, the credential, the list of signed headers and the signature
as multiple values."
  (declare ((or null string) authorization))
  (unless authorization
    (return-from parse-authorization nil))
  (multiple-value-bind (match-start match-end register-start register-end)
      (ppcre:scan "^([^ ]+) +Credential=([^,]+), *Signed-Headers=([^,]+), *Signature=([0-9a-f]+)$"
		  authorization :start start :end end)
    (declare (ignore match-end))
    (when match-start
      (flet ((register (n)
	       (subseq authorization (aref register-start n) (aref register-end n)))
	     (authorization-scope (&key start end)
	       (destructuring-bind (identity request-day region service signature-method)
		   (ppcre:split "/" authorization :start start :end end)
		 (list
		  :identity identity
		  :request-day request-day
		  :region region
		  :service service
		  :signature-method signature-method)))
	     (signed-headers (&key start end)
	       (ppcre:split ";" authorization :start start :end end))
	     )
	(values
	 (register 0)
	 (authorization-scope :start (aref register-start 1) :end (aref register-end 1))
	 (signed-headers :start (aref register-start 2) :end (aref register-end 2))
	 (register 3))))))

(defun verify-signature (request mandatory-headers key)
  "Verify the signature for REQUEST using KEY.
When KEY is a function, it is expected to return the actual key when called
with the identity provided by the request.

The verification either returns T when it succesful or NIL together with
a reason as second value."
  (check-type request webmachine:request)
  (multiple-value-bind (algorithm authorization-scope signed-headers signature)
      (parse-authorization (hunchentoot:header-in :authorization request))
    (flet ((ensure-that (predicate reason)
	     (unless (funcall predicate)
	       (return-from verify-signature
		 (values nil reason))))
	   (request-bears-a-request-date-time-header ()
	     (hunchentoot:header-in (getf *signature-algorithm* :request-date-time) request))
	   (request-bears-an-authorization-header ()
	     (hunchentoot:header-in :authorization request))
	   (authorization-header-is-well-formed ()
	     algorithm)
	   (authorization-scopes-are-present ()
	     (flet ((authorization-scope-is-defined (indicator)
		      (getf authorization-scope indicator)))
	       (every #'authorization-scope-is-defined
		      '(:identity :request-day :region :service :signature-method))))
	   (signature-algorithm-is-known ()
	     (string=
	      algorithm
	      (with-output-to-string (buffer)
		(write-signature-algorithm buffer))))
	   (identity-is-known ()
	     (cond
	       ((stringp key)
		t)
	       ((functionp key)
		(setf key (funcall key (getf authorization-scope :identity))))
	       (t
		nil)))
	   (mandatory-headers-are-signed ()
	     (flet ((header-is-signed (header-name)
		      (find header-name signed-headers :test #'string-equal)))
	       (every #'header-is-signed mandatory-headers)))
	   (mandatory-headers-are-present ()
	     (flet ((header-is-present (header-name)
		      (hunchentoot:header-in header-name request)))
	       (every #'header-is-present mandatory-headers)))
	   (signature-is-valid ()
	     (let ((request-date-time
		     (hunchentoot:header-in*
		      (getf *signature-algorithm* :request-date-time)
		      request))
		   (request-day
		     (getf authorization-scope :request-day))
		   (region
		     (getf authorization-scope :region))
		   (service
		     (getf authorization-scope :service)))
	       (flet ((string-to-sign (hashed-canonical-request)
			(create-string-to-sign
			 :request-date request-date-time
			 :region region
			 :service service
			 :hashed-canonical-request hashed-canonical-request))
		      (hashed-canonical-request ()
			(hashed-payload
			 (create-canonical-request
			  :request-method (hunchentoot:request-method request)
			  :canonical-uri (hunchentoot:request-uri request)
			  :signed-headers
			  (loop :for name :in signed-headers
				:collect (cons name (hunchentoot:header-in name request)))
			  :hashed-payload
			  (hashed-payload
			   (or (hunchentoot:raw-post-data :request request :force-binary t)
			       ""))))))
		 (string=
		  signature
		  (compute-signature
		   :key key
		   :request-date request-day
		   :region region
		   :service service
		   :string-to-sign (string-to-sign (hashed-canonical-request))))))))
      (ensure-that
       #'request-bears-a-request-date-time-header
       "The request does not bear a header carrying the request date and time.")
      (ensure-that
       #'request-bears-an-authorization-header
       "The request does not bear an Authorization header.")
      (ensure-that
       #'authorization-header-is-well-formed
       "The Authorization header beared by the request is not well-formed.")
      (ensure-that
       #'authorization-scopes-are-present
       "The Authorization header misses some authorization scope.")
      (ensure-that
       #'signature-algorithm-is-known
       "The Authorization header mentions an unkown signature algorithm.")
      (ensure-that
       #'identity-is-known
       "The Authorization header mentions an unknown identity.")
      (ensure-that
       #'mandatory-headers-are-signed
       "The Authorization header misses some mandatory signed headers.")
      (ensure-that
       #'mandatory-headers-are-present
       "The request misses some mandatory signed headers.")
      (ensure-that
       #'signature-is-valid
       "The Authorization header bears an invalid signature.")
      (values t))))

(defun testsuite-javascript-1 ()
  (who:with-html-output (*standard-output*)
    (:template
     :id "testcase-result"
     (:details
      (:summary
       (:code
	(:slot :name "outcome" "Testcase Outcome is missing")
	"—"
	(:slot :name "name" "Testcase Name is missing")))
      (:h5 "Expected")
      (:code (:slot :name "expected" "Testcase Expected is missing"))
      (:h5 "Received")
      (:code (:slot :name "expected" "Testcase Received is missing"))))
    (:div :id "testsuite-result")
    (:script
     :id "testsuite-javascript"
     :type "text/javascript"
     (who:str
      (ps
	(var -testcase-result
	     (lambda (&key name outcome expected received)
	       (chain -h-t-m-l-element (call this))
	       (setf (getprop this 'name) name)
	       (setf (getprop this 'outcome) outcome)
	       (setf (getprop this 'expected) expected)
	       (setf (getprop this 'received) received)
	       (var template
		      (chain document
			     (get-element-by-id "testcase-result")
			     content))
	       (var shadow-root
		      (chain this
			     (attach-shadow (create :mode "open"))))
	       (chain shadow-root
		      (append-child (chain template (clone-node true))))
	       (debug (create 'name 'testcase-result 'detail this))
	       this))
	(setf (getprop -testcase-result 'prototype)
	      (chain -object (create -h-t-m-l-element)))
	(setf (chain -testcase-result prototype constructor)
	      -testcase-result)
	(chain custom-elements
	       (define "testcase-result" -testcase-result)))))
    (:testcase-result :name "EXAMPLE")))

(defun testsuite-javascript ()
  (who:with-html-output (*standard-output*)
    (:div :id "testsuite-result")
    (:script
     :id "testsuite-javascript"
     :type "text/javascript"
     (who:str
      (ps
	(defun testcase-result (&key name outcome expected received)
	  (flet ((append-text-node (element text)
		   (let ((text-node
			   (chain document (create-text-node text))))
		     (chain element (append text-node))))
		 (make-span (text &optional (name "span"))
		   (let ((element-span
			   (chain document (create-element name)))
			 (text-node
			   (chain document (create-text-node text))))
		     (chain element-span (append text-node))
		     element-span))
		 (make-preformatted-text (name text)
		   (let ((container
			   (chain document (create-element "div")))
			 (element-span
			   (make-span name "h4"))
			 (element-pre
			   (chain document (create-element "pre")))
			 (element-code
			   (chain document (create-element "code")))
			 (text-node
			   (chain document (create-text-node text))))
		     (chain element-code (append text-node))
		     (chain element-pre (append element-code))
		     (chain container (append element-span))
		     (chain container (append element-pre))
		     container)))
	    (let ((display-result
		    (chain document (create-element "div")))
		  (display-details
		    (chain document (create-element "details")))
		  (display-summary
		    (chain document (create-element "summary")))
		  (display-name
		    (make-span (string-upcase (+ name " — " outcome)) "span"))
		  (display-expected
		    (make-preformatted-text "Expected" expected))
		  (display-received
		    (make-preformatted-text "Received" received)))
	      (chain display-summary (append display-name))
	      (loop :for element :in (list display-summary display-expected display-received)
		    :do (chain display-details (append element)))
	      (setf (getprop display-result 'class-name)
		    (case outcome
		      (:success
		       "alert alert-success")
		      (:failure
		       "alert alert-danger")))
	      (setf (getprop display-name 'class-name) "h4")
	      (setf (getprop display-expected 'class-name) "mt-2")
	      (chain display-result (append display-details))
	      (chain document
		     (get-element-by-id "testcase-result")
		     (append display-result))))))))
    (:div :id "testcase-result")))
  

(defun signature-javascript ()
  (who:with-html-output (*standard-output*)
    (:script
     :id "signature-javascript"
     :type "text/javascript"
     (who:str
      (ps
	(defparameter *signature-algorithm*
	  (create 'external-format :utf-8
		  'mac :hmac
		  'digest :sha256
		  'name "MELUSINA1"
		  'request-date-time "request-date-time"))
	(defun assert (boolean)
	  "Ensure that BOOLEAN is true."
	  (unless boolean
	    (error "Assertion failed.")))
	(defun string-downcase (string)
	  (chain string (to-lower-case)))
	(defun string-upcase (string)
	  (chain string (to-upper-case)))
	(defun reduce (@function sequence &key initial-value)
	  (loop :with state = initial-value
		:for transition :in sequence
		:do (setf state (funcall @function state transition))
		:finally (return state)))
	(defun compute-signature (&key key request-date (region "global") service string-to-sign)
	  "Compute the cryptographic signature for a request scope."
	  (assert (eq :hmac (getprop *signature-algorithm* 'mac)))
	  (assert (eq :sha256 (getprop *signature-algorithm* 'digest)))
	  (let ((external-format
		  (new (-text-encoder (getprop *signature-algorithm* 'external-format)))))
	    (labels ((encode (data)
		       (chain external-format (encode data)))
		     (signature-request-day (request-date)
		       (chain request-date (substr 0 8)))
		     (signature-method ()
		       (string-downcase
			(concatenate 'string (getprop *signature-algorithm* 'name) "_request")))
		     (hmac (key data)
		       (flet ((make-hmac (key digest)
				(assert (eq :sha256 digest))
				(chain window crypto subtle
				       (import-key "raw" key
						   (create 'name "HMAC"
							   'hash "SHA-256")
						   false
						   (list "sign"))))
			      (sign (key)
				(chain window crypto subtle
				       (sign "HMAC" key (encode data))))
			      (entrypoint (key)
				(chain (make-hmac key
						  (getprop *signature-algorithm* 'digest))
				       (then #'sign))))
			 (if (instanceof key -promise)
			     (chain key (then #'entrypoint))
			     (entrypoint key))))
		     (initial-value (key)
		       (encode
			(concatenate 'string
				     (string-upcase
				      (getprop *signature-algorithm* 'name))
				     key)))
		     (hexadecimal (octets)
		       (chain -array prototype map
			      (call (new (-uint8-array octets))
				    (lambda (x)
				      (chain x (to-string 16) (pad-start 2 "0"))))
			      (join ""))))
	      (chain
	       (reduce #'hmac (list (signature-request-day request-date)
				    (string-downcase region)
				    (string-downcase service)
				    (signature-method)
				    string-to-sign)
		       :initial-value (initial-value key))
	       (then #'hexadecimal)))))
	(defun make-buffer ()
	  (create 'contents ""))
	(defun buffer-contents (buffer)
	  (getprop buffer 'contents))
	(defun (setf buffer-contents) (new-value buffer)
	  (setf (getprop buffer 'contents) new-value))
  	(defun write-string (string buffer)
	  (setf (buffer-contents buffer)
		(+ (buffer-contents buffer) string)))
  	(defun write-char (char buffer)
	  (setf (buffer-contents buffer)
		(+ (buffer-contents buffer) char)))
 	(defun write-line (string buffer)
	  (setf (buffer-contents buffer)
		(+ (buffer-contents buffer) string #\Newline)))
	(defun write-newline (buffer)
	  (write-string #\Newline buffer))
	(defun write-signature-algorithm (buffer)
	  "Write signature algorithm associated with *SIGNATURE-ALGORITHM* to BUFFER."
	  (loop :for detail :in '(:name :mac :digest)
		:for separator = "" :then "-"
		:do (write-string separator buffer)
		:do (write-string (string-upcase (getprop *signature-algorithm* detail)) buffer)))
	(defun write-authorization-scope (buffer &key request-date (region "global") service identity)
	  (flet ((scope-signature-method ()
		   (string-downcase
		    (concatenate 'string (getprop *signature-algorithm* 'name) "_request")))
		 (scope-request-day (request-date)
		   (chain request-date (substr 0 8)))
		 (scope-details (&rest details)
		   (when identity
		     (chain details (unshift identity)))
		   details))
	    (loop :for detail :in (scope-details
				   (scope-request-day request-date)
				   (string-downcase region)
				   (string-downcase service)
				   (scope-signature-method))
		  :for separator = "" :then "/"
		  :do (write-string separator buffer)
		  :do (write-string detail buffer))))
	(defun canonical-headers (signed-headers)
	  "Sort SIGNED-HEADERS."
	  (labels ((canonical-header (assoc)
		     (destructuring-bind (name value) assoc
		       (list (string-downcase name)
			     (chain (new (-string value)) (trim)))))
		   (compare-header (assoc1 assoc2)
		     (let ((header1
			     (elt assoc1 0))
			   (header2
			     (elt assoc2 0)))
		       (cond
			 ((< header1 header2)
			  -1)
			 ((> header1 header2)
			  1)
			 (t
			  0))))
		   (sort-by-header (headers)
		     (chain headers (sort compare-header))
		     headers))
	    (sort-by-header
	     (loop :for assoc :in signed-headers
		   :collect (canonical-header assoc)))))
	(defun create-string-to-sign (&key request-date (region "global") service hashed-canonical-request)
	  "Create a final string for signing."
	  (let ((buffer
		  (make-buffer)))
	    (write-signature-algorithm buffer)
	    (write-newline buffer)
	    (write-line request-date buffer)
	    (write-authorization-scope buffer :request-date request-date :region region :service service)
	    (write-newline buffer)
	    (write-string hashed-canonical-request buffer)
	    (buffer-contents buffer)))
	(defun create-canonical-request (&key request-method (canonical-uri "/") (canonical-query-string "")
					      signed-headers hashed-payload)
	  "Create a canonical request for signing."
	  (flet ((write-request-method (request-method buffer)
		   (write-line (string-upcase request-method) buffer))
		 (write-header (name value buffer)
		   (write-string name buffer)
		   (write-char #\Colon buffer)
		   (write-string value buffer)
		   (write-char #\Newline buffer)))
	    (setf signed-headers
		  (canonical-headers (chain -object (entries signed-headers))))
	    (let ((buffer
		    (make-buffer)))
	      (write-request-method request-method buffer)
	      (write-line canonical-uri buffer)
	      (write-line canonical-query-string buffer)
	      (loop :for (name value) :in signed-headers
		    :do (write-header name value buffer))
	      (write-char #\Newline buffer)
	      (loop :for (name _) :in signed-headers
		    :for prefix = "" :then ";"
		    :do (write-string prefix buffer)
		    :do (write-string name buffer))
	      (write-char #\Newline buffer)
	      (write-string hashed-payload buffer)
	      (buffer-contents buffer))))
	(defun hash-payload (text)
	  (assert (eq :sha256 (getprop *signature-algorithm* 'digest)))
	  (let ((external-format
		  (new (-text-encoder (getprop *signature-algorithm* 'external-format)))))
	    (labels ((encode (data)
		       (chain external-format (encode data)))
		     (hexadecimal (octets)
		       (chain -array prototype map
			      (call (new (-uint8-array octets))
				    (lambda (x)
				      (chain x (to-string 16) (pad-start 2 "0"))))
			      (join "")))
		     (entrypoint (text)
		       (chain window crypto subtle
			      (digest "SHA-256" (encode text))
			      (then #'hexadecimal))))
	      (if (instanceof text -promise)
		  (chain text (then #'entrypoint))
		  (entrypoint text)))))
	(defun authorization (&key identity request-date (region "global") service signed-headers signature)
	  "The authorization header."
	  (setf signed-headers
		(canonical-headers (chain -object (entries signed-headers))))
	  (let ((buffer (make-buffer)))
	    (write-signature-algorithm buffer)
	    (write-string " Credential=" buffer)
	    (write-authorization-scope buffer
              :identity identity :request-date request-date :service service :region region)
	    (write-string ", Signed-Headers=" buffer)
	    (loop :for (name _) :in signed-headers
		  :for separator = "" :then ";"
		  :do (write-string separator buffer)
		  :do (write-string name buffer))
	    (write-string ", Signature=" buffer)
	    (write-string signature buffer)
	    (buffer-contents buffer)))
	(defun sign-request (&key request request-date-time (region "global") service signed-headers
				  identity key)
	  (unless request-date-time
	    (setf request-date-time
		  (chain (new (-date))
			 (to-i-s-o-string)
			 (replace-all "-" "")
			 (replace-all ":" "")
			 (replace-all "." ""))))
	  (unless (getprop signed-headers (getprop *signature-algorithm* 'request-date-time))
	    (setf (getprop signed-headers (getprop *signature-algorithm* 'request-date-time))
		  request-date-time))
	  (labels ((add-headers-to-request ()
		     (loop :for (name value) :in (chain -object (entries signed-headers))
			   :do (chain request headers (set name value))))
		   (canonical-uri (request)
		     (let ((url
			     (new (-u-r-l (getprop request 'url)))))
		       (getprop url 'pathname)))
		   (canonical-request (hashed-payload)
		       (create-canonical-request
			:request-method (getprop request 'method)
			:canonical-uri (canonical-uri request)
			:canonical-query-string ""
			:signed-headers signed-headers
			:hashed-payload hashed-payload))
		   (string-to-sign (hashed-canonical-request)
		     (create-string-to-sign
		      :request-date request-date-time
		      :region region
		      :service service
		      :hashed-canonical-request hashed-canonical-request))
		   (make-signature (string-to-sign)
		     (compute-signature
		      :key key
		      :request-date request-date-time
		      :region region
		      :service service
		      :string-to-sign string-to-sign))
		   (add-signature (signature)
		     (chain request headers
			    (set "Authorization"
				 (authorization
				  :identity identity
				  :request-date request-date-time
				  :region region
				  :service service
				  :signed-headers signed-headers
				  :signature signature)))
		     request)
		   (debug-trace (identifier)
		     (lambda (value)
		       (unless (chain request signature)
			 (setf (chain request signature) (create)))
		       (setf (getprop (chain request signature) identifier) value)
		       value)))
	    (add-headers-to-request)
	    (chain (hash-payload (chain request (text)))
		   (then #'canonical-request)
		   (then (debug-trace 'canonical-request))
		   (then #'hash-payload)
		   (then (debug-trace 'hashed-canonical-request))
		   (then #'string-to-sign)
		   (then (debug-trace 'string-to-sign))
		   (then #'make-signature)
		   (then (debug-trace 'make-signature))		   
		   (then #'add-signature)))))))))

(defun testsuite-signature-javascript ()
  (who:with-html-output (*standard-output*)
    (:script
     :id "testsuite-signature-javascript"
     :type "text/javascript"
     (who:str
      (ps
	(defun testsuite-signature ()
	  (let ((*signature-algorithm*
		  (create 'external-format :utf-8
			  'mac :hmac
			  'digest :sha256
			  'name "AWS4")))
	    (flet ((assert-string= (&key name expected received)
		     (flet ((entrypoint (received)
			      (testcase-result :name name
					       :outcome (if (= expected received) :success :failure)
					       :expected expected
					       :received received)))
		       (if (instanceof received -promise)
			   (chain received (then #'entrypoint))
			   (entrypoint received)))))
	      (assert-string=
	       :name "CREATE-CANONICAL-REQUEST"
	       :expected "GET
/api/v0/testsuite

host:localhost
x-melusina-1:AnExampleValueForASignedHeader
x-request-date-time:20180527T093358Z

host;x-melusina-1;x-request-date-time
e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
	       :received
	       (create-canonical-request
		:request-method :get
		:canonical-uri "/api/v0/testsuite"
		:signed-headers
		(create
		 :x-melusina-1 "AnExampleValueForASignedHeader"
		 :x-request-date-time "20180527T093358Z"
		 :host "localhost")
		:hashed-payload "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
	      (assert-string=
	       :name "CREATE-STRING-TO-SIGN"
	       :expected    "AWS4-HMAC-SHA256
20180527T093358Z
20180527/global/testsuite/aws4_request
e21858f044b2d96960e70f815b2436183ba88084d94c0fc824e95694e7afe2c7"
	       :received
	       (create-string-to-sign
		:request-date "20180527T093358Z"
		:region :global
		:service :testsuite
		:hashed-canonical-request "e21858f044b2d96960e70f815b2436183ba88084d94c0fc824e95694e7afe2c7"))
	      (assert-string=
	       :name "COMPUTE-SIGNATURE"
	       :expected "5c4446f0df8e87814c774517f95d970b46dbf25d9279b6b8b418b779f817ef5e"
	       :received
	       (compute-signature
		:key "ThisIsNotAnActualSecretAccessKey"
		:request-date "20180527T093358Z"
		:region :global
		:service :testsuite
		:string-to-sign "AWS4-HMAC-SHA256
20180527T093358Z
20180527/global/testsuite/aws4_request
e21858f044b2d96960e70f815b2436183ba88084d94c0fc824e95694e7afe2c7"))
	      (assert-string=
	       :name "HASH-PAYLOAD"
	       :expected "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
	       :received
	       (hash-payload ""))
	      (assert-string=
	       :name "AUTHORIZATION"
	       :expected
	       #.(concatenate
		  'string
		  "AWS4-HMAC-SHA256 "
		  "Credential=ThisIsNotARealIdentity/20180527/global/testsuite/aws4_request, "
		  "Signed-Headers=host, "
		  "Signature=5c4446f0df8e87814c774517f95d970b46dbf25d9279b6b8b418b779f817ef5e")
	       :received
	       (authorization
		:identity "ThisIsNotARealIdentity"
		:request-date "20180527T093358Z"
		:region "global"
		:service "testsuite"
		:signed-headers (create 'host "localhost")
		:signature "5c4446f0df8e87814c774517f95d970b46dbf25d9279b6b8b418b779f817ef5e")))))
	(chain window (add-event-listener "load" #'testsuite-signature)))))))


;;;;
;;;; Resource Verifying Signatures
;;;;

(defclass verify-hmac-authorization-trait (webmachine:resource)
  ((signature-algorithm
    :initform '(:external-format :utf-8
		:mac :hmac
		:digest :sha256
		:name "MELUSINA1"
		:request-date-time :request-date-time)
    :documentation
    "Parameters used in signature algorithms.")
   (mandatory-signed-headers
    :initform '(:host :request-date-time))))

(defgeneric find-signature-key (resource identity)
  (:documentation
   "Find the hashed key for IDENTITY.
This must be implemented by resources using `HMAC-AUHTORIZATION'."))

(defmethod webmachine:resource-authorized-p ((resource verify-hmac-authorization-trait) request)
  (flet ((find-key (identity)
	   (find-signature-key resource identity)))
    (let ((*signature-algorithm*
	    (slot-value resource 'signature-algorithm)))
      (verify-signature request
			(slot-value resource 'mandatory-signed-headers)
			#'find-key))))


(in-package #:org.melusina.webmachine/example)


;;;;
;;;; Example 
;;;;

(defclass private-constant-resource (org.melusina.webmachine/signature:verify-hmac-authorization-trait constant-resource)
  ((signature-key-repository
    :initarg :signature-key-repository))
  (:default-initargs
   :signature-key-repository nil))

(defmethod org.melusina.webmachine/signature:find-signature-key ((resource private-constant-resource) identity)
  (cdr (assoc identity (slot-value resource 'signature-key-repository) :test #'string=)))

(defun make-example-data ()
  (make-instance 'private-constant-resource
		 :flexible-negotiation-p t
		 :signature-key-repository '(("TESTSUITE" . "ThisIsNotARealKey"))
		 :name 'example-data
		 :path "/api/v0/testsuite"
		 :response "{\"message\":\"Example Message\"}"))



;;;;
;;;; Signature Resource
;;;;

(defun demonstrate-signature-button ()
  (who:with-html-output (*standard-output*)
    (:button :type "button"
	     :id "demonstrate-signature"
	     :class "btn btn-outline-secondary mb-4"
	     :data-wm-dispatch "demonstrate-signature"
	     "Demonstrate Signature")))

(defun signature-state-javascript ()
  (who:with-html-output (*standard-output*)
    (:script
     :id "user-state-javascript"
     :type "text/javascript"
     (who:str
      (ps
 	(defun make-signature-initial-state ()
	  (create :busy nil))
	(defun signature-busy-p (state)
	  (getprop state 'busy))
	(defun (setf signature-busy-p) (new-value state)
	  (setf (getprop state 'busy) new-value))
	(defun signature-transition (action state)
	  (case (getprop action 'name)
	    (:enable
	     (setf (signature-busy-p state) nil))
	    (:disable
	     (setf (signature-busy-p state) t))
	    (:demonstrate-signature
	     (setf (signature-busy-p state) t)
	     (demonstrate-signature)))
	  state)
	(defun demonstrate-signature ()
	  (let ((request
		  (new (-request
			"http://localhost:8080/api/v0/testsuite"
			(create 'method "GET")))))
	    (chain
	     (sign-request :request request
			   :region "global"
			   :service "testsuite"
			   :signed-headers (create "host" "localhost:8080")
			   :identity "TESTSUITE"
			   :key "ThisIsNotARealKey")
	     (then #'debug)
	     (then #'fetch)
	     (then #'debug))))
	(install-application
	 (make-signature-initial-state)
	 #'signature-transition))))))

(define-constant-resource (signature :uri "/signature" :content-type :text/html)
  (html-page (:title "Signature Example for Webmachine"
	      :navigation "Signature")
    (utilities-javascript)
    (functional-state-javascript)
    (signature-state-javascript)
    (html-page-title "Signature")
    (demonstrate-signature-button)
    (org.melusina.webmachine/signature:testsuite-javascript)
    (org.melusina.webmachine/signature:signature-javascript)
    (org.melusina.webmachine/signature:testsuite-signature-javascript)))

;;;; End of file `signature.lisp'
