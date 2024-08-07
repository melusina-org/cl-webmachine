;;;; request.lisp — HTTP Request for Webmachine

;;;; Webmachine (https://github.com/melusina-org/cl-webmachine)
;;;; This file is part of Webmachine.
;;;;
;;;; Copyright © 2018–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:webmachine)

(defclass request (hunchentoot:request)
  ((path-parameters
    :initarg :path-parameters
    :initform nil
    :documentation
    "The alist of parameters inferred by the path.")
   (language
    :initarg :language
    :initform nil
    :documentation "The negotiated language for the request.
When this parameter is non NIL, it it set by the content negotiation and
the client expects the returned content to be in that language.")
   (charset
    :initarg :charset
    :initform nil
    :documentation "The negotiated charset for the request.
When this parameter is not NIL it is set by the content negotiation
and the client expects the returned content to be prepared using
this charset.")
   (encoding
    :initarg :charset
    :initform nil
    :documentation "The negotiated encoding for the request.
When this parameter is non NIL, it is set by the content negotiation
and the client expects the returned content to be encoded using
the corresponding function.

The possible values for this field are:

  NIL, :IDENTITY, :GZIP, :DEFLATE, and :COMPRESS."))
  (:documentation
   "Objects of this class hold all the information
about an incoming request. They are created automatically by
acceptors and can be accessed by the corresponding handler."))


;;;;
;;;; Request by Method
;;;;

(defclass get-request (request) nil
  (:documentation "The GET method requests a representation of the specified resource.
Requests using GET should only retrieve data.
WWW: https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/GET"))

(defclass head-request (request) nil
  (:documentation "The HEAD method asks for a response nearly identical to that of a GET request,
but without the response body.
WWW: https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/HEAD"))

(defclass post-request (request) nil
  (:documentation "The POST method is used to submit an entity to the specified resource,
often causing a change in state or side effects on the server.
WWW: https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/POST"))

(defclass put-request (request) nil
  (:documentation "The PUT method replaces all current representations of the target resource
with the request payload.
WWW: https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/PUT"))

(defclass delete-request (request) nil
  (:documentation "The DELETE method deletes the specified resource.
WWW: https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/DELETE"))

(defclass connect-request (request) nil
  (:documentation "The CONNECT method establishes a tunnel to the server identified
by the target resource.
WWW: https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/CONNECT"))

(defclass options-request (request) nil
  (:documentation "The OPTIONS method is used to describe the communication options
for the target resource.
WWW: https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/OPTIONS"))

(defclass trace-request (request) nil
  (:documentation "The TRACE method performs a message loop-back test along the path
to the target resource.
WWW: https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/TRACE"))

(defclass patch-request (request) nil
  (:documentation "The PATCH method is used to apply partial modifications to a resource.
WWW: https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/PATCH"))


;;;;
;;;; WITH-PATH-PARAMETERS
;;;;

(defun parameter-value (name request)
  "The value associated to NAME in request.
When NAME has no associated value, a status code 400 Bad Request is
returned."
  (or (cdr (assoc name (slot-value request 'path-parameters)
		  :test #'string-equal))
      (http-error 400)))

(defmacro with-path-parameters (parameters request &body body)
  "Run BODY in an environment where PARAMETERS are bound to REQUEST.
The PARAMETERS are either a Symbol, which then associated to a parameter
of the same name, or a list with two elements, a Symbol and a parameter name."
  (alexandria:once-only (request)
    (flet ((well-formed-p (parameter-entry)
	     (and (listp parameter-entry)
		  (= 2 (length parameter-entry))
		  (symbolp (first parameter-entry))
		  (keywordp (second parameter-entry))))
	   (error-malformed (parameter-entry)
	     (error #.(concatenate
		       'string
		       "Malformed parameter entry: ~S should be a symbol "
		       "or (variable-name parameter-name)")
		    parameter-entry))
	   (binding (parameter-entry)
	     (let ((variable-name (first parameter-entry))
		   (parameter-name (second parameter-entry)))
	       `(,variable-name (parameter-value ,parameter-name ,request)))))
      (let* ((parameters
	       (loop :for parameter-entry :in parameters
		     :collect (cond
				((symbolp parameter-entry)
				 (list parameter-entry (symbol-name parameter-entry)))
				((well-formed-p parameter-entry)
				 parameter-entry)
				(t
				 (error-malformed parameter-entry)))))
	     (bindings
	       (mapcar #'binding parameters)))
	`(let ,bindings ,@body)))))

;;;; End of file `request.lisp'
