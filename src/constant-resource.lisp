;;;; constant-resource.lisp — Constant Resource

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

(defclass constant-resource (resource)
  ((available-p
    :initarg :available-p
    :documentation
    "The return value of the `RESOURCE-AVAILABLE-P' generic function.")
   (known-methods
    :initarg :known-methods
    :documentation
    "The return value of the `RESOURCE-KNOWN-METHODS' generic function.")
   (uri-too-long-p
    :initarg :uri-too-long-p
    :documentation
    "The return value of the `RESOURCE-URI-TOO-LONG-P' generic function.")
   (payload-too-large-p
    :initarg :payload-too-large-p
    :documentation
    "The return value of the `RESOURCE-PAYLOAD-TOO-LARGE-P' generic function.")
   (allowed-methods
    :initarg :allowed-methods
    :documentation
    "The return value of the `RESOURCE-ALLOWED-METHODS' generic function.")
   (valid-request-p
    :initarg :valid-request-p
    :documentation
    "The return value of the `RESOURCE-VALID-REQUEST-P' generic function.")
   (authorized-p
    :initarg :authorized-p
    :documentation
    "The return value of the `RESOURCE-AUTHORIZED-P' generic function.")
   (forbidden-p
    :initarg :forbidden-p
    :documentation
    "The return value of the `RESOURCE-FORBIDDEN-P' generic function.")
   (valid-content-headers-p
    :initarg :valid-content-headers-p
    :documentation
    "The return value of the `RESOURCE-VALID-CONTENT-HEADERS-P' generic function.")
   (valid-content-type-p
    :initarg :valid-content-type-p
    :documentation
    "The return value of the `RESOURCE-VALID-CONTENT-TYPE-P' generic function.")
   (options
    :initarg :options
    :initform t
    :documentation
    "The return value of the `RESOURCE-OPTIONS' generic function.")
   (content-types-provided
    :initarg :content-types-provided
    :initform '(:text/html)
    :documentation
    "The return value of the `RESOURCE-CONTENT-TYPES-PROVIDED' generic function.")
   (languages-provided
    :initarg :languages-provided
    :documentation
    "The return value of the `RESOURCE-LANGUAGES-PROVIDED' generic function.")
   (charsets-provided
    :initarg :charsets-provided
    :documentation
    "The return value of the `RESOURCE-CHARSETS-PROVIDED' generic function.")
   (encodings-provided
    :initarg :encodings-provided
    :documentation
    "The return value of the `RESOURCE-ENCODINGS-PROVIDED' generic function.")
   (flexible-negotiation-p
    :initarg :flexible-negotiation-p
    :documentation
    "The return value of the `RESOURCE-FLEXIBLE-NEGOTIATION-P' generic function.")
   (response
    :initarg :response
    :documentation
    "The response for the constant resources.
The response can be one of the following

- A string, which is sent to the client.
- A function of a stream, which is responsible for writing
  the response body in the provided stream.
- An alist mapping content-types to strings or functions, with
  the same convention as above."))
  (:default-initargs
   :available-p t
   :uri-too-long-p nil
   :payload-too-large-p nil
   :allowed-methods '(:get :head)
   :known-methods '(:get :head :post :put :delete :connect :options :trace :patch)
   :valid-request-p t
   :authorized-p t
   :forbidden-p nil
   :valid-content-headers-p t
   :valid-content-type-p t
   :options t
   :content-types-provided '(:text/plain)
   :languages-provided '(:en-US)
   :charsets-provided '(:utf-8)
   :encodings-provided '(:identity)
   :flexible-negotiation-p nil
   :response '((:text/plain . "A constant resource.")))
  (:documentation
   "A resource on which generic functions return a fixed value, corresponding
to initialisation parameters of the instance. Such a resource is useful
for testing."))

(defmethod initialize-instance :after ((instance constant-resource)
                                       &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (with-slots (content-types-provided) instance
    (setf content-types-provided
          (mapcar #'find-media-type content-types-provided))
    (setf content-types-provided
          (delete nil content-types-provided)))
  (with-slots (response) instance
    (when (typep response 'list)
      (dolist (binding response)
	(setf (car binding) (find-media-type (car binding)))))))

(defmethod write-resource-response ((resource constant-resource)
				    (request get-request)
				    (reply reply)
				    response-body)
  (flet ((choose-response (response)
           (etypecase response
	     (function
	      response)
	     (string
	      response)
	     (symbol
	      response)
             (list
              (cdr (assoc (find-media-type
			   (reply-media-type reply))
			  response)))))
         (send-response (response)
           (when response
	     (etypecase response
	       (string 
		response)
	       (function
		(funcall response response-body))
	       (symbol
		(funcall response response-body))))))
    (send-response
     (choose-response (slot-value resource 'response)))))

(defmethod resource-available-p and ((resource constant-resource))
  (slot-value resource 'available-p))

(defmethod resource-known-methods append ((resource constant-resource))
  (slot-value resource 'known-methods))

(defmethod resource-uri-too-long-p ((resource constant-resource) request)
  (slot-value resource 'uri-too-long-p))

(defmethod resource-payload-too-large-p ((resource constant-resource) request)
  (slot-value resource 'payload-too-large-p))

(defmethod resource-allowed-methods ((resource constant-resource))
  (slot-value resource 'allowed-methods))

(defmethod resource-valid-request-p and ((resource constant-resource) request)
  (slot-value resource 'valid-request-p))

(defmethod resource-authorized-p ((resource constant-resource) request)
  (if (slot-value resource 'authorized-p)
      (values t)
      (values nil "None")))

(defmethod resource-forbidden-p or ((resource constant-resource) request)
  (slot-value resource 'forbidden-p))

(defmethod resource-valid-content-headers-p and ((resource constant-resource) request)
  (slot-value resource 'valid-content-headers-p))

(defmethod resource-valid-content-type-p and ((resource constant-resource) request)
  (slot-value resource 'valid-content-type-p))

(defmethod resource-options append ((resource constant-resource) request)
  (slot-value resource 'options))

(defmethod resource-content-types-provided ((resource constant-resource) request)
  (slot-value resource 'content-types-provided))

(defmethod resource-languages-provided ((resource constant-resource) request)
  (slot-value resource 'languages-provided))

(defmethod resource-charsets-provided ((resource constant-resource) request)
  (slot-value resource 'charsets-provided))

(defmethod resource-encodings-provided ((resource constant-resource) request)
  (slot-value resource 'encodings-provided))

(defmethod resource-flexible-negotiation-p ((resource constant-resource) request)
  (slot-value resource 'flexible-negotiation-p))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun register-constant-resource (name &key uri (content-type :text/html))
    (setf (resource name)
	  (make-instance 'constant-resource
			 :flexible-negotiation-p t
			 :name name
			 :response name
			 :content-types-provided (list content-type)
			 :path uri))))

(defmacro define-constant-resource ((name &key uri content-type) &body body)
  "Define a constant resource."
  (check-type name symbol)
  (unless uri
    (error "A CONTSTANT-RESOURCE requires a URI."))
  `(progn
     (defun ,name (stream)
       (let ((*standard-output* stream))
	 ,@body))
     (register-constant-resource (quote ,name) :uri ,uri :content-type ,content-type)))

(defun make-constant-resource (&rest initargs
			       &key path name available-p uri-too-long-p
				    payload-too-large-p allowed-methods
				    known-methods valid-request-p authorized-p
				    forbidden-p valid-content-headers-p
				    valid-content-type-p options
				    content-types-provided languages-provided
				    charsets-provided encodings-provided
				    flexible-negotiation-p response)
  "Make a CONSTANT-RESOURCE with provided details."
  (declare (ignore path name available-p uri-too-long-p payload-too-large-p
		   allowed-methods known-methods valid-request-p authorized-p
		   forbidden-p valid-content-headers-p valid-content-type-p
		   options content-types-provided languages-provided
		   charsets-provided encodings-provided flexible-negotiation-p
		   response))
  (apply #'make-instance 'constant-resource initargs))

;;;; End of file `constant-resource.lisp'
