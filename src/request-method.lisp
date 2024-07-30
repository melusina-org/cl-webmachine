;;;; request-method.lisp — Repository of Request Methods for Webmachine

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

;;;
;;; Request Methods
;;;

(defclass request-method nil
  ((identifier
    :initform (error "No request-method IDENTIFIER.")
    :initarg :identifier
    :documentation "A string representing the REQUEST-METHOD.")
   (request-class
    :initform 'request
    :initarg :request-class
    :documentation "The specialized class of requests associated to this method.")
   (destructive-p
    :initform (error "No request-method DESTRUCTIVE-P flag.")
    :initarg :destructive-p
    :documentation "A flag identifying request-methods associated with destructive operations.")
   (description
    :initform "I am too lazy to describe this request-method."
    :initarg :description
    :documentation
    "A description of the request-method, this description can be inserted in automatically
generated documents."))
  (:documentation
   "This a request method for the HTTP protocol."))

(defun request-method-name (instance)
  "The name of a request method."
  (check-type instance request-method)
  (string (slot-value instance 'identifier)))

(defmethod print-object ((object request-method) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (identifier) object
      (format stream "~S" identifier))))

(defvar *request-method-repository* (make-hash-table)
  "A repository for request methods.
When a request-method is defined, it is added to this table, using its identifier–a keyword–as the key.")

(defmethod initialize-instance :after ((instance request-method) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (gethash (slot-value instance 'identifier) *request-method-repository*) instance))

(defun request-method-p (thing)
  "Predicate recognising request-methods."
  (typep thing 'request-method))


;;;;
;;;; Request Method Repository
;;;;

(defun find-request-method (designator)
"Find a request-method whose identifier matches DESIGNATOR.
When a request method corresponding to DESIGNATOR is found, this request
method is returned, otherwise NIL is returned. The DESIGNATOR can be a string,
a keyword, a request-method, or a symbol."
  (cond
    ((or (stringp designator)
	 (keywordp designator))
     (maphash (lambda (k v)
                (when (string-equal (string k) (string designator))
                  (return-from find-request-method v)))
              *request-method-repository*))
    ((request-method-p designator)
     (find-request-method (slot-value designator 'identifier)))
    ((typep designator 'hunchentoot:request)
     (find-request-method (hunchentoot:request-method designator)))
    ((and designator (symbolp designator))
     (gethash designator *request-method-repository*))
    (t
     (error "~A is not a valid request-method designator." designator))))

(defun remove-request-method (designator)
  "Remove request method from the repository of known request methods.
Return T if there was such an entry, or NIL if not."
  (let ((request-method
	  (find-request-method designator)))
    (when request-method
      (remhash (slot-value request-method 'identifier) *request-method-repository*))))

(defun make-request-method (&rest initargs &key identifier request-class destructive-p description)
  "Make a request-method with the given IDENTIFIER and ARGS."
  (declare (ignore request-class destructive-p description))
  (remove-request-method identifier)
  (apply #'make-instance 'request-method initargs))

(defmacro define-request-method (identifier &rest initargs &key request-class destructive-p description)
  "Define a request-method with the given IDENTIFIER and ARGS."
  (declare (ignore request-class destructive-p description))
  `(make-request-method :identifier ,identifier ,@initargs))

(defun list-request-methods ()
  "A list of designators of all request-methods known to the system."
  (loop :for designator :being :the :hash-keys :of *request-method-repository*
        :collect designator))

(defun describe-request-methods ()
  "A text holding the description of every known request method."
  (with-output-to-string (description)
    (maphash (lambda (k v)
               (format description "~&~A: ~A" k (slot-value v 'description)))
             *request-method-repository*)))

(defparameter *request-method-repository-initial-content*
  '((:identifier :get
     :request-class get-request
     :destructive-p nil
     :description "The GET method requests a representation of the specified resource.
Requests using GET should only retrieve data.
WWW: https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/GET")
    (:identifier :head
     :request-class head-request
     :destructive-p nil
     :description "The HEAD method asks for a response nearly identical to that of a GET request,
but without the response body.
WWW: https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/HEAD")
    (:identifier :post
     :request-class post-request
     :destructive-p t
     :description "The POST method is used to submit an entity to the specified resource,
often causing a change in state or side effects on the server.
WWW: https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/POST")
    (:identifier :put
     :request-class put-request
     :destructive-p t
     :description "The PUT method replaces all current representations of the target resource
with the request payload.
WWW: https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/PUT")
    (:identifier :delete
     :request-class delete-request
     :destructive-p t
     :description "The DELETE method deletes the specified resource.
WWW: https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/DELETE")
    (:identifier :connect
     :request-class connect-request
     :destructive-p nil
     :description "The CONNECT method establishes a tunnel to the server identified
by the target resource.
WWW: https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/CONNECT")
    (:identifier :options
     :request-class options-request
     :destructive-p nil
     :description "The OPTIONS method is used to describe the communication options
for the target resource.
WWW: https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/OPTIONS")
    (:identifier :trace
     :request-class trace-request
     :destructive-p nil
     :description "The TRACE method performs a message loop-back test along the path
to the target resource.
WWW: https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/TRACE")
    (:identifier :patch
     :request-class patch-request
     :destructive-p t
     :description "The PATCH method is used to apply partial modifications to a resource.
WWW: https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/PATCH")))

(loop :for request-method-spec :in *request-method-repository-initial-content*
      :do (apply #'make-request-method request-method-spec))

;;;; End of file `request-method.lisp'
