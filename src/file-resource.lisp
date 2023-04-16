;;;; file-resource.lisp — Directory Resource

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

(defclass file-resource (resource)
  ((alias
    :initarg :alias
    :documentation "The PATHNAME to the file served by the resource.")
   (content-type
    :initarg :content-type
    :initform nil))
  (:documentation
   "A `FILE-RESOURCE' map a regular file into the web space.

The request URI is validated using `HUNCHENTOOT::PARSE-PATH' so that it
does not contain directory traversals, explicit devices not host fields.
When the URI of a request fails to validate, a Bad Request status
is returned.

The served CONTENT-TYPE is inferred from the name of the file requested,
based on the CONTENT-TYPE-RULES slot."))

(defmethod initialize-instance :after ((instance file-resource)
                                       &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (flet ((check-that-path-does-not-end-with-a-slash ()
	   (with-slots (path) instance
	     (unless (and (stringp path)
			  (plusp (length path))
			  (not (char= (char path (1- (length path))) #\/)))
	       (error "The resource PATH ~S must end with a slash." path))))
	 (check-that-alias-designates-a-file ()
	   (with-slots (alias) instance
	     (when (fad:directory-pathname-p alias)
	       (error "The resource ALIAS ~S must designate a file." alias))))
	 (ensure-content-type-is-initialised ()
	   (with-slots (content-type alias) instance
	     (unless content-type
	       (setf content-type (file-content-type alias))))))
    (check-that-path-does-not-end-with-a-slash)
    (check-that-alias-designates-a-file)
    (ensure-content-type-is-initialised)))

(defmethod print-object ((object file-resource) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name path alias) object
      (format stream "~S :URI ~S :ALIAS ~S" name path alias))))

(defun make-file-resource (&rest initargs &key path name alias)
  "Make a FILE-RESOURCE with provided details."
  (declare (ignore path name alias))
  (apply #'make-instance 'file-resource initargs))

(defmethod resource-handle-request-p ((instance file-resource) request)
  (string=
   (slot-value request 'hunchentoot:script-name)
   (slot-value instance 'path)))

(defmethod resource-exists-p and ((resource file-resource) request)
  (with-slots (alias) resource
    (and (fad:file-exists-p alias)
         (not (fad:directory-exists-p alias)))))

(defmethod resource-forbidden-p or ((resource file-resource) request)
  (with-slots (alias) resource
    (with-open-file (file alias
			  :direction :input
			  :if-does-not-exist nil)
      (not file))))

(defmethod resource-allowed-methods ((resource file-resource))
  '(:get :head :options))

(defmethod resource-content-types-provided ((resource file-resource) request)
  (list (slot-value resource 'content-type)))

(defmethod resource-flexible-negotiation-p ((resource file-resource) request)
  t)

(defmethod write-resource-response ((resource file-resource)
				    (request get-request)
				    (reply reply)
				    response-body)
  (send-file-contents (slot-value resource 'alias) response-body))

;;;; End of file `file-resource.lisp'
