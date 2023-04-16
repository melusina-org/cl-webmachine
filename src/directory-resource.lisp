;;;; directory-resource.lisp — Directory Resource

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

(defclass directory-resource (resource)
  ((alias
    :initarg :alias
    :documentation "The directory owning the files served by the resource.
This must be a PATHNAME designating a directory."))
  (:documentation
   "A `DIRECTORY-RESOURCE' map any part of the filesystem into the web space.
It is similar to Apache's Alias feature. The content of the directory is only
read once after the resource is created.

The request URI is validated using `HUNCHENTOOT::PARSE-PATH' so that it
does not contain directory traversals, explicit devices not host fields.
When the URI of a request fails to validate, a Bad Request status
is returned.

The served CONTENT-TYPE is inferred from the name of the file requested,
based on the CONTENT-TYPE-RULES slot."))

(defmethod initialize-instance :after ((instance directory-resource)
                                       &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (flet ((check-that-path-ends-with-a-slash ()
	   (with-slots (path) instance
	     (unless (and (stringp path)
			  (plusp (length path))
			  (char= (char path (1- (length path))) #\/))
	       (error "The resource PATH ~S must end with a slash." path))))
	 (check-that-alias-designates-a-directory ()
	   (with-slots (alias) instance
	     (unless (fad:directory-pathname-p alias)
	       (error "The resource ALIAS ~S must designate a directory." alias)))))
    (check-that-path-ends-with-a-slash)
    (check-that-alias-designates-a-directory)))

(defmethod print-object ((object directory-resource) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name path alias) object
      (format stream "~S :URI ~S :ALIAS ~S" name path alias))))

(defun make-directory-resource (&rest initargs &key path name alias)
  "Make a DIRECTORY-RESOURCE with provided details."
  (declare (ignore path name alias))
  (apply #'make-instance 'directory-resource initargs))

(defmethod resource-handle-request-p ((instance directory-resource) request)
  (hunchentoot::starts-with-p
   (slot-value request 'hunchentoot:script-name)
   (slot-value instance 'path)))

(defmethod resource-valid-request-p and ((resource directory-resource) request)
  (let ((pathname
	  (with-slots (alias path) resource
	    (let ((request-pathname
		    (hunchentoot:request-pathname request path)))
	      (when request-pathname
		(merge-pathnames request-pathname alias))))))
    (flet ((store-pathname ()
	     (when pathname
	       (setf (hunchentoot:aux-request-value :pathname request) pathname)))
	   (store-content-type ()
	     (when pathname
	       (setf (hunchentoot:aux-request-value :content-type request)
		     (file-content-type pathname)))))
      (and (store-pathname)
	   (not (wild-pathname-p pathname))
	   (store-content-type)))))

(defmethod resource-exists-p and ((resource directory-resource) request)
  (let ((pathname
	  (hunchentoot:aux-request-value :pathname request)))
    (and (fad:file-exists-p pathname)
         (not (fad:directory-exists-p pathname)))))

(defmethod resource-forbidden-p or ((resource directory-resource) request)
  (let ((pathname
	  (hunchentoot:aux-request-value :pathname request)))
    (with-open-file (file pathname
			  :direction :input
			  :if-does-not-exist nil)
      (not file))))

(defmethod resource-allowed-methods ((resource directory-resource))
  '(:get :head :options))

(defmethod resource-content-types-provided ((resource directory-resource) request)
  (list (hunchentoot:aux-request-value :content-type request)))

(defmethod resource-flexible-negotiation-p ((resource directory-resource) request)
  t)

(defmethod write-resource-response ((resource directory-resource)
				    (request get-request)
				    (reply reply)
				    response-body)
  (send-file-contents (hunchentoot:aux-request-value :pathname request) response-body))

;;;; End of file `directory-resource.lisp'
