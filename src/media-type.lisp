;;;; media-type.lisp — Repository of Media Types or MIME Types for Webmachine

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

(defclass media-type nil
  ((name
    :initarg :name
    :reader media-type-name
    :initform (error "No media-type NAME.")
    :documentation "A string representing the MEDIA-TYPE.")
   (suffixes
    :initarg :suffixes
    :initform nil
    :documentation "A list of file name SUFFIXES typically used for files of that MEDIA-TYPE.")
   (description
    :initarg :description
    :reader media-type-description
    :initform nil
    :documentation
    "A description of the media-type. If present, this description can be inserted
in automatically generated documents."))
  (:documentation "This represents a MEDIA-TYPE."))

(defmethod print-object ((object media-type) stream)
  (format stream "#<MEDIA-TYPE ~S>" (slot-value object 'name)))

(defvar *media-type-repository* (make-hash-table)
  "The table of all media-types.
When a media-type is defined, it is added to this table, using
its NAME as the key.")

(defmethod initialize-instance :after ((instance media-type) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (gethash (slot-value instance 'name) *media-type-repository*) instance))

(defun media-type-p (thing)
  "Predicate recognising media types."
  (typep thing 'media-type))

(defun find-media-type (designator)
  "Find a media-type whose name matches DESIGNATOR.
When a media type corresponding to DESIGNATOR is found, this request
method is returned, otherwise NIL is returned. The DESIGNATOR can be a string,
a keyword, a request-method, or a symbol."
  (cond
    ((stringp designator)
     (maphash (lambda (k v)
                (when (string-equal (string k) (string designator)
				    :end2 (position #\; designator))
                  (return-from find-media-type v)))
              *media-type-repository*))
    ((keywordp designator)
     (maphash (lambda (k v)
                (when (string-equal (string k) designator)
                  (return-from find-media-type v)))
              *media-type-repository*))
    ((media-type-p designator)
     designator)
    ((and designator (symbolp designator))
     (gethash designator *media-type-repository*))
    (t
     (error "~A is not a valid media-type designator." designator))))

(defun remove-media-type (designator)
  "Remove media type from the repository of known media types.
Return T if there was such an entry, or NIL if not."
  (let ((media-type
	  (find-media-type designator)))
    (when media-type
      (remhash (slot-value media-type 'name) *media-type-repository*))))

(defun make-media-type (&rest initargs &key name suffixes description)
  "Make a media-type with the given NAME and ARGS."
  (declare (ignore suffixes description))
  (check-type name string)
  (remove-media-type name)
  (apply #'make-instance 'media-type initargs))

(defmacro define-media-type (name &rest initargs &key suffixes description)
  "Define a media-type with the given NAME and ARGS."
  (declare (ignore suffixes description))
  `(make-media-type :name ,name ,@initargs))

(defun list-media-types ()
  "A list of designators of all media types known to the system."
  (loop :for designator :being :the :hash-keys :of *media-type-repository*
        :collect designator))

(defun describe-media-types ()
  "A text holding the description of every known media type."
  (with-output-to-string (description)
    (maphash (lambda (k v)
               (format description "~&~A: ~A" k
		       (or (slot-value v 'description)
			   "No description provided")))
             *media-type-repository*)))

(defun media-type-repository-initial-content ()
  "The initial content of the media type repostiory."
  (loop :for (media-subtype . suffixes) :in hunchentoot::*mime-type-list*
        :collect (list :name media-subtype :suffixes suffixes)))

(loop :for media-type-spec :in (media-type-repository-initial-content)
      :do (apply #'make-media-type media-type-spec))

;;;; End of file `media-type.lisp'
