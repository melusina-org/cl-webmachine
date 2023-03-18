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
   (identifier
    :initarg :identifier
    :initform (error "No media-type IDENTIFIER.")
    :documentation "A keyword representing the MEDIA-TYPE.")
   (reply-class
    :initarg :reply-class
    :initform 'application
    :documentation "The class to use to subtype replies with this content type.")
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
its IDENTIFIER as the key.")

(defmethod initialize-instance :after ((instance media-type) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (gethash (slot-value instance 'identifier) *media-type-repository*) instance))

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
     (nth-value 0 (gethash designator *media-type-repository*)))
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
      (remhash (slot-value media-type 'identifier) *media-type-repository*))))

(defun make-media-type (&rest initargs &key identifier name reply-class description)
  "Make a media-type with the given NAME and ARGS."
  (check-type identifier keyword)
  (check-type name (or null string))
  (check-type reply-class symbol)
  (check-type description string)
  (remove-media-type identifier)
  (unless name
    (setf initargs
	  (append (list :name (string-downcase (string identifier))) initargs)))
  (apply #'make-instance 'media-type initargs))

(defmacro define-media-type (identifier &rest initargs &key reply-class description)
  "Define a media-type with the given NAME and ARGS."
  (declare (ignore reply-class description))
  `(make-media-type :identifier ,identifier ,@initargs))

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


;;;;
;;;; Media Type Repository Initial Content
;;;;  (Discrete Types)
;;;;

(define-media-type :application
  :reply-class 'application-reply
  :description
  "Any kind of binary data that doesn't fall explicitly into one of the other types.
Either data that will be executed or interpreted in some way or binary
data that requires a specific application or category of application to
use. Generic binary data (or binary data whose true type is unknown) is
application/octet-stream. Other common examples include application/pdf,
application/pkcs8, and application/zip. (See application type registry
at IANA)")

(define-media-type :audio
  :reply-class 'audio-reply
  :description "Audio or music data.
Examples include audio/mpeg, audio/vorbis.")

(define-media-type :font
  :reply-class 'font-reply
  :description "Font/typeface data.
Common examples include font/woff, font/ttf, and font/otf.")

(define-media-type :image
  :reply-class 'image-reply
  :description "Image or graphical data.
This includes both bitmap and vector still images as well as animated
versions of still image formats such as animated GIF or APNG. Common
examples are image/jpeg, image/png, and image/svg+xml.")

(define-media-type :model
  :reply-class 'model-reply
  :description "Model data for a 3D object or scene.
Examples include model/3mf and model/vrml.")

(define-media-type :text
  :reply-class 'text-reply
  :description "Text-only data.
This includes any human-readable content, source code, or textual data
such as comma-separated value (CSV) formatted data. Examples include:
text/plain, text/csv, and text/html.")

(define-media-type :video
  :reply-class 'video-reply
  :description "Video data or files, such as MP4 movies (video/mp4).")


;;;;
;;;; Media Type Repository Initial Content
;;;;  (Important Media Types)
;;;;

(define-media-type :application/octet-stream
  :reply-class 'application/octet-stream-reply
  :description "This is the default for binary files.
As it means unknown binary file, browsers usually don't execute it,
or even ask if it should be executed. They treat it as if the
Content-Disposition header was set to attachment, and propose a “Save
As” dialog.")

(define-media-type :text/css
  :reply-class 'text/css-reply
  :description "CSS files used to style a Web page must be sent with text/css.
If a server doesn't recognize the .css suffix for CSS files, it may send
them with text/plain or application/octet-stream MIME types. If so, they
won't be recognized as CSS by most browsers and will be ignored.")

(define-media-type :text/html
  :reply-class 'text/html-reply
  :description "All HTML content should be served with this type.")

(define-media-type :text/javascript
  :reply-class 'text/javascript-reply
  :description "The type for serving JavaScript programs.")

(define-media-type :text/plain
  :reply-class 'text/plain-reply
  :description "This is the default for textual files.
Even if it really means “unknown textual file” browsers assume they can display it.")

(define-media-type :image/jpeg
  :reply-class 'image/jpeg-reply
  :description "Images stored as JPEG data.")

(define-media-type :image/png
  :reply-class 'image/png-reply
  :description "Images stored as PNG data.")

(define-media-type :image/svg+xml
  :reply-class 'image/svg+xml-reply
  :description "Images stored as XVG+XML data.")

;;;; End of file `media-type.lisp'
