;;;; reply.lisp — HTTP Reply for Webmachine

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

(defclass reply (hunchentoot:reply)
  ((content-handler
    :initarg :content-type
    :initform 'resource-to-text/html
    :documentation
    "The content handler function used to present the request result.
The CONTENT-HANDLER is an atom that can be `FUNCALL'-ed.

See also `RESOURCE-CONTENT-TYPES-PROVIDED'."))
  (:documentation "Objects of this class hold all the information
about an outgoing reply.  They are created automatically by
Webmachine and can be accessed and modified by the corresponding
handler."))


;;;;
;;;; Replies assocaited to discrete media types
;;;;

(defclass application-reply (reply) nil
  (:documentation "The REPLY subclass for content of general type."))

(defclass audio-reply (reply) nil
  (:documentation "The REPLY subclass for audio content."))

(defclass font-reply (reply) nil
  (:documentation "The REPLY subclass for font or typeface content."))

(defclass image-reply (reply) nil
  (:documentation "The REPLY subclass for image content."))

(defclass model-reply (reply) nil
  (:documentation "The REPLY subclass for 3D model content."))

(defclass text-reply (reply) nil
  (:documentation "The REPLY subclass for text content."))

(defclass video-reply (reply) nil
  (:documentation "The REPLY subclass for video content."))

(defclass application/octet-stream-reply (application-reply) nil
  (:documentation "The REPLY subclass for application/octet-stream content."))

(defclass text/css-reply (text-reply) nil
  (:documentation "The REPLY subclass for text/css content."))

(defclass text/html-reply (text-reply) nil
  (:documentation "The REPLY subclass for text/html content."))

(defclass text/javascript-reply (text-reply) nil
  (:documentation "The REPLY subclass for text/javascript content."))

(defclass text/plain-reply (text-reply) nil
  (:documentation "The REPLY subclass for text/plain content."))

(defclass image/jpeg-reply (image-reply) nil
  (:documentation "The REPLY subclass for image/jpeg content."))

(defclass image/png-reply (image-reply) nil
  (:documentation "The REPLY subclass for image/png content."))

(defclass image/svg+xml-reply (image-reply) nil
  (:documentation "The REPLY subclass for image/svg+xml content."))

;;;; End of file `reply.lisp'

