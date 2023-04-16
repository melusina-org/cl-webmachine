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
  ((media-type
    :initform :application
    :reader reply-media-type
    :allocation :class))
  (:documentation "Objects of this class hold all the information
about an outgoing reply.  They are created automatically by
Webmachine and can be accessed and modified by the corresponding
handler."))


;;;;
;;;; Replies assocaited to discrete media types
;;;;

(defclass application-reply (reply)
  ((media-type
    :initform :application
    :allocation :class))
  (:documentation "The REPLY subclass for content of general type."))

(defclass audio-reply (reply)
  ((media-type
    :initform :audio
    :allocation :class))
  (:documentation "The REPLY subclass for audio content."))

(defclass font-reply (reply)
  ((media-type
    :initform :font
    :allocation :class))
  (:documentation "The REPLY subclass for font or typeface content."))

(defclass image-reply (reply)
  ((media-type
    :initform :image
    :allocation :class))
  (:documentation "The REPLY subclass for image content."))

(defclass model-reply (reply)
  ((media-type
    :initform :model
    :allocation :class))
  (:documentation "The REPLY subclass for 3D model content."))

(defclass text-reply (reply)
  ((media-type
    :initform :text
    :allocation :class))
  (:documentation "The REPLY subclass for text content."))

(defclass video-reply (reply)
  ((media-type
    :initform :video
    :allocation :class))
  (:documentation "The REPLY subclass for video content."))

(defclass application/octet-stream-reply (application-reply)
  ((media-type
    :initform :application/octet-stream
    :allocation :class))
  (:documentation "The REPLY subclass for application/octet-stream content."))

(defclass application/json-reply (application-reply)
  ((media-type
    :initform :application/json
    :allocation :class))
  (:documentation "The REPLY subclass for application/json content."))

(defclass text/css-reply (text-reply)
  ((media-type
    :initform :text/css
    :allocation :class))
  (:documentation "The REPLY subclass for text/css content."))

(defclass text/html-reply (text-reply)
  ((media-type
    :initform :text/html
    :allocation :class))
  (:documentation "The REPLY subclass for text/html content."))

(defclass text/javascript-reply (text-reply)
  ((media-type
    :initform :text/javascript
    :allocation :class))
  (:documentation "The REPLY subclass for text/javascript content."))

(defclass text/plain-reply (text-reply)
  ((media-type
    :initform :text/plain
    :allocation :class))
  (:documentation "The REPLY subclass for text/plain content."))

(defclass image/jpeg-reply (image-reply)
  ((media-type
    :initform :image/jpeg
    :allocation :class))
  (:documentation "The REPLY subclass for image/jpeg content."))

(defclass image/png-reply (image-reply)
  ((media-type
    :initform :image/png
    :allocation :class))
  (:documentation "The REPLY subclass for image/png content."))

(defclass image/svg+xml-reply (image-reply)
  ((media-type
    :initform :image/svg+xml
    :allocation :class))
  (:documentation "The REPLY subclass for image/svg+xml content."))

;;;; End of file `reply.lisp'
