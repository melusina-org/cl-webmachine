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

(in-package #:org.melusina.webmachine)

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

;;;; End of file `request.lisp'
