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

;;;; End of file `reply.lisp'
