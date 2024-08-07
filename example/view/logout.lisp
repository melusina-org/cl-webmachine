;;;; logout.lisp — logout View Webmachine Example

;;;; Webmachine (https://github.com/melusina-org/cl-webmachine)
;;;; This file is part of Webmachine.
;;;;
;;;; Copyright © 2018–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:webmachine/example)

(hunchentoot:define-easy-handler (logout :uri "/logout") ()
  (when hunchentoot:*session*
    (hunchentoot:remove-session hunchentoot:*session*))
  (hunchentoot:redirect "/home"))

;;;; End of file `logout.lisp'
