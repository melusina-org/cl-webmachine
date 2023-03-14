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

(in-package #:org.melusina.webmachine/testsuite)

(define-testcase ensure-important-request-methods-for-web-developers-are-known ()
  (let ((important-request-methods
	  '("GET" "HEAD" "POST" "PUT" "DELETE" "CONNECT" "OPTIONS" "TRACE" "PATCH")))
    (loop :for important-request-method :in important-request-methods
	  :do (assert-t* (webmachine:find-request-method important-request-method)))))

(define-testcase testsuite-request-method ()
  (ensure-important-request-methods-for-web-developers-are-known))

;;;; End of file `request-method.lisp'
