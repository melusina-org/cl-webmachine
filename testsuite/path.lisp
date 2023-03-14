;;;; path.lisp — URI Path for Webmachine Testsuite

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

(define-testcase test-match-path (path uri expected-match-p expected-parameters)
  (multiple-value-bind (actual-match-p actual-parameters)
      (webmachine::match-path path uri)
    (assert-equal expected-match-p actual-match-p )
    (assert-equal expected-parameters actual-parameters)))

(define-testcase testsuite-path ()
  (test-match-path "/test/v3b13" "/test/v3b13"
		   t nil)
  (test-match-path "/user/:id" "/user/012"
		   t '((:id . "012")))
  (assert-nil
   (webmachine::match-path "/user/:id" "/user/012/activity/connect"))
  (assert-nil
   (webmachine::match-path "/user/:id" "/chrome/bootstrap/bootstrap.css"))
  (test-match-path "/user/:id/activity/*" "/user/012/activity/connect"
		   t '((:id . "012")
		       (:relative-path . "connect")))
  (assert-nil
   (webmachine::match-path "/user/:id/activity/*" "/user/012"))
  (assert-nil
   (webmachine::match-path "/user/:id/activity/*" "/chrome/bootstrap/bootstrap.css"))
  (test-match-path "/chrome/bootstrap/*" "/chrome/bootstrap/bootstrap.css"
		   t '((:relative-path . "bootstrap.css"))))

;;;; End of file `path.lisp'
