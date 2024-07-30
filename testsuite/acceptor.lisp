;;;; acceptor.lisp — Acceptor for Webmachine

;;;; Webmachine (https://github.com/melusina-org/cl-webmachine)
;;;; This file is part of Webmachine.
;;;;
;;;; Copyright © 2018–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:webmachine/testsuite)

(define-testcase testsuite-acceptor ()
  (with-testsuite-acceptor ()
    (with-http-reply ("/non-existent")
      (assert-http-status 404)
      (assert-http-header-match :server "Webmachine .*"))))

;;;; End of file `acceptor.lisp'
