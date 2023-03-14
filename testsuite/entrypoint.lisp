;;;; entrypoint.lisp — Entrypoint for Webmachine

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

(define-testcase run-all-tests ()
  (testsuite-utilities)
  (testsuite-condition)
  (testsuite-content)
  (testsuite-request-method)
  (testsuite-media-type)
  (testsuite-path)
  (testsuite-acceptor)
  (testsuite-resource))

;;;; End of file `entrypoint.lisp'
