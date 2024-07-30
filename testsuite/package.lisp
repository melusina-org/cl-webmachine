;;;; package.lisp — Package for Webmachine tests

;;;; Webmachine (https://github.com/melusina-org/cl-webmachine)
;;;; This file is part of Webmachine.
;;;;
;;;; Copyright © 2018–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:webmachine/testsuite
  (:use #:common-lisp)
  (:local-nicknames
   (#:example #:webmachine/example)
   (#:signature #:webmachine/signature))
  (:import-from
   #:alexandria
   #:ensure-list
   #:with-unique-names)
  (:import-from #:confidence
   #:define-testcase
   #:define-assertion
   #:assert-condition
   #:assert=
   #:assert-t
   #:assert-t*
   #:assert-nil
   #:assert-eq
   #:assert-equal
   #:assert-set-equal
   #:assert-string=
   #:assert-type))

(in-package #:webmachine/testsuite)

;;;; End of file `package.lisp'
