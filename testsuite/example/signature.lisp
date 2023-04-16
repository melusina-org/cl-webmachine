;;;; signature.lisp — Testsuite for signature

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

(define-testcase ensure-that-create-canonical-request-works-as-expected ()
  (assert-string=
   "GET
/api/v0/testsuite

host:webmachine.melusina.local
x-melusina-1:AnExampleValueForASignedHeader
x-request-date:20180527T093358Z

host;x-melusina-1;x-request-date
e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
   (signature::create-canonical-request
    :request-method :get
    :canonical-uri "/api/v0/testsuite"
    :signed-headers
    '((:x-melusina-1 . "AnExampleValueForASignedHeader")
      (:x-request-date . "20180527T093358Z")
      (:host . "webmachine.melusina.local"))
    :hashed-payload (signature::hashed-payload ""))))

(define-testcase ensure-that-create-string-to-sign-works-as-expected ()
  (assert-string=
   "AWS4-HMAC-SHA256
20180527T093358Z
20180527/global/testsuite/aws4_request
e21858f044b2d96960e70f815b2436183ba88084d94c0fc824e95694e7afe2c7"
   (let ((signature::*signature-algorithm*
	    '(:external-format :utf-8
	      :mac :hmac
	      :digest :sha256
	      :name "AWS4")))
     (signature::create-string-to-sign
      :request-date "20180527T093358Z"
      :region :global
      :service :testsuite
      :hashed-canonical-request "e21858f044b2d96960e70f815b2436183ba88084d94c0fc824e95694e7afe2c7"))))

(define-testcase ensure-that-compute-signature-works-as-expected ()
  (assert-string=
   "5c4446f0df8e87814c774517f95d970b46dbf25d9279b6b8b418b779f817ef5e"
   (let ((signature::*signature-algorithm*
	   '(:external-format :utf-8
	     :mac :hmac
	     :digest :sha256
	     :name "AWS4")))
     (signature::compute-signature
      :key "ThisIsNotAnActualSecretAccessKey"
      :request-date "20180527T093358Z"
      :region :global
      :service :testsuite
      :string-to-sign "AWS4-HMAC-SHA256
20180527T093358Z
20180527/global/testsuite/aws4_request
e21858f044b2d96960e70f815b2436183ba88084d94c0fc824e95694e7afe2c7"))))

(define-testcase testsuite-signature ()
  (ensure-that-create-canonical-request-works-as-expected)
  (ensure-that-create-string-to-sign-works-as-expected)
  (ensure-that-compute-signature-works-as-expected))

;;;; End of file `signature.lisp'
