;;;; media-type.lisp — Repository of Media Types or MIME Types for Webmachine

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

(define-testcase ensure-important-mime-types-for-web-developers-are-known ()
  (let ((important-mime-types
	  '("application/octet-stream"
	    "text/plain"
	    "text/css"
	    "text/html"
	    "text/javascript")))
    (loop :for important-mime-type :in important-mime-types
	  :do (assert-t* (webmachine:find-media-type important-mime-type)))))

(define-testcase test-find-media-type-polymorphism ()
  (let ((media-type
	  (webmachine:define-media-type "application/testsuite"
	    :suffixes nil
	    :description "A media type that represents a test purpose."))
	(lookup-keys
	  '("application/testsuite"
	    "application/testsuite;q=0."
	    :application/testsuite)))
    (dolist (lookup-key lookup-keys)
      (assert-eq media-type (webmachine:find-media-type lookup-key)))))

(define-testcase testsuite-media-type ()
  (ensure-important-mime-types-for-web-developers-are-known)
  (test-find-media-type-polymorphism))

;;;; End of file `media-type.lisp'
