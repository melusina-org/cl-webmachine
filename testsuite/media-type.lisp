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

(in-package #:webmachine/testsuite)

(define-testcase ensure-media-types-are-known (media-types)
  (loop :for important-media-type :in media-types
	:do (assert-t* (webmachine:find-media-type important-media-type))))

(define-testcase ensure-repository-keys-are-keywords ()
  (loop :for key :being :the :hash-key :of webmachine::*media-type-repository*
	:do (assert-type key 'keyword)))

(define-testcase ensure-main-media-types-categories-are-correctly-defined ()
  (ensure-media-types-are-known '(:application :audio :font :image :model :text :video))
  (assert-nil (webmachine:find-media-type :example)))

(define-testcase ensure-important-media-types-for-web-developers-are-known ()
  (ensure-media-types-are-known
   '(:application/octet-stream
     :application/json
     :text/plain
     :text/css
     :text/html
     :text/javascript)))

(define-testcase ensure-important-image-media-types-are-known ()
  (ensure-media-types-are-known '(:image/png :image/jpeg :image/svg+xml)))

(define-testcase test-find-media-type-polymorphism ()
  (let ((media-type
	  (webmachine:define-media-type :application/testsuite
	    :reply-class 'webmachine:application-reply
	    :description "A media type that represents a test purpose."))
	(lookup-keys
	  '("application/testsuite"
	    "application/testsuite;q=0."
	    :application/testsuite)))
    (dolist (lookup-key lookup-keys)
      (assert-eq media-type (webmachine:find-media-type lookup-key)))))

(define-testcase testsuite-media-type ()
  (ensure-repository-keys-are-keywords)
  (ensure-important-media-types-for-web-developers-are-known)
  (ensure-important-image-media-types-are-known)
  (test-find-media-type-polymorphism))

;;;; End of file `media-type.lisp'
