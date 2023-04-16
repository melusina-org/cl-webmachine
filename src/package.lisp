;;;; package.lisp — Package for Webmachine

;;;; Webmachine (https://github.com/melusina-org/cl-webmachine)
;;;; This file is part of Webmachine.
;;;;
;;;; Copyright © 2018–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.webmachine
  (:use #:cl)
  (:import-from #:trivia #:match)
  (:import-from
   #:alexandria
   #:ensure-list
   #:with-unique-names)
  (:export
   ;; Configuration
   #:*catch-errors*
   ;; Condition
   #:http-condition
   #:http-condition-status-code
   #:http-condition-short-description
   #:http-error
   ;; Content
   #:make-content-output-stream
   #:with-content-output-to-sequence
   ;; Request
   #:request
   #:path-parameters
   #:language
   #:charset
   #:encoding
   #:get-request
   #:head-request
   #:post-request
   #:put-request
   #:delete-request
   #:connect-request
   #:options-request
   #:trace-request
   #:patch-request
   ;; Reply
   #:reply
   #:application-reply
   #:audio-reply
   #:font-reply
   #:image-reply
   #:model-reply
   #:text-reply
   #:video-reply
   #:application/octet-stream-reply
   #:text/css-reply
   #:text/html-reply
   #:text/javascript-reply
   #:text/plain-reply
   #:image/jpeg-reply
   #:image/png-reply
   #:image/svg+xml-reply
   ;; Repository of Request Methods
   #:request-method
   #:name
   #:destructive-p
   #:description
   #:request-method-p
   #:make-request-method
   #:define-request-method
   #:find-request-method
   #:remove-request-method
   #:list-request-methods
   #:describe-request-methods
   ;; Repository of Media Types
   #:media-type
   #:media-type-name
   #:media-type-description
   #:media-type-p
   #:make-media-type
   #:define-media-type
   #:find-media-type
   #:remove-media-type
   #:list-media-types
   #:describe-media-types
   #:match-media-type-p
   ;; Repository of Web Resources
   #:resource
   #:resource-p
   #:find-resource
   #:remove-resource
   ;; HTTP Protocol Semantic of Resource Operations 
   #:resource-available-p
   #:resource-exists-p
   #:resource-known-methods
   #:resource-uri-too-long-p
   #:resource-payload-too-large-p
   #:resource-allowed-methods
   #:resource-valid-request-p
   #:resource-valid-content-headers-p
   #:resource-valid-content-type-p
   #:resource-authorized-p
   #:resource-forbidden-p
   #:resource-options
   #:resource-content-types-provided
   #:resource-languages-provided
   #:resource-encodings-provided
   #:resource-charsets-provided
   #:resource-to-text/html
   ;; Acceptor
   #:acceptor
   #:ssl-acceptor
   #:make-acceptor
  ))

(in-package #:org.melusina.webmachine)

;;;; End of file `package.lisp'
