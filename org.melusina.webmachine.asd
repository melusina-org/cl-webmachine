;;;; org.melusina.webmachine.asd — System definition for Webmachine

;;;; Webmachine (https://github.com/melusina-org/cl-webmachine)
;;;; This file is part of Webmachine.
;;;;
;;;; Copyright © 2018–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(asdf:defsystem #:org.melusina.webmachine
  :description "HTTP Semantic Awareness on top of Hunchentoot"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:alexandria
	       #:cl-ppcre
	       #:hunchentoot
	       #:parse-number
	       #:trivia)
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "utilities")
                 (:file "configuration")
                 (:file "condition")
                 (:file "content")
                 (:file "request")
                 (:file "reply")
                 (:file "request-method")
                 (:file "media-type")
                 (:file "path")
                 (:file "resource")
		 (:file "acceptor")))))

(asdf:defsystem #:org.melusina.webmachine/testsuite
  :description "Testsuite for the Common Lisp Webmachine"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:alexandria
	       #:cl-ppcre
	       #:drakma
	       #:org.melusina.confidence
	       #:org.melusina.webmachine)
  :components
  ((:module "testsuite"
    :components ((:file "package")
                 (:file "utilities")
                 (:file "configuration")
                 (:file "condition")
                 (:file "content")
                 (:file "request")
                 (:file "reply")
                 (:file "acceptor")
                 (:file "request-method")
                 (:file "media-type")
                 (:file "path")
                 (:file "resource")
		 (:file "entrypoint")))))

;;;; End of file `org.melusina.webmachine.asd'
