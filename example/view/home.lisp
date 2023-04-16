;;;; home.lisp — Home View Webmachine Example

;;;; Webmachine (https://github.com/melusina-org/cl-webmachine)
;;;; This file is part of Webmachine.
;;;;
;;;; Copyright © 2018–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.webmachine/example)

(define-constant-resource (view-home :uri "/home" :content-type :text/html)
  (html-page (:title "Home for Webmachine Example"
	      :navigation "Home")
    (html-page-title "Webmachine")
    (who:with-html-output (*standard-output*)
      (:div :class "container"
	    (:div :class "p-5 mb-4 bg-light rounded-3"
		  (:div :class "container-fluid py-5"
			(:h1 :class "display-5 fw-bold"
			     "Home for Webmachine Example Application")
			(:p :class "fs-4"
			    #.(join
				"A Lisp system to develop web applications."
				"It is based on the book by Adam Tornhill,"
				"<em>Lisp for the WEB.</em>"))))))))

;;;; End of file `home.lisp'
