;;;; html.lisp — Entrypoint for Webmachine

;;;; Webmachine (https://github.com/melusina-org/cl-webmachine)
;;;; This file is part of Webmachine.
;;;;
;;;; Copyright © 2018–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:webmachine/server)


;;;;
;;;; HTML Page
;;;;

(defmacro html-page ((&key title (lang "en") (mathjax nil) (chartjs nil))
		     &body body)
  `(who:with-html-output-to-string (*standard-output* nil :prologue t :indent nil)
     (:html
      :lang ,lang
      (:head
       (:meta :charset "utf-8")
       (:title ,title)
       (:link
	:href "/bootstrap/css/bootstrap.min.css"
	:rel "stylesheet")
       (write-mathjax-configuration ,mathjax)
       (write-chartjs-configuration ,chartjs))
      (:body
       ,@body
       (:script
	:src "/bootstrap/js/bootstrap.bundle.min.js"
	:type "text/javascript")))))

;;;; End of file `html.lisp'
