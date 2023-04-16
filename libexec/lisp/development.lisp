;;;; setup.lisp — Project setup for Webmachine

;;;; Webmachine (https://github.com/melusina-org/cl-webmachine)
;;;; This file is part of Webmachine.
;;;;
;;;; Copyright © 2018–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.webmachine/development
  (:use #:cl)
  (:local-nicknames
   (#:atelier #:org.melusina.atelier)
   (#:example #:org.melusina.webmachine/example))
  (:export
   #:lint))

(in-package #:org.melusina.webmachine/development)

(defparameter *parameter-bindings*
  '((:copyright-holder . "Michaël Le Barbier")
    (:copyright-year . "2018–2023")
    (:project-filename . "org.melusina.webmachine")
    (:project-name . "Webmachine")
    (:project-description . "HTTP Semantic Awareness on top of Hunchentoot")
    (:project-long-description .
	 #.(concatenate 'string
	    "Webmachine is an application layer that adds HTTP semantic awareness "
	    "on top of the excellent bit-pushing and HTTP syntax-management provided "
	    "by Hunchentoot, and provides a simple and clean way to connect that "
	    "to your application's behavior."

	    "The design is inspired by the mythic Webmachine of Erlang "
	    "and honours it with that name."))
    (:homepage . "https://github.com/melusina-org/cl-webmachine")
    (:license . :MIT)))

(defun lint ()
  (let ((atelier:*parameter-bindings* *parameter-bindings*))
    (atelier:lint
     #p"src"
     #p"testsuite"
     #p"server"
     #p"example"
     #p"development"
     #p"libexec/lisp/setup.lisp")))

;;;; End of file `setup.lisp'
