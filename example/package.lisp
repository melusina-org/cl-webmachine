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

(defpackage #:org.melusina.webmachine/example
  (:use #:common-lisp)
  (:local-nicknames
   (#:who #:cl-who)
   (#:css #:cl-css)
   (#:webmachine #:org.melusina.webmachine)
   (#:server #:org.melusina.webmachine/server))
  (:export
   
   #:toplevel))

(in-package #:org.melusina.webmachine/example)

;;;; End of file `package.lisp'