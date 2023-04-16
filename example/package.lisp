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
  (:use #:common-lisp #:parenscript)
  (:local-nicknames
   (#:who #:cl-who)
   (#:css #:cl-css)
   (#:webmachine #:org.melusina.webmachine)
   (#:asset #:org.melusina.webmachine/asset)
   (#:server #:org.melusina.webmachine/server))
  (:import-from
   #:org.melusina.webmachine
   #:define-constant-resource)
  (:export
   #:functional-state-javascript
   #:start
   #:stop
   #:create-users
   #:toplevel))

(in-package #:org.melusina.webmachine/example)

;;;; End of file `package.lisp'
