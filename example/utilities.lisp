;;;; utilities.lisp — Utilities for Webmachine

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro join (&body strings)
    "Join STRINGS into a single string, using spaces."
    (with-output-to-string (buffer)
      (loop :for string :in strings
	    :for separator = "" :then " "
	    :do (progn
		  (write-string separator buffer)
		  (write-string string buffer))))))

;;;; End of file `utilities.lisp'
