;;;; content.lisp — Content Output Testsuite

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

(define-testcase testsuite-content ()
  (flet ((latin-small-ligature-oe (external-format)
	   (webmachine:with-content-output-to-sequence (buffer :external-format external-format)
	     (write-char #\latin_small_ligature_oe buffer))))
    (let ((iso-8859-15
	    (latin-small-ligature-oe :iso-8859-15)))
      (assert= 1 (length iso-8859-15))
      (assert= #xBD (aref iso-8859-15 0)))
    (let ((utf-8
	    (latin-small-ligature-oe :utf-8)))
      (assert= 2 (length utf-8))
      (assert= #xC5 (aref utf-8 0))
      (assert= #x93 (aref utf-8 1)))))
      
;;;; End of file `content.lisp'
