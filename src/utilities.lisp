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

(in-package #:org.melusina.webmachine)


;;;;
;;;; STRING-MATCH
;;;;

(defun string-match (pattern text)
  "Predicate recognising TEXT matching a globbing PATTERN."
  (let ((text-length (length text))
        (pattern-length (length pattern)))
    (labels
        ((match-step (i j)
           (case (when (and (<= j text-length) (< i pattern-length))
		   (elt pattern i))
             ((nil)
              (eq j text-length))
             (#\?
              (and (< j text-length) (match-step (1+ i) (1+ j))))
             (#\*
	      (or (match-step (1+ i) j) (match-step i (1+ j))))
             (t
              (when (< j text-length)
                (and (char= (elt pattern i) (elt text j))
                     (match-step (1+ i) (1+ j))))))))
      (match-step 0 0))))

;;;; End of file `utilities.lisp'
