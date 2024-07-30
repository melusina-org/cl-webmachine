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

(in-package #:webmachine)


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


;;;;
;;;; File Content Type
;;;;

(defparameter *file-content-type-rules*
  '((:text/css "css")
    (:text/javascript "js")
    (:application/json "json" "map")
    (:image/svg+xml "svg")
    (:image/png "png")
    (:image/jpeg "jpeg" "jpg")
    (:font/woff "woff")))

(defparameter *file-content-type-table*
  (flet ((make-file-content-type-table (content-type-rules)
	   "Make a hash table whose content is based on CONTENT-TYPE-RULES."
	   (flet ((add-to-table (keys hash-table value)
		    (loop :for key :in keys
			  :do (setf (gethash key hash-table) value))))
	     (loop :with table = (make-hash-table :test #'equalp)
		   :for (mime-type . suffixes) :in content-type-rules
		   :do (add-to-table suffixes table mime-type)
		   :finally (return table)))))
    (make-file-content-type-table *file-content-type-rules*)))

(defun file-content-type (pathname)
  "Guess content-type for PATHNAME."
  (gethash (pathname-type pathname) *file-content-type-table*))


;;;;
;;;; SEND-FILE-CONTENTS
;;;;

(defun send-file-contents (pathname response-body &optional (buffer-size #.(* 4 1024)))
  "Send file contents of PATHNAME on stream RESPONSE-BODY.

Fail with Status code 500 when the file cannot be read. It assumes
that the ability to read the file has been otherwise verified and
that the file hence has been removed, which is an application logic
error or an environmental error."
  (with-open-file (file pathname
			:direction :input
			:if-does-not-exist nil
			:element-type '(unsigned-byte 8))
    (flet ((check-that-file-has-not-been-removed ()
	     (unless file
	       (http-error 500 "Cannot find file anymore.")))
	   (write-file-contents ()
	     (loop :with bytes-to-send = (or (file-length file) (http-error 500))
		   :with buffer = (make-array buffer-size :element-type '(unsigned-byte 8))
		   :while (> bytes-to-send 0)
		   :for expected-size = (min bytes-to-send buffer-size)
		   :for received-size = (read-sequence buffer file :end expected-size)
		   :do (progn
			 (unless (eql expected-size received-size)
			   (http-error 500 "Cannot read input from file"))
			 (write-sequence buffer response-body :end received-size)
			 (decf bytes-to-send received-size)))
	     (finish-output response-body)))
      (check-that-file-has-not-been-removed)
      (write-file-contents))))

;;;; End of file `utilities.lisp'
