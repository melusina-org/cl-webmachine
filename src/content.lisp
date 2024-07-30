;;;; content.lisp — Content Output for Webmachine

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

(defun make-content-output-stream (raw-output-stream &key external-format encoding)
  "Make a content output stream writing to the underlying RAW-OUTPUT-STREAM.
The final output stream takes care of writing characters using the
specified EXTERNAL-FORMAT and of applying the given ENCODING.

Accepted values for EXTERNAL-FORMAT are external format designators
as recognised by the flexi-streams library. Accepted values for the
ENCODING parameter are

  :IDENTITY :GZIP :DEFLATE and :COMPRESS"
  (let ((external-output-stream
          (if external-format
              (flexi-streams:make-flexi-stream raw-output-stream :external-format external-format)
              raw-output-stream)))
    (ecase encoding
      ((nil :identity)
       external-output-stream)
      ((:gzip :deflate :compress)
       (http-error 501 "Encoding ~A not implemented" encoding)))))

(defmacro with-content-output-to-sequence ((var &key external-format encoding) &body body)
  "Create an in-memory content output stream for reply content and bind VAR to it in BODY.
The return value of this macro is a vector containing the octets resulting from
writing to VAR within BODY."
  (with-unique-names (private-buffer)
    `(let (,var)
       (unwind-protect
            (flexi-streams:with-output-to-sequence (,private-buffer)
              (setq ,var (make-content-output-stream ,private-buffer
                                                     :external-format ,external-format
                                                     :encoding ,encoding))
              ,@body)
         (when ,var (close ,var))))))

;;;; End of file `content.lisp'
