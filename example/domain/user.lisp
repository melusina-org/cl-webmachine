;;;; user.lisp — User for Webmachine Example

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

(defclass user ()
  ((id
    :initarg :id
    :reader id)
   (name
    :initarg :name)
   (email
    :initarg :email
    :accessor email)
   (password
    :initarg :password)
   (administrator
    :initarg :administrator)))

(defvar *users* nil)

(defun add-user (&rest initargs &key name email password administrator)
  (declare (ignore name email password administrator))
  (flet ((next-id ()
	   (if *users*
	       (1+ (slot-value (first *users*) 'id))
	       0)))
    (push (apply #'make-instance 'user :id (next-id) initargs) *users*)))

(defun find-user (designator)
  "Find a user by ID or EMAIL."
  (typecase designator
    (string
     (find designator *users* :test #'string-equal :key #'email))
    (integer
     (find designator *users* :test #'= :key #'id))))

(defun verify-user-password (user password)
  "Verify if PASSWORD is legitimate for USER."
  (unless user
    (return-from verify-user-password nil))
  (string-equal password (slot-value user 'password)))

;;;; End of file `user.lisp'
