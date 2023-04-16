;;;; login.lisp — login View Webmachine Example

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

(defun login-form ()
  (who:with-html-output (*standard-output*)
    (:form :action "/login/submit" :method :post
     (:div :class "mb-3"
	   (:label :for "login-email"
		   :class "form-label"
		   "Email address")
	   (:input :type "email" :class "form-control"
		   :id "login-email"
		   :name "email"
		   :aria-describedby "login-email-help")
	   (:div :id "login-email-help"
		 :class "form-text"
		 "We'll never share your email with anyone else."))
     (:div :class "mb-3"
	   (:label :for "login-password"
		   :class "form-label"
		   "Password")
	   (:input :type "password" :class "form-control"
		   :id "login-password"
		   :name "password"
		   :aria-describedby "login-password-help")
	   (:div :id "login-password-help"
		 :class "form-text"
		 "Choose a strong password."))
     (:button :type "submit"
	      :class "btn btn-primary"
	      "Submit"))))

(hunchentoot:define-easy-handler (login :uri "/login") ()
  (html-page (:title "Login for Webmachine Example")
    (html-page-title "Login")
    (:div :class "card p-4 my-4"
	  (login-form))))

(hunchentoot:define-easy-handler (login-submit :uri "/login/submit")
    (email password)
  (if (verify-user-password (find-user email) password)
      (progn
	(hunchentoot:start-session)
	(hunchentoot:redirect "/about"))
      (reply-forbidden)))

;;;; End of file `login.lisp'
