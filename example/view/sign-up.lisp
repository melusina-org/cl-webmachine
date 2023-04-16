;;;; sign-up.lisp — Sign-up View Webmachine Example

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

(defun sign-up-form ()
  (who:with-html-output (*standard-output*)
    (:form :action "/sign-up/submit" :method :post
     (:div :class "mb-3"
	   (:label :for "sign-up-name"
		   :class "form-label"
		   "Name")
	   (:input :type "text" :class "form-control"
		   :id "sign-up-name"
		   :name "name"
		   :aria-describedby "sign-up-name-help")
	   (:div :id "sign-up-name-help"
		 :class "form-text"
		 "The name, as it should be displayed on screen."))
     (:div :class "mb-3"
	   (:label :for "sign-up-email"
		   :class "form-label"
		   "Email address")
	   (:input :type "email" :class "form-control"
		   :id "sign-up-email"
		   :name "email"
		   :aria-describedby "sign-up-email-help")
	   (:div :id "sign-up-email-help"
		 :class "form-text"
		 "We'll never share your email with anyone else."))
     (:div :class "mb-3"
	   (:label :for "sign-up-password"
		   :class "form-label"
		   "Password")
	   (:input :type "password" :class "form-control"
		   :id "sign-up-password"
		   :name "password"
		   :aria-describedby "sign-up-password-help")
	   (:div :id "sign-up-password-help"
		 :class "form-text"
		 "Choose a strong password."))
     (:div :class "mb-3 form-check"
	   (:input :type "checkbox"
		   :class "form-check-input"
		   :id "administrator"
		   :name "administrator")
	   (:label :class "form-check-label"
		   :for "administrator"
		   "Administrator"))
     (:button :type "submit"
	      :class "btn btn-primary"
	      "Submit"))))

(hunchentoot:define-easy-handler (sign-up :uri "/sign-up") ()
  (html-page (:title "Sign-up for Webmachine Example")
    (html-page-title "Sign-up")
    (:div :class "card p-4 my-4"
	  (sign-up-form))))

(hunchentoot:define-easy-handler (sign-up-submit :uri "/sign-up/submit")
    (name email password administrator)
  (add-user
   :name name
   :email email
   :password password
   :administrator administrator)
  (hunchentoot:redirect "/user"))

;;;; End of file `sign-up.lisp'
