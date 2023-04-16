;;;; user.lisp — User View Webmachine Example

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

(defun html-user-row (user)
  (flet ((user-location (id)
	   (format nil "window.location='/user/~A'" id)))
    (with-slots (id name email administrator) user
      (who:with-html-output (*standard-output*)
	(:tr :onclick (user-location id)
	     (:th :scope "row" (who:str id))
	     (:td (who:str name))
	     (:td (who:str email))
	     (:td (who:str (if administrator "Yes" "No"))))))))

(defun html-user-table ()
  (who:with-html-output (*standard-output*)
    (:table
     :class "table table-hover table-striped"
     (:thead
      (:tr
       (:th :scope "col" "#")
       (:th :scope "col" "Name")
       (:th :scope "col" "Email")
       (:th :scope "col" "Administrator")))
     (:tbody
      (loop :for user :in *users*
	    :do (html-user-row user))))))

(defun add-user-form (&key dismiss-button)
  (who:with-html-output (*standard-output*)
    (:form :action "/user/add" :method :post
	   (:div :class "mb-3"
		 (:label :for "add-user-name"
			 :class "form-label"
			 "Name")
		 (:input :type "text" :class "form-control"
			 :id "add-user-name"
			 :name "name"
			 :aria-describedby "add-user-name-help")
		 (:div :id "add-user-name-help"
		       :class "form-text"
		       "The name, as it should be displayed on screen."))
	   (:div :class "mb-3"
		 (:label :for "add-user-email"
			 :class "form-label"
			 "Email address")
		 (:input :type "email" :class "form-control"
			 :id "add-user-email"
			 :name "email"
			 :aria-describedby "add-user-email-help")
		 (:div :id "add-user-email-help"
		       :class "form-text"
		       "We'll never share your email with anyone else."))
	   (:div :class "mb-3"
		 (:label :for "add-user-password"
			 :class "form-label"
			 "Password")
		 (:input :type "password" :class "form-control"
			 :id "add-user-password"
			 :name "password"
			 :aria-describedby "add-user-password-help")
		 (:div :id "add-user-password-help"
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
		    "Submit")
	   (when dismiss-button
	     (who:htm
	      (:button :type "button"
		       :class "btn btn-secondary ms-2"
		       :data-bs-dismiss "modal"
		       "Dismiss"))))))

(defun add-user-button ()
  (who:with-html-output (*standard-output*)
    (:button :type "button" :class "btn btn-outline-primary me-2"
	     :data-bs-toggle "modal"
	     :data-bs-target "#add-user-dialog"
	     "Add a User")
    (:div :class "modal fade" :id "add-user-dialog"
	  :data-bs-keyboard "false"
	  :tabindex "-1"
	  :aria-hidden "true"
	  (:div :class "modal-dialog modal-dialog-centered"
		(:div :class "p-5 modal-content"
		      (html-modal-title "Add a User")
		      (add-user-form :dismiss-button t))))))

(defun close-all-sessions-button ()
  (who:with-html-output (*standard-output*)
    (:button :type "button" :class "btn btn-outline-danger"
	     :data-bs-toggle "modal"
	     :data-bs-target "#close-all-sessions-dialog"
	     "Close all Sessions")
    (:div :class "modal fade" :id "close-all-sessions-dialog"
	  :data-bs-keyboard "false"
	  :tabindex "-1"
	  :aria-hidden "true"
	  (:div :class "modal-dialog modal-dialog-centered"
		(:div :class "p-5 modal-content"
		      (html-modal-title "Close all Sessions")
		      (:p
		       #.(join
			   "This terminates all user sessions."
			   "Every active user could loose unsaved work"
			   "and will need to reauthenticate."))
		      (:form :action "/user/close-all-sessions"
		       	     (:button :type "submit"
				      :class "btn btn-danger"
				      "Close all Sessions")
			     (:button :type "button"
				      :class "btn btn-secondary ms-2"
				      :data-bs-dismiss "modal"
				      "Dismiss")))))))

(defun html-user-toolbar ()
  (who:with-html-output (*standard-output*)
    (:div :class "d-grid gap-2 d-md-block"
	  (add-user-button)
	  (close-all-sessions-button)))) 

(hunchentoot:define-easy-handler (api-user-add :uri "/user/add")
    (name email password administrator)
  (with-valid-session
    (add-user
     :name name
     :email email
     :password password
     :administrator administrator)
    (hunchentoot:redirect "/user")))

(hunchentoot:define-easy-handler (api-user-close-all-sessions :uri "/user/close-all-sessions") ()
  (with-valid-session
    (hunchentoot:reset-sessions)
    (hunchentoot:redirect "/home")))

(hunchentoot:define-easy-handler (user-administration :uri "/user") ()
  (with-valid-session
    (html-page (:title "Users for Webmachine Example"
		   :navigation "Users")
      (html-page-title "User Administration")
      (html-user-toolbar)
      (html-page-section (:id "user-table" :title "User Table")
	(html-user-table)))))



;;;;
;;;; User Resource
;;;;

(defun view-user-html (user)
  (html-page (:title "Webmachine Example User " :navigation "User")
    (html-page-title "Webmachine Example User ~A" (id user))
    (who:with-html-output (*standard-output*)
      (:div :class "container"
	    (:div :class "p-5 mb-4 bg-light rounded-3"
		  (:div :class "container-fluid py-5"
			(:h1 :class "display-5 fw-bold"
			     (with-slots (name email) user
			       (who:fmt "~A <~A>" name user)))
			(:p :class "fs-4"
			    (if (slot-value user 'administrator)
				(who:str "This user has the right to adminstrate the system.")
				(who:str "This is just a normal user.")))))))))

(defclass user-resource (webmachine:resource)
  nil
  (:default-initargs
   :name 'user
   :path "/user/:id")
  (:documentation "The user Web resource."))

(defmethod webmachine:resource-exists-p and ((instance user-resource) request)
  (declare (ignore instance))
  (webmachine:with-path-parameters (id) request
    (find-user (parse-integer id))))

(defmethod webmachine:resource-forbidden-p or ((instance user-resource) request)
  (declare (ignore instance request))
  (not hunchentoot:*session*))

(defmethod webmachine:write-resource-response ((instance user-resource)
					       (request webmachine:get-request)
					       (reply webmachine:text/html-reply)
					       destination)
  (declare (ignore instance))
  (webmachine:with-path-parameters (id) request
    (let ((user
	    (find-user (parse-integer id))))
      (let ((*standard-output* destination))
	(view-user-html user)))))
      
;;;; End of file `user.lisp'
