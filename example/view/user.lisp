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

(defun user-administration-javascript ()
  (who:with-html-output (*standard-output*)
    (:script
     :id "user-administration-javascript"
     :type "text/javascript"
     (who:str
      (ps
 	(defun make-user-initial-state ()
	  (create :busy nil))
	(defun user-busy-p (state)
	  (getprop state 'busy))
	(defun (setf user-busy-p) (new-value state)
	  (setf (getprop state 'busy) new-value))
	(defun user-transition (action state)
	  (case (getprop action 'name)
	    (:enable
	     (setf (user-busy-p state) nil))
	    (:disable
	     (setf (user-busy-p state) t))
	    (:demonstrate-busy
	     (setf (user-busy-p state) t)
	     (chain
	      (fetch
	       "http://webmachine.melusina.local:8080/user"
	       (create 'method "GET"
		       'mode "cors"))
	      (then (delay-promise 1000))
	      (then (lambda (response)
		      (declare (ignore response))
		      (dispatch-application-action :enable)))))
	    (:add-user
	     (setf (user-busy-p state) t)
	     (chain
	      (fetch
	       "http://webmachine.melusina.local:8080/user"
	       (create 'method "POST"
		       'mode "cors"
		       'headers (create "Content-Type" "application/json")
		       'body (json-stringify (create 'add-user-request (getprop action 'user-definition)))))
	      (then (lambda (response)
		      (declare (ignore response))
		      (dispatch-application-action :enable)))))
	    (:close-all-sessions
	     (setf (user-busy-p state) t)
	     (chain
	      (fetch
	       "http://webmachine.melusina.local:8080/user"
	       (create 'method "POST"
		       'mode "cors"
		       'headers (create "Content-Type" "application/json")
		       'body (json-stringify (create 'close-all-sessions-request (create)))))
	      (then (lambda (response)
		      (cond
			((getprop response 'redirected)
			 (chain window location (replace (getprop response 'url))))
			((getprop response 'ok)
			 (dispatch-application-action (create 'name :enable)))
			(t
			 (dispatch-application-action (create 'name :error "Cannot close all sessions."))))))
	      (then (lambda (response)
		      (declare (ignore response))
		      (dispatch-application-action :enable))))))
	  state)
	(install-application
	 (make-user-initial-state)
	 #'user-transition))))))

;;;;
;;;; User Rows
;;;;

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

(defun html-user-table (&optional (users *users*))
  (who:with-html-output (*standard-output*)
    (:table
     :class "table table-hover table-striped"
     :id "user-table"
     (:thead
      (:tr
       (:th :scope "col" "#")
       (:th :scope "col" "Name")
       (:th :scope "col" "Email")
       (:th :scope "col" "Administrator")))
     (:tbody
      (loop :for user :in users
	    :do (html-user-row user))))))

(defun add-user-form (&key dismiss-button)
  (who:with-html-output (*standard-output*)
    (:form :id "add-user-form"
	   :method :post
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
		    :id "add-user-submit"
		    :class "btn btn-primary"
		    :data-bs-dismiss "modal"
		    "Submit")
	   (when dismiss-button
	     (who:htm
	      (:button :type "button"
		       :class "btn btn-secondary ms-2"
		       :data-bs-dismiss "modal"
		       "Dismiss"))))
    (:script
     :type "text/javascript"
     (who:str
      (ps
	(defun on-add-user-submit (event)
	  (chain event (prevent-default))
	  (dispatch-application-action
	   (create 'name :add-user
		   'user-definition (form-data-as-dto "add-user-form"))))
	(connect-application "add-user-submit" :click #'on-add-user-submit))))))

(defun add-user-button ()
  (who:with-html-output (*standard-output*)
    (:button :type "button" :class "btn btn-outline-primary"
	     :id "add-user-button"
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
		      (add-user-form :dismiss-button t))))
    (:script
     :type "text/javascript"
     (who:str
      (ps
	(connect-application "add-user-button" :state #'update-disabled-property))))))

(defun close-all-sessions-button ()
  (who:with-html-output (*standard-output*)
    (:button :type "button" :class "btn btn-outline-danger ms-2"
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
		       #.(concatenate 'string
			   "This terminates all user sessions."
			   "Every active user could loose unsaved work"
			   "and will need to reauthenticate."))
		      (:form
		       (:button :type "submit"
				:id "close-all-sessions-submit"
				:class "btn btn-danger"
				:data-bs-dismiss "modal"
				"Close all Sessions")
		       (:button :type "button"
				:class "btn btn-secondary ms-2"
				:data-bs-dismiss "modal"
				"Dismiss")))))
    (:script
     :id "close-all-sessions-javascript"
     :type "text/javascript"
     (who:str
      (ps
	(defun on-close-all-sessions ()
	  (chain event (prevent-default))
 	  (dispatch-application-action
	   (create 'name :close-all-sessions)))
	(connect-application "close-all-sessions-submit" :click #'on-close-all-sessions)
	(connect-application "close-all-sessions-submit" :state #'update-disabled-property))))))

(defun add-sample-button ()
  (who:with-html-output (*standard-output*)
    (:button :type "button"
	     :id "add-sample-user"
	     :class "btn btn-outline-secondary ms-2"
	     "Add a Sample User")
    (:script
     :id "add-sample-button-javascript"
     :type "text/javascript"
     (who:str
      (ps
	(defun on-add-sample-user ()
 	  (flet ((make-sample-user-definition ()
		   (create 'name "Sample User"
			   'email "sample.user@melusina.invalid"
			   'password "*"
			   'administrator "on")))
	    (dispatch-application-action
	     (create 'name :add-user
		     'user-definition (make-sample-user-definition)))))
	(connect-application "add-sample-user" :click #'on-add-sample-user)
	(connect-application "add-sample-user" :state #'update-disabled-property))))))


(defun demonstrate-busy-button ()
  (who:with-html-output (*standard-output*)
    (:button :type "button"
	     :id "demonstrate-busy"
	     :class "btn btn-outline-secondary ms-2"
	     "Demonstrate Busy")
    (:script
     :type "text/javascript"
     (who:str
      (ps
	(defun on-demonstrate-busy ()
	  (dispatch-application-action
	   (create 'name :demonstrate-busy)))
	(connect-application "demonstrate-busy" :click #'on-demonstrate-busy)
	(connect-application "demonstrate-busy" :state #'update-disabled-property))))))


(defun html-user-administration-toolbar ()
  (who:with-html-output (*standard-output*)
    (:div :class "d-grid gap-2 d-md-block"
	  (add-user-button)
	  (close-all-sessions-button)
	  (add-sample-button)
	  (demonstrate-busy-button))))

(defclass user-administration (webmachine:resource)
  nil
  (:default-initargs
   :name 'user-administration
   :path "/user")
  (:documentation "The user administration resource."))

(defmethod webmachine:resource-flexible-negotiation-p ((instance user-administration) request)
  t)

(defmethod webmachine:resource-payload-too-large-p ((instance user-administration) request)
  (let* ((content-length-header
	   (hunchentoot:header-in* :content-length))
	 (content-length
	   (when content-length-header
	     (parse-integer content-length-header :junk-allowed t)))
	 (maximal-content-length 4096))
    (and content-length (> content-length maximal-content-length))))

(defmethod webmachine:resource-content-types-provided ((instance user-administration))
  '(:text/html))

(defmethod webmachine:resource-languages-provided ((instance user-administration))
  '(:en-US))

(defmethod webmachine:resource-allowed-methods ((instance user-administration))
  '(:get :post))

(defmethod webmachine:resource-forbidden-p or ((instance user-administration) request)
  (declare (ignore instance request))
  (not hunchentoot:*session*))

(defmethod webmachine:resource-valid-content-type-p and ((instance user-administration) (request webmachine:post-request))
  (let ((content-type
	  (hunchentoot:header-in* :content-type)))
    (when content-type
      (eq (webmachine:find-media-type content-type)
	  (webmachine:find-media-type :application/json)))))

(defmethod webmachine:write-resource-response ((instance user-administration)
					       (request webmachine:get-request)
					       (reply webmachine:text/html-reply)
					       response-body)
  (declare (ignore instance))
  (html-page (:title "Users for Webmachine Example"
	      :navigation "Users")
    (utilities-javascript)
    (functional-state-javascript)
    (user-administration-javascript)
    (html-page-title "User Administration")
    (html-user-administration-toolbar)
    (html-page-section (:id "user-table" :title "User Table")
      (html-user-table))))

(defmethod webmachine:write-resource-response ((instance user-administration)
					       (request webmachine:post-request)
					       (reply webmachine:text/html-reply)
					       response-body)
  (declare (ignore instance))
  (flet ((close-all-sessions ()
	   (hunchentoot:reset-sessions)
	   (hunchentoot:redirect "/home"))
	 (create-user (post-data)
	   (flet ((field (key &key (mandatory t) type)
		    (let ((field-value
			    (gethash key post-data)))
		      (case type
			('boolean
			 (setf field-value (and field-value t))))
		      (unless (or (not mandatory) field-value)
			(webmachine:http-error 400))
		      field-value)))
	     (add-user
	      :name (field "name")
	      :email (field "email")
	      :password (field "password")
	      :administrator (field "administrator" :type 'boolean :mandatory nil)))
	   (hunchentoot:redirect "/user")))
    (let ((post-data
	    (shasht:read-json
	     (hunchentoot:raw-post-data :request request :force-binary t :want-stream t))))
      (cond
	((not (typep post-data 'hash-table))
	 (webmachine:http-error 400))
	((gethash "addUserRequest" post-data)
	 (create-user (gethash "addUserRequest" post-data)))
	((gethash "closeAllSessionsRequest" post-data)
	 (close-all-sessions))
	(t
	 (webmachine:http-error 400))))))


;;;;
;;;; User Resource
;;;;

(defun user-state-javascript (user)
  (who:with-html-output (*standard-output*)
    (:script
     :id "user-state-javascript"
     :type "text/javascript"
     (who:str
      (ps
	(defvar *route*
	  (lisp (format nil "http://webmachine.melusina.local:8080/user/~A" (id user))))
 	(defun make-user-initial-state ()
	  (create :busy nil))
	(defun user-busy-p (state)
	  (getprop state 'busy))
	(defun (setf user-busy-p) (new-value state)
	  (setf (getprop state 'busy) new-value))
	(defun update-disabled-property (&key busy element)
	  (if busy
	      (disable-dom-element element)
	      (enable-dom-element element)))
	(defun user-transition (action state)
	  (case (getprop action 'name)
	    (:enable
	     (setf (user-busy-p state) nil))
	    (:disable
	     (setf (user-busy-p state) t))
	    (:remove-user
	     (setf (user-busy-p state) t)
	     (chain
	      (fetch *route*
		     (create 'method "DELETE"
			     'mode "cors"
			     'headers (create "Content-Type" "application/json")))
	      (then (lambda (response)
		      (cond
			((getprop response 'redirected)
			 (chain window location (replace (getprop response 'url))))
			((getprop response 'ok)
			 (dispatch-application-action (create 'name :enable)))
			(t
			 (dispatch-application-action (create 'name :error
							      'message (+ "Cannot remove user "
									  (lisp (id user)) ". "
									  (getprop response 'status-text)))))))))))
	  state)
	(install-application
	 (make-user-initial-state)
	 #'user-transition))))))

(defun html-user-toolbar (user)
  (who:with-html-output (*standard-output*)
    (:div :class "d-grid gap-2 d-md-block"
	  (html-remove-user-button user))))

(defun html-remove-user-button (user)
  (who:with-html-output (*standard-output*)
    (:button :type "button" :class "btn btn-outline-danger"
	     :id "remove-user-entrypoint"
	     :data-bs-toggle "modal"
	     :data-bs-target "#remove-user-confirmation"
	     "Remove")
    (:div :class "modal fade" :id "remove-user-confirmation"
	  :data-bs-keyboard "false"
	  :tabindex "-1"
	  :aria-hidden "true"
	  (:div :class "modal-dialog modal-dialog-centered"
		(:div :class "p-5 modal-content"
		      (html-modal-title "Remove User")
		      (html-user-table (list user))
		      (:div :class "container"
			    (:button :type "submit"
				     :id "remove-user-submit"
				     :class "btn btn-danger"
				     :data-bs-dismiss "modal"
				     "Remove")
			    (:button :type "button"
				     :class "btn btn-secondary ms-2"
				     :data-bs-dismiss "modal"
				     "Dismiss")))))
    (:script
     :type "text/javascript"
     (who:str
      (ps
	(defun find-dom-element (element)
	  (if (stringp element)
	      (chain document (get-element-by-id element))
	      element))
	(defun (setf event-listener) (callback element event)
	  (chain (find-dom-element element)
		 (add-event-listener event callback)))
	(defun console-log-response (response)
	  (chain console (debug response)))
	(defun on-click-remove (event)
	  (dispatch-application-action (create 'name :remove-user)))
	(connect-application "remove-user-submit" :click #'on-click-remove)
	(connect-application "remove-user-entrypoint" :state #'update-disabled-property))))))

(defun view-user-html (user)
  (html-page (:title "Webmachine Example User " :navigation "User")
    (utilities-javascript)
    (functional-state-javascript)
    (user-state-javascript user)
    (html-page-title "User ~A" (id user))
    (html-user-toolbar user)
    (:div :class "container my-4"
	  (:div :class "p-5 mb-4 bg-light rounded-3"
		(:div :class "container-fluid py-5"
		      (:h1 :class "display-5 fw-bold"
			   (with-slots (name email id) user
			     (who:fmt "~A &lt;~A&gt;" name email)))
		      (:p :class "fs-4"
			  (if (slot-value user 'administrator)
			      (who:str "This user has the right to adminstrate the system.")
			      (who:str "This is just a normal user."))))))))

(defclass user-resource (webmachine:resource)
  nil
  (:default-initargs
   :name 'user
   :path "/user/:id")
  (:documentation "The user Web resource."))

(defmethod webmachine:resource-flexible-negotiation-p ((instance user-resource) request)
  t)

(defmethod webmachine:resource-content-types-provided ((instance user-resource))
  '(:text/html))

(defmethod webmachine:resource-languages-provided ((instance user-resource))
  '(:en-US))

(defmethod webmachine:resource-allowed-methods ((instance user-resource))
  '(:get :delete))

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
					       response-body)
  (declare (ignore instance))
  (webmachine:with-path-parameters (id) request
    (let ((user
	    (find-user (parse-integer id))))
      (view-user-html user))))

(defmethod webmachine:write-resource-response ((instance user-resource)
					       (request webmachine:delete-request)
					       (reply webmachine:text/html-reply)
					       response-body)
  (declare (ignore instance))
  (webmachine:with-path-parameters (id) request
    (delete-user (parse-integer id))
    (hunchentoot:redirect "/user")))

;;;; End of file `user.lisp'
