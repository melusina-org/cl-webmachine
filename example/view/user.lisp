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


;;;;
;;;; User Index
;;;;

(defun user-index-javascript ()
  (who:with-html-output (*standard-output*)
    (:script
     :id "user-index-javascript"
     :type "text/javascript"
     (who:str
      (ps
 	(defun make-user-initial-state ()
	  (create :busy nil))
	(defun user-busy-p (state)
	  (getprop state 'busy))
	(defun (setf user-busy-p) (new-value state)
	  (setf (getprop state 'busy) new-value))
	(defun action-name (action)
	  (getprop action 'name))
	(defun user-transition (action state)
	  (case (action-name action)
	    (:enable
	     (setf (user-busy-p state) nil))
	    (:disable
	     (setf (user-busy-p state) t))
	    (:demonstrate-busy
	     (setf (user-busy-p state) t)
	     (chain
	      (fetch
	       "http://webmachine.melusina.local:8080/user"
	       (create 'method "GET"))
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
		       'headers (create "Content-Type" "application/json")
		       'body (json-stringify (create 'add-user-request (getprop action 'form-data)))))
	      (then (lambda (response)
		      (declare (ignore response))
		      (dispatch-application-action :enable)
		      (chain window location (replace "http://webmachine.melusina.local:8080/user"))))))
	    (:add-sample-user
 	     (flet ((make-sample-user-definition ()
		      (create 'name "Sample User"
			      'email "sample.user@melusina.invalid"
			      'password "*"
			      'administrator "on")))
	       (dispatch-application-action
		(create 'name :add-user
			'form-data (make-sample-user-definition)))))
	    (:close-all-sessions
	     (setf (user-busy-p state) t)
	     (chain
	      (fetch
	       "http://webmachine.melusina.local:8080/user"
	       (create 'method "POST"
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
	(defun connect-data-wm-controller (event)
	  "Connect all nodes with a controller attribute have their `disabled' attribute changing with the busy state.
A controller attribute is one of
  data-wm-dispatch
  data-wm-submit
  data-wm-controller"
	  (declare (ignore event))
	  (flet ((query ()
		   (chain document (query-selector-all "button[data-wm-dispatch],button[data-wm-submit],[data-wm-controller]"))))
	    (dolist (element (query))
	      (debug (create 'name 'connect-data-wm-controller 'element element))
	      (connect-application element :state #'update-disabled-property))))
	(chain window (add-event-listener "load" #'connect-data-wm-controller))
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
		    :data-wm-submit :add-user
		    :data-bs-dismiss "modal"
		    "Submit")
	   (when dismiss-button
	     (who:htm
	      (:button :type "button"
		       :class "btn btn-secondary ms-2"
		       :data-bs-dismiss "modal"
		       "Dismiss"))))))

(defun add-user-button ()
  (who:with-html-output (*standard-output*)
    (:button :type "button" :class "btn btn-outline-primary"
	     :id "add-user-button"
	     :data-bs-toggle "modal"
	     :data-bs-target "#add-user-dialog"
	     :data-wm-controller t
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
    (:button :type "button" :class "btn btn-outline-danger ms-2"
	     :data-wm-controller t
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
				:data-wm-dispatch :close-all-sessions
				"Close all Sessions")
		       (:button :type "button"
				:class "btn btn-secondary ms-2"
				:data-bs-dismiss "modal"
				"Dismiss")))))))

(defun add-sample-button ()
  (who:with-html-output (*standard-output*)
    (:button :type "button"
	     :id "add-sample-user"
	     :class "btn btn-outline-secondary ms-2"
	     :data-wm-dispatch :add-sample-user
	     "Add a Sample User")))

(defun demonstrate-busy-button ()
  (who:with-html-output (*standard-output*)
    (:button :type "button"
	     :id "demonstrate-busy"
	     :class "btn btn-outline-secondary ms-2"
	     :data-wm-dispatch "demonstrate-busy"
	     "Demonstrate Busy")))

(defun html-user-index-toolbar ()
  (who:with-html-output (*standard-output*)
    (:div :class "d-grid gap-2 d-md-block"
	  (add-user-button)
	  (close-all-sessions-button)
	  (add-sample-button)
	  (demonstrate-busy-button))))

(defclass user-index (webmachine:resource)
  nil
  (:default-initargs
   :name 'user-index
   :path "/user")
  (:documentation "The user index resource."))

(defmethod webmachine:resource-flexible-negotiation-p ((instance user-index) request)
  t)

(defmethod webmachine:resource-payload-too-large-p ((instance user-index) request)
  (let* ((content-length-header
	   (hunchentoot:header-in* :content-length))
	 (content-length
	   (when content-length-header
	     (parse-integer content-length-header :junk-allowed t)))
	 (maximal-content-length 4096))
    (and content-length (> content-length maximal-content-length))))

(defmethod webmachine:resource-content-types-provided ((instance user-index) request)
  '(:text/html))

(defmethod webmachine:resource-languages-provided ((instance user-index) request)
  '(:en-US))

(defmethod webmachine:resource-allowed-methods ((instance user-index))
  '(:get :post))

(defmethod webmachine:resource-forbidden-p or ((instance user-index) request)
  (declare (ignore instance request))
  (not hunchentoot:*session*))

(defmethod webmachine:resource-valid-content-type-p and ((instance user-index) (request webmachine:post-request))
  (let ((content-type
	  (hunchentoot:header-in* :content-type)))
    (when content-type
      (eq (webmachine:find-media-type content-type)
	  (webmachine:find-media-type :application/json)))))

(defmethod webmachine:write-resource-response ((instance user-index)
					       (request webmachine:get-request)
					       (reply webmachine:text/html-reply)
					       response-body)
  (declare (ignore instance))
  (html-page (:title "Users for Webmachine Example"
	      :navigation "Users")
    (utilities-javascript)
    (functional-state-javascript)
    (user-index-javascript)
    (html-page-title "User Index")
    (html-user-index-toolbar)
    (html-page-section (:id "user-table" :title "User Table")
      (html-user-table))))

(defmethod webmachine:write-resource-response ((instance user-index)
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
;;;; User Detail
;;;;

(defun user-detail-javascript (user)
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
	(defun user-transition (action state)
	  (case (getprop action 'name)
	    (:enable
	     (setf (user-busy-p state) nil))
	    (:disable
	     (setf (user-busy-p state) t))
	    (:delete-user
	     (setf (user-busy-p state) t)
	     (chain
	      (fetch *route*
		     (create 'method "DELETE"
			     'headers (create "Content-Type" "application/json")))
	      (then (lambda (response)
		      (cond
			((getprop response 'redirected)
			 (chain window location (replace (getprop response 'url))))
			((getprop response 'ok)
			 (dispatch-application-action (create 'name :enable))
			 (chain window location (replace "http://webmachine.melusina.local:8080/user")))
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
	  (html-delete-user-button user))))

(defun html-delete-user-button (user)
  (who:with-html-output (*standard-output*)
    (:button :type "button" :class "btn btn-outline-danger"
	     :id "delete-user-entrypoint"
	     :data-bs-toggle "modal"
	     :data-bs-target "#delete-user-confirmation"
	     "Remove")
    (:div :class "modal fade" :id "delete-user-confirmation"
	  :data-bs-keyboard "false"
	  :tabindex "-1"
	  :aria-hidden "true"
	  (:div :class "modal-dialog modal-dialog-centered"
		(:div :class "p-5 modal-content"
		      (html-modal-title "Remove User")
		      (html-user-table (list user))
		      (:div :class "container"
			    (:button :type "submit"
				     :id "delete-user-submit"
				     :class "btn btn-danger"
				     :data-bs-dismiss "modal"
				     :data-wm-dispatch "delete-user"
				     "Remove")
			    (:button :type "button"
				     :class "btn btn-secondary ms-2"
				     :data-bs-dismiss "modal"
				     "Dismiss")))))))

(defun view-user-detail (user)
  (html-page (:title "Webmachine User Details" :navigation "User")
    (utilities-javascript)
    (functional-state-javascript)
    (user-detail-javascript user)
    (html-page-title "User Details for ~A" (id user))
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

(defclass user-detail (webmachine:resource)
  nil
  (:default-initargs
   :name 'user
   :path "/user/:id")
  (:documentation "The user administration resource."))

(defmethod webmachine:resource-flexible-negotiation-p ((instance user-detail) request)
  t)

(defmethod webmachine:resource-content-types-provided ((instance user-detail) request)
  '(:text/html))

(defmethod webmachine:resource-languages-provided ((instance user-detail) request)
  '(:en-US))

(defmethod webmachine:resource-allowed-methods ((instance user-detail))
  '(:get :head :delete))

(defmethod webmachine:resource-exists-p and ((instance user-detail) request)
  (declare (ignore instance))
  (webmachine:with-path-parameters (id) request
    (find-user (parse-integer id))))

(defmethod webmachine:resource-forbidden-p or ((instance user-detail) request)
  (declare (ignore instance request))
  (not hunchentoot:*session*))

(defmethod webmachine:write-resource-response ((instance user-detail)
					       (request webmachine:get-request)
					       (reply webmachine:text/html-reply)
					       response-body)
  (declare (ignore instance reply response-body))
  (webmachine:with-path-parameters (id) request
    (let ((user
	    (find-user (parse-integer id))))
      (view-user-detail user))))

(defmethod webmachine:write-resource-response ((instance user-detail)
					       (request webmachine:delete-request)
					       (reply webmachine:text/html-reply)
					       response-body)
  (declare (ignore instance))
  (webmachine:with-path-parameters (id) request
    (delete-user (parse-integer id))))


;;;;
;;;; API
;;;;

(defun make-user-index-resource ()
  (make-instance 'user-index))

(defun make-user-detail-resource ()
  (make-instance 'user-detail))

;;;; End of file `user.lisp'
