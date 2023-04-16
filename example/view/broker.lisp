;;;; broker.lisp — Event Broker Webmachine Example

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

(defun broker-close-all-sessions-button (&key margin)
  (who:with-html-output (*standard-output*)
    (:button :type "button" :class (bootstrap-margin "btn btn-outline-danger" margin)
	     :id "close-all-sessions"
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
		      (:form :action "#"
		       	     (:button :type "submit"
				      :class "btn btn-danger"
				      "Close all Sessions")
			     (:button :type "button"
				      :class "btn btn-secondary ms-2"
				      :data-bs-dismiss "modal"
				      "Dismiss")))))
    (:script
     :id "close-all-sessions"
     :type "text/javascript"
     (who:str
      (ps
	 (defun update-close-all-sessions (&key busy element)
	   (if busy
	       (disable-dom-element element)
	       (enable-dom-element element))))))))

(defun broker-toolbar ()
  (who:with-html-output (*standard-output*)
    (:div :class "container"
	  (:div :class "d-grid gap-2 d-md-block mt-1 mb-3"
		(:button :type "button"
			 :id "toolbar-enable"
			 :class (bootstrap-margin "btn btn-outline-secondary" :end)
			 "Enable")
		(:button :type "button"
			 :id "toolbar-disable"
			 :class (bootstrap-margin "btn btn-outline-secondary" :start)
			 "Disable")
		(broker-close-all-sessions-button :margin :start)))))

(defun webmachine-javascript ()
  (who:with-html-output (*standard-output*)
    (:script
     :id "webmachine-javascript"
     :type "text/javascript"
     (who:str
      (ps
	(defun object-to-plist (object)
	  (chain -object (entries object) (flat)))
	(defvar *application* nil)
	(defun make-application (initial-state transition)
	  (create :bus (new -Event-Target)
		  :state initial-state
		  :transition transition))
	(defun error (details)
	  (chain console (log details)))
	(defun debug (debug)
	  (chain console (log debug)))
	(defun find-dom-element (element)
	  (if (stringp element)
	      (or
	       (chain document (get-element-by-id element))
	       (error (+ "No element with id " element)))
	      element))
	(defun receive-webmachine-action (event)
	  (with-slots (bus state transition) *application*
	    (let ((next-state
		    (funcall transition (getprop event :detail) state)))
	      (setf state next-state)
	      (chain bus (dispatch-event (new (-custom-event "state" (create :detail state))))))))
	(defun install-webmachine (initial-state transition)
	  (unless *application*
	    (setf *application* (make-application initial-state transition))
	    (with-slots (bus) *application*
	      (chain bus (add-event-listener
			  :action
			  #'receive-webmachine-action)))))
	(defun dispatch-webmachine (detail)
	  (let ((event
		  (new (-custom-event "action" (create :detail detail)))))
	    (with-slots (bus) *application*
	      (chain bus (dispatch-event event)))))
	(defun connect-webmachine-state (element event callback)
	  (with-slots (bus) *application*
	    (chain bus (add-event-listener
			:state
			(lambda (state)
			  (apply callback :element element (object-to-plist (getprop state :detail))))))))
	(defun connect (element event callback)
	  (case event
	    (:state
	     (connect-webmachine-state (find-dom-element element) event callback))
	    (t
	     (chain (find-dom-element element)
		    (add-event-listener event callback))))))))))

(defun application-state ()
  (who:with-html-output (*standard-output*)
    (:script
     :id "application-state"
     :type "text/javascript"
     (who:str
      (ps
	(defun make-initial-state ()
	  (create :busy nil))
	(defun application-busy-p (state)
	  (getprop state :busy))
	(defun (setf application-busy-p) (new-value state)
	  (setf (getprop state :busy) new-value))
	(defun next-state (action state)
	  (case (getprop action :name)
	    (:enable
	     (setf (application-busy-p state) nil))
	    (:disable
	     (setf (application-busy-p state) t)))
	  state))))))

(defun broker-event-sink ()
  (who:with-html-output (*standard-output*)
    (:div
     :class "container"
     (:code :id "event-sink")
     (:script
      :type "text/javascript"
      (who:str
       (ps
	 (defun make-broker (initial-state reducer)
	   (let ((broker
		   (new -Event-Target)))
	     (setf (@ broker :history)
		   initial-state)
	     (setf (@ broker reducer)
		   reducer)
	     broker))
	 (var *broker*
	      (make-broker))
	 (defun html-empty-sink ()
	   (who-ps-html (:p "There is no event recorded.")))
	 (defun html-event (event)
	   (who-ps-html (:p (chain -J-S-O-N (stringify event)))))
	 (defun html-filled-sink (events)
	   (who-ps-html
	    (:code
	     (loop :for event :in events
		   :collect (html-event event)))))
	 (defun html-sink (recorded-events)
	   (if (null recorded-events)
	       (html-empty-sink)
	       (html-filled-sink recorded-events)))
	 (defun update-sink (element)
	   (let ((event-sink
		   (find-dom-element "event-sink")))
	     (setf (inner-html event-sink)
		   (html-sink (@ *broker* :history)))))
	 (defun initialize-event-sink ()
	   (let ((event-sink
		   (find-dom-element "event-sink")))
	     (update-sink event-sink)))
	 (defun on-click-button-enable ()
	   (dispatch-webmachine (create :name :enable)))
	 (defun on-click-button-disable ()
	   (dispatch-webmachine (create :name :disable )))
	 (defun enable-dom-element (element)
	   (chain (find-dom-element element) (remove-attribute "disabled")))
	 (defun disable-dom-element (element)
	   (chain (find-dom-element element) (set-attribute "disabled" t)))
	 (initialize-event-sink)
	 (install-webmachine (make-initial-state) #'next-state)
	 (connect "toolbar-disable" :click #'on-click-button-disable)
	 (connect "toolbar-enable" :click #'on-click-button-enable)
	 (connect "close-all-sessions" :state update-close-all-sessions)

	 ))))))

(hunchentoot:define-easy-handler (broker :uri "/broker") ()
  (html-page (:title "Event Broker for Webmachine"
	      :navigation "About")
    (webmachine-javascript)
    (application-state)
    (html-page-title "Event Broker")
    (broker-toolbar)
    (broker-event-sink)))

#|
  (:script
     :type "text/javascript"
     (who:str
      (ps
	(defun find-dom-element (element)
	  (if (stringp element)
	      (chain document (get-element-by-id element))
	      element))
	(defun enable-dom-element (element)
	  (chain (find-dom-element element) (remove-attribute "disabled")))
	(defun disable-dom-element (element)
	  (chain (find-dom-element element) (set-attribute "disabled" t)))
	(defun (setf event-listener) (callback element event)
	  (chain (find-dom-element element)
		 (add-event-listener event callback)))
	;; Setup
	(defun on-click-send-data ()
	  (var xhr (new (-X-M-L-Http-Request)))
	  (var form-data (new (-Form-Data)))
	  (chain form-data (append "example1" "value1"))
	  (chain xhr (add-event-listener
		      "loadstart"
		      (lambda (event)
			(disable-dom-element "interaction")
			(disable-dom-element "send-data"))))
	  (chain xhr (add-event-listener
		      "loadend"
		      (lambda (event)
			(enable-dom-element "interaction")
			(enable-dom-element "send-data"))))
	  (chain xhr (add-event-listener
		      "load"
		      (lambda (event)
			(alert "XHR Load"))))
	  (chain xhr (add-event-listener "error" (lambda (event)
						  (alert "XHR Error"))))
	  (chain xhr (open "PUT" "http://localhost:8080/user/1"))
	  (chain xhr (send form-data)))
	(defun on-click-button-enable ()
	  (enable-dom-element "send-data"))
	(defun on-click-button-disable ()
	  (disable-dom-element "send-data"))
	(setf (event-listener "toolbar-disable" "click") #'on-click-button-disable)
	(setf (event-listener "toolbar-enable" "click") #'on-click-button-enable)
	(setf (event-listener "send-data" "click") #'on-click-send-data))))
|#





;;;; End of file `event-loop.lisp'
