;;;; javascript.lisp — JavaScript for Web Apps à la Redux

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

(defun utilities-javascript ()
  (who:with-html-output (*standard-output*)
    (:script
     :id "utilities-javascript"
     :type "text/javascript"
     (who:str
      (ps
	(defparameter *debug* t
	  "Flag governing debugging application.")
	(defun error (details)
	  (chain console (log details)))
	(defun debug (details)
	  (chain console (debug details)))
	;; DOM Element
	(defun find-dom-element (element)
	  (if (stringp element)
	      (or
	       (chain document (get-element-by-id element))
	       (error (+ "No element with id " element)))
	      element))
	(defun enable-dom-element (element)
	  (chain (find-dom-element element) (remove-attribute "disabled")))
	(defun disable-dom-element (element)
	  (chain (find-dom-element element) (set-attribute "disabled" t)))
	(defun update-disabled-property (&key disabled busy element)
	  "Update the disabled attribute of a DOM element.
The attribute is updated according to the value of the DISABLED or BUSY
value."
	  (if (or disabled busy)
	      (disable-dom-element element)
	      (enable-dom-element element)))
	;; JSON
	(defun json-stringify (object)
	  (chain -j-s-o-n (stringify object)))
	(defun json-parse (text)
	  (chain -j-s-o-n (parse text)))
	(defun form-data-as-dto (element)
	  "Turn the form data from ELEMENT to a DTO.
A DTO is a plain JavaScript object that can be converted to a string with
`json-stringify'."
	  (flet ((form-data (element)
		   (new (-form-data (find-dom-element element))))
		 (as-dto (form-data)
		   (chain -object
			  (from-entries (new (-map (chain form-data (entries))))))))
	    (as-dto (form-data element))))
	;; Miscellaneous
	(defun shallow-clone (object)
	  "Create a shallow clone of OBJECT."
	  (chain -object (assign (create) object)))
	(defun delay-promise (duration)
	  "Delay promise for DURATION in milliseconds.

Example:
    (chain
      PROMISE
      (then (delay-promise 1000))
      (then CALLBACK))
"
	  (lambda (response)
	    (new (-promise (lambda (resolve)
			     (set-timeout
			      (lambda ()
				(funcall resolve response))
			      duration)))))))))))

(defun functional-state-javascript ()
  "Write functional state JavaScript to *STANDARD-OUTPUT*.
The functional state JavaScript library helps its user to write web applications
were the application state is immutable and whose transformations are computed
by pure functions.  This approach is inspired by React Redux."
  (who:with-html-output (*standard-output*)
    (:script
     :id "functional-state-javascript"
     :type "text/javascript"
     (who:str
      (ps
	(defvar *application* nil
	  "The current web application.")
	(defun make-application (initial-state transition)
	  (create 'event-bus (new -Event-Target)
		  'state initial-state
		  'transition transition))
	(defun dispatch-application-event (event-type detail)
	  (let ((event
		  (new (-custom-event event-type (create :detail detail)))))
	    (with-slots (event-bus) *application*
	      (chain event-bus (dispatch-event event)))))
	(defun install-application (initial-state transition)
	  (flet ((process-application-action (event)
		   (with-slots (event-bus state transition) *application*
		     (let ((last-state
			     state)
			   (next-state
			     (funcall transition
				      (getprop event 'detail)
				      (shallow-clone state))))
		       (setf state next-state)
		       (when *debug*
			 (debug (create 'last-state last-state
					'event-detail (getprop event 'detail)
					'next-state next-state)))
		       (dispatch-application-event :state next-state)))))
	    (unless *application*
	      (setf *application* (make-application initial-state transition))
	      (with-slots (event-bus) *application*
		(chain event-bus
		       (add-event-listener :action #'process-application-action))))))
	(defun dispatch-application-action (detail)
	  (if (stringp detail)
	      (dispatch-application-event :action (create 'name detail))
	      (dispatch-application-event :action detail)))
	(defun connect-application (element event-type callback)
	  (let ((element
		  (find-dom-element element)))
	    (labels ((object-to-plist (object)
		       "Transform a JavaScript object to a property list."
		       (chain -object (entries object) (flat)))
		     (state-event-listener (state)
		       (apply callback :element element
			      (object-to-plist (getprop state :detail)))))
	      (case event-type
		(:state
		 (with-slots (event-bus) *application*
		   (chain event-bus
			  (add-event-listener :state #'state-event-listener))))
		(t
		 (chain element
			(add-event-listener event-type callback))))))))))))

;;;; End of file `javascript.lisp'
