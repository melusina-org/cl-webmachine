;;;; features.lisp — Features View Webmachine Example

;;;; Webmachine (https://github.com/melusina-org/cl-webmachine)
;;;; This file is part of Webmachine.
;;;;
;;;; Copyright © 2018–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:webmachine/example)

(defun features-paragraph (&key label icon badges paragraph actions)
  "Write an features paragraph."
  (flet ((badges (badges)
	   (when badges
	     (who:with-html-output (*standard-output*)
	       (:div
		(loop :for badge :on badges
		      :for class = "badge text-bg-secondary me-1 my-1"
		      :then (if (null (cdr badge))
				"badge text-bg-secondary ms-1 my-1"
				"badge text-bg-secondary mx-1 my-1")
		      :do (who:htm
			   (:span :class class
				  (who:str (first badge)))))))))
	 (buttons (actions)
	   (when actions
	     (who:with-html-output (*standard-output*)
	       (:div
		(loop :for action :on actions
		      :for link = (cdr (first action))
		      :for label = (car (first action))
		      :for class = "btn btn-primary me-1"
		      :then (if (null (cdr action))
				"btn btn-secondary ms-1"
				"btn btn-secondary mx-1")
		      :do (who:htm
			   (:a :href link :class class
			       (who:str label)))))))))
    (who:with-html-output (*standard-output*)
      (:div :class "col d-flex align-items-start"
            (:div :class "icon-square bg-light text-dark flex-shrink-0 me-3"
		  (bootstrap-icon :class "bi" :width "1em" :height "1em" :icon icon))
	    (:div
	     (:h2 (who:str label))
	     (badges badges)
	     (:p (who:str paragraph))
	     (buttons actions))))))
   
(defmacro features-section ((&key id label) &body body-forms)
  `(who:with-html-output (*standard-output*)
     (:div :class "py-5" :id ,id
	   (:h2 :class "pb-2 border-bottom" ,label)
	   (:div :class "row g-4 py-5 row-cols-1 row-cols-lg-2"
		 ,@body-forms))))

(defun features-done ()
  (features-section (:id "features-done" :label "Implemented Features")
    (features-paragraph
     :label "Authentication"
     :icon "patch-check"
     :paragraph
     #.(join
	 "The application authentication feature allows"
	 "password based authentication. Users can Login and Logout."
	 "The Administrator can close all open sessions."))
    (features-paragraph
     :label "Health"
     :icon "bandaid"
     :paragraph
     #.(join
	 "A basic health information is displayed."))))

(defun features-todo ()
  (features-section (:id "features-todo" :label "Missing Features")
    (features-paragraph
     :label "Dynamic Web Application"
     :icon "list-task"
     :paragraph
     #.(join
	 "The application needs the ability to trigger actions using buttons,"
	 "without reloading the page, and monitor progress using a web socket."))))


;;;;
;;;; Features Resource
;;;;

(define-constant-resource (features :uri "/features" :content-type :text/html)
  (html-page (:title "Features Example for Webmachine"
	      :navigation "Features")
    (html-page-title "Application Features")
    (features-done)
    (features-todo)))

;;;; End of file `features.lisp'
