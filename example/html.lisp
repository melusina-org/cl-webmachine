;;;; html.lisp — Entrypoint for Webmachine

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf
   (who:html-mode) :html5
   who:*attribute-quote-char* #\"))

(defun bootstrap-icon (&key (class "bi") width height label (icon "bootstrap"))
  (who:with-html-output (*standard-output*)
    (:svg 
     :class class :width width :height height :role "img"
     :aria-label label
     (format *standard-output*
	     "<use xlink:href=\"/bootstrap/icons.svg#~A\"></use>" icon))))

(defun bootstrap-margin (classes margin)
  "Ass MARGIN to CLASSES."
  (let ((margin-class
	  (when margin
	    (ecase margin
	      (:start
	       "ms-1")
	      (:end
	       "me-1")
	      ((:x :both)
	       "mx-1")))))
    (if margin-class
	(concatenate 'string classes " " margin-class)
	classes)))

(defun html-classes (&rest classes)
  "Concatenate classes."
  (format nil "~{~A~^ ~}" classes))

(defun html-login-sign-up-combo ()
  (who:with-html-output (*standard-output*)
    (:div :class "col-md-3 text-end"
	  (:a :role "button"
	      :href "/login"
	      :class "btn btn-outline-primary me-2"
	      "Login")
	  (:a :role "button"
	      :href "/sign-up"
	      :class "btn btn-primary me-2"
	      "Sign-up"))))

(defun html-logout ()
  (who:with-html-output (*standard-output*)
    (:div :class "col-md-3 text-end"
	  (:a :role "button"
	      :href "/logout"
	      :class "btn btn-outline-primary me-2"
	      "Logout"))))

(defun html-login-or-logout ()
  (if hunchentoot:*session*
      (html-logout)
      (html-login-sign-up-combo)))

(defun html-navigation-bar (&optional current)
  "Write the navigation bar of the web application."
  (let ((icon-href "/")
	(icon-name "bootstrap")
	(icon-label "Bootstrap")
	(navigation-list '(("Home" . "/home")
			   ("Features" . "/features")
			   ("Users" . "/user")
			   ("Health" . "/health")
			   ("About" . "/about"))))
    (who:with-html-output (*standard-output*)
      (:div :class "container" :id "navigation-bar"
	    (:header :class #.(join
				"d-flex flex-wrap align-items-center"
				"justify-content-center justify-content-md-between"
				"py-3 mb-4 border-bottom")
		     (:a :class #.(join
				    "d-flex align-items-center"
				    "col-md-3 mb-2 mb-md-0"
				    "text-dark text-decoration-none")
		      :href icon-href
		      (bootstrap-icon :class "bi me-2" :width "40" :height "32"
					 :label icon-label :icon icon-name))
		     (:ul
		      :class "nav col-12 col-md-auto mb-2 justify-content-center mb-md-0"
		      (flet ((class (label)
			       (let ((active
				       "nav-link px-2 link-dark b")
				     (inactive
				       "nav-link px-2 link-secondary"))
				 (cond
				   ((not current)
				    inactive)
				   ((string-equal current label)
				    active)
				   (t
				    inactive)))))
			(loop :for (label . href) :in navigation-list
			      :do (who:htm
				   (:li (:a :href href
					    :class (class label)
					    (who:str label)))))))
		     (html-login-or-logout))))))

(defun html-page-title (title &rest format-arguments)
  (who:with-html-output (*standard-output*)
    (:h1 :class "display-1 my-5 text-center"
	 (apply #'format *standard-output* title format-arguments))))

(defmacro html-page-section ((&key id title (columns "row-cols-lg-1")) &body body-forms)
  `(who:with-html-output (*standard-output*)
     (:div :class "py-5" :id ,id
	   (:h2 :class "pb-2 border-bottom" ,title)
	   (:div :class (concatenate 'string "row g-4 py-5 " ,columns)
		 ,@body-forms))))

(defun html-modal-title (title)
  (who:with-html-output (*standard-output*)
    (:h1 :class "my-3 text-center" (who:str title))))


;;;;
;;;; HTML Page
;;;;

(hunchentoot:define-easy-handler (example.css :uri "/example.css") ()
  (setf (hunchentoot:content-type*) "text/css")
  (css:css
   '((".icon-square"
      :display "inline-flex"
      :align-items "center"
      :justify-content "center"
      :width "3rem"
      :height "3rem"
      :font-size "1.5rem"
      :border-radius ".75rem"))))
   

(defmacro html-page ((&key title (lang "en") (mathjax nil) (chartjs nil) navigation)
		     &body body)
  `(who:with-html-output-to-string (*standard-output* nil :prologue t :indent nil)
     (:html
      :lang ,lang
      (:head
       (:meta :charset "utf-8")
       (:title ,title)
       (:link
	:href "/bootstrap/css/bootstrap.min.css"
	:rel "stylesheet")
       (:link
	:href "/example.css"
	:rel "stylesheet")
       (server::write-mathjax-configuration ,mathjax)
       (server::write-chartjs-configuration ,chartjs))
      (:body
       (html-navigation-bar ,navigation)
       (:div :class "container"
	     ,@body)
       (:script
	:src "/bootstrap/js/bootstrap.bundle.min.js"
	:type "text/javascript")))))


;;;;
;;;; Forbidden Page
;;;;

(defun forbidden-page ()
  (html-page (:title "Forbidden Access to Webmachine Example")
    (html-page-title "Forbidden")
    (who:with-html-output (*standard-output*)
      (:div :class "container"
	    (:div :class "p-5 mb-4 bg-light rounded-3"
		  (:div :class "container-fluid py-5"
			(:h1 :class "display-5 fw-bold"
			     "Regulated access to a ressource")
			(:p :class "fs-4"
			    #.(join
				"The access to the requested ressource is"
				"regulated and is only accessible to"
				"authorised users."
				"Use the <em>Login</em> button to start"
				"a session."))
			(:a :href "/login"
			    :class "btn btn-primary btn-lg"
			    :type="button"
			    "Login")))))))

(defun reply-forbidden ()
  (progn
    (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)
    (forbidden-page)))

(defmacro with-valid-session (&body body-forms)
  `(if hunchentoot:*session*
       (progn ,@body-forms)
       (reply-forbidden)))

;;;; End of file `html.lisp'
