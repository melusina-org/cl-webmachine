;;;; about.lisp — About View Webmachine Example

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

(defun about-paragraph (&key label icon badges paragraph actions)
  "Write an about paragraph."
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

(defun about-software (&key label icon paragraph license version
			    (actions '(("Homepage" . "#"))))
  "Write an about software paragraph."
  (about-paragraph
   :label label
   :icon icon
   :paragraph paragraph
   :badges (list version license)
   :actions actions))
   
(defmacro about-section ((&key id label) &body body-forms)
  `(who:with-html-output (*standard-output*)
     (:div :class "py-5" :id ,id
	   (:h2 :class "pb-2 border-bottom" ,label)
	   (:div :class "row g-4 py-5 row-cols-1 row-cols-lg-2"
		 ,@body-forms))))


;;;;
;;;; Project
;;;;

(defun about-project ()
  (html-page-section (:id "project" :title "Webmachine Project")
    (about-paragraph
     :label "GitHub"
     :icon "github"
     :paragraph
     #.(concatenate 'string
	 "The GitHub repository where the source of"
	 "the Webmachine Project is stored.")
     :actions
     '(("Source" .
	"https://github.com/melusina-org/cl-webmachine")
       ("Actions" .
	"https://github.com/melusina-org/cl-webmachine/actions")
       ("Issues" .
	"https://github.com/melusina-org/cl-webmachine/issues")
       ("Stargazers" .
	"https://github.com/melusina-org/cl-webmachine/stargazers")))))


;;;;
;;;; External Contributions
;;;;

(defun about-external-contributions ()
  (html-page-section (:id "contrib" :title "External Contributions"
		 :columns "row-cols-lg-2")
    (about-software
     :label "Bootstrap"
     :icon "bootstrap"
     :paragraph
     #.(concatenate 'string
	 "Powerful, extensible, and feature-packed frontend toolkit."
	 "Build and customize with Sass, utilize prebuilt grid system"
	 "and components, and bring projects to life with powerful"
	 "JavaScript plugins.")
     :version "5.3.0-alpha3"
     :license "MIT"
     :actions
     '(("Homepage" . "https://getbootstrap.com")
       ("Documentation" .
	"https://getbootstrap.com/docs/5.3/getting-started/introduction/")
       ("Icons" . "https://icons.getbootstrap.com")))
    (about-software
     :label "MathJax"
     :icon "calculator"
     :version "3.2.2"
     :license "Apache 2.0"
     :paragraph
     #.(concatenate 'string
	 "Beautiful and accessible math in all browsers."
	 "A JavaScript display engine for mathematics"
	 "that works in all browsers. No more setup for readers."
	 "It just works.")
     :actions
     '(("Homepage" .
	"https://www.mathjax.org")
       ("Repository" .
	"https://github.com/mathjax/MathJax-src")
       ("Release" .
	"https://github.com/mathjax/MathJax-src/releases/tag/3.2.2")))
    (about-software
     :label "ChartJS"
     :icon "clipboard2-data"
     :version "4.2.0"
     :license "MIT"
     :paragraph
     #.(concatenate 'string
	 "Chart.js provides a set of frequently used chart types, plugins,"
	 "and customization options. In addition to a reasonable set of"
	 "built-in chart types, you can use additional community-maintained"
	 "chart types. On top of that, it’s possible to combine several"
	 "chart types into a mixed chart (essentially, blending multiple"
	 "chart types into one on the same canvas).")
     :actions
     '(("Homepage" .
	"https://www.chartjs.org/docs/latest/")
       ("Samples" .
	"https://www.chartjs.org/docs/latest/samples/information.html")
       ("Release" .
	"https://github.com/chartjs/Chart.js/releases/tag/v4.2.0")))
    (about-software
     :label "Hunchentoot"
     :icon "box-seam"
     :version hunchentoot:*hunchentoot-version*
     :license "BSD-2"
     :paragraph
     #.(concatenate 'string
	 "Hunchentoot is a web server written in Common Lisp and"
	 "at the same time a toolkit for building dynamic websites.")
     :actions
     '(("Homepage" .
	"http://edicl.github.io/hunchentoot/")
       ("Repository" .
	"https://github.com/edicl/hunchentoot")
       ("Release" .
	"https://github.com/edicl/hunchentoot/releases/tag/v1.3.0")))
    (about-software
     :label "Parenscript"
     :icon "box-seam"
     :version parenscript:*version*
     :license "BSD-3"
     :paragraph
     #.(concatenate
	'string
	"Parenscript is a translator from an extended subset of Common Lisp to JavaScript. Parenscript code "
	"can run almost identically on both the browser (as JavaScript) and server (as Common Lisp).")
     :actions
     '(("Homepage" .
	"https://parenscript.common-lisp.dev")
       ("Repository" .
	"https://gitlab.common-lisp.net/parenscript/parenscript.git")
       ("Documentation" .
	"https://parenscript.common-lisp.dev/reference.html")))
    ))


;;;;
;;;; 
;;;;

(defun about-environment ()
  (html-page-section (:id "contrib" :title "Environment"
		 :columns "row-cols-lg-2")
    (about-paragraph
     :label "Machine"
     :icon "cpu-fill"
     :paragraph
     #.(concatenate 'string
	 "Characteristics of the current machine as reflected by"
	 "Common Lisp functions <em>machine-type,</em>"
	 "<em>machine-version</em> and <em>machine-instance.</em>")
     :badges (list (machine-type) (machine-version) (machine-instance)))
    (about-paragraph
     :label "Lisp Implementation"
     :icon "arrow-repeat"
     :paragraph
     #.(concatenate 'string
	 "Characteristics of the current Lisp implementation as reflected by"
	 "Common Lisp functions <em>lisp-implementation-type,</em>"
	 "and <em>lisp-implementation-version.</em>")
     :badges (list (lisp-implementation-type) (lisp-implementation-version)))
    #+quicklisp
    (about-paragraph
     :label "Quicklisp"
     :icon "quora"
     :badges (list (ql:dist-version "quicklisp"))
     :paragraph
     #.(concatenate 'string
	 "Quicklisp is a library manager for Common Lisp. It works with your"
	 "existing Common Lisp implementation to download, install, and load"
	 "any of over 1,500 libraries with a few simple commands.")
    :actions
    '(("Homepage" .
       "https://www.quicklisp.org/beta/")))))


;;;;
;;;; About Resource
;;;;

(define-constant-resource (about :uri "/about" :content-type :text/html)
  (html-page (:title "About Example for Webmachine"
	      :navigation "About")
    (html-page-title "About")
    (about-project)
    (about-external-contributions)
    (about-environment)))

;;;; End of file `about.lisp'
