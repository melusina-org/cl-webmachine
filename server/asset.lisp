;;;; asset.lisp — Asset for Webmachine

;;;; Webmachine (https://github.com/melusina-org/cl-webmachine)
;;;; This file is part of Webmachine.
;;;;
;;;; Copyright © 2018–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.webmachine/server)

(defparameter *assetdir*
  #.(flet ((make-asset-dir (pathname)
	     (when pathname
	       (make-pathname
		:directory
		(append
		 (butlast (pathname-directory pathname))
		 '("asset"))))))
      (or (uiop:getenv "ASSETDIR")
	  (make-asset-dir *compile-file-pathname*)
	  (make-asset-dir *load-pathname*)))
  "The pathname to the directory holding program assets.")

;;;;
;;;; Combine Dispatchers
;;;;

(defun combine-dispatchers (&rest dispatchers)
  "Combine DISPATCHERS in one dispatcher accepting the same requests.
Every member of DISPATCHERS should be a dispatcher function. If any
member of DISPATCHERS is a list, the COMBINE-DISPATCHERS
function is called rescursively on it."
  (let ((flat-dispatchers
	  (loop :for dispatcher :in dispatchers
		:collect (if (listp dispatcher)
			     (apply #'combine-dispatchers dispatcher)
			     dispatcher))))
    (lambda (request)
      (loop :for dispatcher :in flat-dispatchers
	    :for handler = (funcall dispatcher request)
	    :when handler
	    :return handler))))


;;;;
;;;; Bootstrap
;;;;

(defun create-bootstrap-dispatcher-and-handler ()
  "Create a Hunchentoot dispatcher for Bootstrap assets."
  (combine-dispatchers
   (hunchentoot:create-static-file-dispatcher-and-handler
    "/bootstrap/icons.svg"
    (merge-pathnames #p"bootstrap-icons.svg" *assetdir*))
   (hunchentoot:create-folder-dispatcher-and-handler
    "/bootstrap/"
    (merge-pathnames #p"bootstrap-5.3.0-alpha1-dist/" *assetdir*))))



;;;;
;;;; MathJax
;;;;

(defun create-mathjax-dispatcher-and-handler ()
  "Create a Hunchentoot dispatcher for MathJax assets."
  (hunchentoot:create-folder-dispatcher-and-handler
   "/mathjax/"
   (merge-pathnames #p"mathjax@3/es5/" *assetdir*)))

(defun write-mathjax-configuration (configuration)
  "Write MathJax configuration in HTML header.
Supported values for CONFIGURATION are: NIL, T
and :TEX-CHTML."
  (unless configuration
    (return-from write-mathjax-configuration))
  (who:with-html-output (*standard-output*)
    (:script
     :type "text/javascript"
     :id "MathJax-configuration"
     "
MathJax = {
  tex: {
    packages: {'[+]': ['configmacros']},
    macros: {
      R: \"\\\\mathbf{R}\"
    }
  }
}
")
    (case configuration
      ((t :tex-chtml)
       (who:htm
	(:script :type "text/javascript" :id "MathJax-script"
		 :src "/mathjax/tex-chtml.js"
		 :async t))))))


;;;;
;;;; ChartJS
;;;;

(defun create-chartjs-dispatcher-and-handler ()
  "Create a Hunchentoot dispatcher for ChartJS assets."
  (hunchentoot:create-static-file-dispatcher-and-handler
   "/chart.umd.min.js"
   (merge-pathnames #p"chart.js-4.2.0/chart.umd.min.js" *assetdir*)))

(defun write-chartjs-configuration (configuration)
  "Write ChartJS configuration in header.
The possible CONFIGURATION values are NIL, T, :CDN."
  (unless configuration
    (return-from write-chartjs-configuration))
  (case configuration
    (:cdn
     (who:with-html-output (*standard-output*)
       (:script
	:src #.(concatenate
		'string
		"https://cdnjs.cloudflare.com/ajax/libs"
		"/Chart.js/4.2.0/chart.umd.min.js")
	:integrity #.(concatenate
		      'string
		      "sha512-0gS26t/01v98xlf2QF4QS1k32/YHWfFs8HfBM/j7g"
		      "S97Tr8WxpJqoiDND8r1HgFwGGYRs0aRt33EY8xE91ZgJw==")
	:crossorigin "anonymous"
	:referrerpolicy "no-referrer")))
    (t
     (who:with-html-output (*standard-output*)
       (:script
	:src "/contrib/chart.umd.min.js")))))

;;;; End of file `asset.lisp'
