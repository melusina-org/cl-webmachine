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

(defpackage #:org.melusina.webmachine/asset
  (:use #:cl)
  (:local-nicknames
   (#:webmachine #:org.melusina.webmachine))
  (:export
   #:make-bootstrap-resource
   #:make-bootstrap-icons-resource
   #:make-mathjax-resource
   #:make-chartjs-resource))

(in-package #:org.melusina.webmachine/asset)

(defparameter *assetdir*
  #.(merge-pathnames
     #p"asset/"
     (asdf:system-source-directory (string-downcase (package-name *package*))))
  "The pathname to the directory holding program assets.")

(defun asset-pathname (pathname)
  "Make a PATHNAME relative to asset directory."
  (merge-pathnames pathname *assetdir*))


;;;;
;;;; Bootstrap
;;;;

(defun make-bootstrap-resource (&key (name 'bootstrap) (path "/bootstrap/"))
  "Make a resource serving Bootstrap 5.3.0-alpha1 assets."
  (webmachine:make-directory-resource
   :name name
   :path path
   :alias (asset-pathname #p"bootstrap-5.3.0-alpha1-dist/")))

(defun make-bootstrap-icons-resource (&key (name 'bootstrap-icons) (path "/bootstrap/icons.svg"))
  "Make a resource serving Bootstrap icons."
  (webmachine:make-file-resource
   :name name
   :path path
   :alias (asset-pathname #p"bootstrap-icons.svg")))


;;;;
;;;; MathJax
;;;;

(defun make-mathjax-resource (&key (name 'mathjax) (path "/mathjax/"))
  "Make a resource serving MathJax 3.2.2 assets."
  (webmachine:make-directory-resource
   :name name
   :path path
   :alias (asset-pathname #p"mathjax@3/es5/")))


;;;;
;;;; ChartJS
;;;;

(defun make-chartjs-resource (&key (name 'chartjs) (path "/chart.umd.min.js"))
  "Make a resource serving ChartJS 4.2.0 asset."
  (webmachine:make-file-resource
   :name name
   :path path
   :alias (asset-pathname #p"chart.js-4.2.0/chart.umd.min.js")))

;;;; End of file `asset.lisp'
