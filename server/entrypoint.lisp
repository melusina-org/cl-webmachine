;;;; entrypoint.lisp — Entrypoint for Webmachine

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

(defparameter *swank-port* 4005
  "The port of the SWANK listener.")

(defparameter *service-port* 8080
  "The port of the service listener.")

(defparameter *server-address* "127.0.0.1")


;;;;
;;;; Acceptor
;;;;

(defclass acceptor (hunchentoot:easy-acceptor webmachine:acceptor)
  nil
  (:documentation "Webmachine example acceptor.
We use an acceptor deriving from Hunchentoot to easily serve static content."))

(defun update-service-dispatch-table ()
  "Make a dispatch table of the service."
  (setf hunchentoot:*dispatch-table*
	(list #'hunchentoot:dispatch-easy-handlers
	      (create-mathjax-dispatcher-and-handler)
	      (create-bootstrap-dispatcher-and-handler)
	      (create-chartjs-dispatcher-and-handler))))



;;;;
;;;; Start and Stop the Server
;;;;

(defvar *swank-acceptor* nil
  "The instance of the swank server running.")

(defvar *service-acceptor* nil
  "The instance of the service running.")

(defun start-swank ()
  "Start the SWANK server."
  (unless *swank-acceptor*
    (setf swank::*loopback-interface* *server-address*)
    (swank-loader:init)
    (swank:create-server
     :port *swank-port*
     :dont-close t
     :style swank:*communication-style*)
    (setf *swank-acceptor* t)))

(defun stop-swank ()
  "Stop the SWANK server."
  (when *swank-acceptor*
    (swank:stop-server *swank-port*)
    (setf *swank-acceptor* nil)))

(defun make-service-acceptor (resources)
  "Make a service Hunchentoot acceptor."
  (make-instance 'acceptor
		 :resources resources
                 :port *service-port*
                 :address *server-address*))

(defun start-service (resources)
  "Make and start a service Hunchentoot acceptor."
  (unless *service-acceptor*
    (setf *service-acceptor*
          (hunchentoot:start (make-service-acceptor resources)))))

(defun stop-service ()
  "Stop the service Hunchentoot acceptor."
  (when *service-acceptor*
    (hunchentoot:stop *service-acceptor*)
    (setf *service-acceptor* nil)))

(defun start (resources &key (service t) (swank nil))
  "Start the server."
  (when service
    (update-service-dispatch-table)
    (start-service resources))
  (when swank
    (start-swank)))

(defun stop (&key (service t) (swank t))
  "Stop the server."
  (when service
    (stop-service))
  (when swank
    (stop-swank)))


;;;;
;;;; Resources
;;;;

(defun resources ()
  "The list of resources from the running server."
  (unless *service-acceptor*
    (error "There is no service acceptor whose resources could be accessed."))
  (slot-value 'resources *service-acceptor*))

(defun (setf resources) (resources)
  (unless *service-acceptor*
    (error "There is no service acceptor whose resources could be accessed."))
  (setf (slot-value 'resources *service-acceptor*)  resources))

;;;; End of file `entrypoint.lisp'
