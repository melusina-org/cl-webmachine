;;;; resource.lisp — Web Resource for Webmachine

;;;; Webmachine (https://github.com/melusina-org/cl-webmachine)
;;;; This file is part of Webmachine.
;;;;
;;;; Copyright © 2018–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.webmachine/testsuite)

(defclass test-resource-hello (webmachine:resource) nil)

(defmethod webmachine:resource-to-text/html ((resource test-resource-hello) request destination)
  (format destination "<html>Hello, world!</html>"))

;(defmethod webmachine:resource-to-text/plain ((resource test-resource-hello) request destination)
;  (format destination "Hello!"))

(defclass constant-resource (webmachine:resource)
  ((available-p
    :initarg :available-p
    :initform t
    :documentation
    "The return value of the `WEBMACHINE:RESOURCE-AVAILABLE-P' generic function.")
   (known-methods
    :initarg :known-methods
    :initform nil
    :documentation
    "The return value of the `WEBMACHINE:RESOURCE-KNOWN-METHODS' generic function.")
   (uri-too-long-p
    :initarg :uri-too-long-p
    :initform nil
    :documentation
    "The return value of the `WEBMACHINE:RESOURCE-URI-TOO-LONG-P' generic function.")
   (payload-too-large-p
    :initarg :payload-too-large-p
    :initform nil
    :documentation
    "The return value of the `WEBMACHINE:RESOURCE-PAYLOAD-TOO-LARGE-P' generic function.")
   (allowed-methods
    :initarg :allowed-methods
    :initform '(:get :head)
    :documentation
    "The return value of the `WEBMACHINE:RESOURCE-ALLOWED-METHODS' generic function.")
   (valid-request-p
    :initarg :valid-request-p
    :initform t
    :documentation
    "The return value of the `WEBMACHINE:RESOURCE-VALID-REQUEST-P' generic function.")
   (authorized-p
    :initarg :authorized-p
    :initform t
    :documentation
    "The return value of the `WEBMACHINE:RESOURCE-AUTHORIZED-P' generic function.")
   (forbidden-p
    :initarg :forbidden-p
    :initform nil
    :documentation
    "The return value of the `WEBMACHINE:RESOURCE-FORBIDDEN-P' generic function.")
   (valid-content-headers-p
    :initarg :valid-content-headers-p
    :initform t
    :documentation
    "The return value of the `WEBMACHINE:RESOURCE-VALID-CONTENT-HEADERS-P' generic function.")
   (valid-content-type-p
    :initarg :valid-content-type-p
    :initform t
    :documentation
    "The return value of the `WEBMACHINE:RESOURCE-VALID-CONTENT-TYPE-P' generic function.")
   (options
    :initarg :options
    :initform t
    :documentation
    "The return value of the `WEBMACHINE:RESOURCE-OPTIONS' generic function.")
   (content-types-provided
    :initarg :content-types-provided
    :initform '((:text/html . webmachine:resource-to-text/html))
    :documentation
    "The return value of the `WEBMACHINE:RESOURCE-CONTENT-TYPES-PROVIDED' generic function.")
   (languages-provided
    :initarg :languages-provided
    :initform nil
    :documentation
    "The return value of the `WEBMACHINE:RESOURCE-LANGUAGES-PROVIDED' generic function.")
   (charsets-provided
    :initarg :charsets-provided
    :initform '(:utf-8)
    :documentation
    "The return value of the `WEBMACHINE:RESOURCE-CHARSETS-PROVIDED' generic function.")
   (encodings-provided
    :initarg :encodings-provided
    :initform '(:identity)
    :documentation
    "The return value of the `WEBMACHINE:RESOURCE-ENCODINGS-PROVIDED' generic function."))
  (:documentation
   "A resource on which generic functions return a fixed value, corresponding
to initialisation parameters of the instance. Such a resource is useful
for testing."))


(defmethod webmachine:resource-to-text/html ((resource constant-resource) request destination)
  (format destination "<html>A constant resource!</html>"))

(defmethod webmachine:resource-available-p and ((resource constant-resource))
  (slot-value resource 'available-p))

(defmethod webmachine:resource-known-methods append ((resource constant-resource))
  (slot-value resource 'known-methods))

(defmethod webmachine:resource-uri-too-long-p ((resource constant-resource) request)
  (slot-value resource 'uri-too-long-p))

(defmethod webmachine:resource-payload-too-large-p ((resource constant-resource) request)
  (slot-value resource 'payload-too-large-p))

(defmethod webmachine:resource-allowed-methods ((resource constant-resource))
  (slot-value resource 'allowed-methods))

(defmethod webmachine:resource-valid-request-p and ((resource constant-resource) request)
  (slot-value resource 'valid-request-p))

(defmethod webmachine:resource-authorized-p ((resource constant-resource) request)
  (if (slot-value resource 'authorized-p)
      (values t)
      (values nil "None")))

(defmethod webmachine:resource-forbidden-p or ((resource constant-resource) request)
  (slot-value resource 'forbidden-p))

(defmethod webmachine:resource-valid-content-headers-p and ((resource constant-resource) request)
  (slot-value resource 'valid-content-headers-p))

(defmethod webmachine:resource-valid-content-type-p and ((resource constant-resource) request)
  (slot-value resource 'valid-content-type-p))

(defmethod webmachine:resource-options append ((resource constant-resource))
  (slot-value resource 'options))

(defmethod webmachine:resource-content-types-provided ((resource constant-resource))
  (slot-value resource 'content-types-provided))

(defmethod webmachine:resource-languages-provided ((resource constant-resource))
  (slot-value resource 'languages-provided))

(defmethod webmachine:resource-charsets-provided ((resource constant-resource))
  (slot-value resource 'charsets-provided))

(defmethod webmachine:resource-encodings-provided ((resource constant-resource))
  (slot-value resource 'encodings-provided))

(define-testcase test-v3b13 ()
  (with-testsuite-acceptor
      ((make-instance 'constant-resource
		      :path "/test/v3b13"
		      :name 'test/v3b13
		      :available-p nil))
    (with-http-reply ("/test/v3b13" :content-type :text/plain)
      (assert-http-status 503))))

(define-testcase test-v3b12 ()
  "The extension of allowed method list is not supported by Hunchentoot."
  (with-testsuite-acceptor
      ((make-instance 'constant-resource
                      :path "/test/v3b12-report"
                      :name 'test/v3b12-report
                      :known-methods '(:report)
                      :allowed-methods '(:get :head :report))
       (make-instance 'constant-resource
                      :path "/test/v3b12-standard"
                      :name 'test/v3b12-standard))
    (with-http-reply ("/test/v3b12-report" :method :report)
      (assert-http-status 200))
    (with-http-reply ("/test/v3b12-standard" :method :report)
      (assert-http-status 501))))

(define-testcase test-v3b11 ()
  (with-testsuite-acceptor
      ((make-instance 'constant-resource
                      :path "/test/v3b11-uri-too-long"
                      :name 'test/v3b11-uri-too-long
                      :uri-too-long-p t)
       (make-instance 'constant-resource
                      :path "/test/v3b11-uri-acceptably-long"
                      :name 'test/v3b11-uri-acceptably-long
                      :uri-too-long-p nil))
    (with-http-reply ("/test/v3b11-uri-acceptably-long")
      (assert-http-status 200))
    (with-http-reply ("/test/v3b11-uri-too-long")
      (assert-http-status 414))))

(define-testcase test-v3b10 ()
  (with-testsuite-acceptor
      ((make-instance 'constant-resource
                      :path "/test/v3b10"
                      :name 'v3b10
                      :allowed-methods '(:get)))
    (with-http-reply ("/test/v3b10")
      (assert-http-status 200)
      (assert-http-header-match :allow "GET"))
    (with-http-reply ("/test/v3b10" :method :put)
      (assert-http-status 405))))

(define-testcase test-v3b9 ()
  (with-testsuite-acceptor
      ((make-instance 'constant-resource
                      :path "/test/v3b9-malformed"
                      :name 'v3b9-malformed
                      :valid-request-p nil)
       (make-instance 'constant-resource
                      :path "/test/v3b9-wellformed"
                      :name 'v3b9-wellformed
                      :valid-request-p t))
    (with-http-reply ("/test/v3b9-wellformed")
      (assert-http-status 200))
    (with-http-reply ("/test/v3b9-malformed")
      (assert-http-status 400))))

(define-testcase test-v3b8 ()
  (with-testsuite-acceptor
      ((make-instance 'constant-resource
                      :path "/test/v3b8-forbidden"
                      :name 'v3b8-forbidden
                      :authorized-p nil)
       (make-instance 'constant-resource
                      :path "/test/v3b8-authorized"
                      :name 'v3b8-authorized
                      :authorized-p t))
    (with-http-reply ("/test/v3b8-authorized")
      (assert-http-status 200))
    (with-http-reply ("/test/v3b8-forbidden")
      (assert-http-header-match :www-authenticate "None")
      (assert-http-status 401))))

(define-testcase test-v3b7 ()
  (with-testsuite-acceptor
      ((make-instance 'constant-resource
                      :path "/test/v3b7-forbidden"
                      :name 'v3b7-forbidden
                      :forbidden-p t)
       (make-instance 'constant-resource
                      :path "/test/v3b7-authorized"
                      :name 'v3b7-authorized
                      :forbidden-p nil))
    (with-http-reply ("/test/v3b7-authorized")
      (assert-http-status 200))
    (with-http-reply ("/test/v3b7-forbidden")
      (assert-http-status 403))))

(define-testcase test-v3b6 ()
  (with-testsuite-acceptor
      ((make-instance 'constant-resource
                      :path "/test/v3b6-invalid"
                      :name 'v3b6-invalid
                      :valid-content-headers-p nil)
       (make-instance 'constant-resource
                      :path "/test/v3b6-valid"
                      :name 'v3b6-valid
                      :valid-content-headers-p t))
    (with-http-reply ("/test/v3b6-valid")
      (assert-http-status 200))
    (with-http-reply ("/test/v3b6-invalid")
      (assert-http-status 501))))

(define-testcase test-v3b5 ()
  (with-testsuite-acceptor
      ((make-instance 'constant-resource
                      :path "/test/v3b5-invalid"
                      :name 'v3b5-invalid
                      :valid-content-type-p nil)
       (make-instance 'constant-resource
                      :path "/test/v3b5-valid"
                      :name 'v3b5-valid
                      :valid-content-type-p t))
    (with-http-reply ("/test/v3b5-valid")
      (assert-http-status 200))
    (with-http-reply ("/test/v3b5-invalid")
      (assert-http-status 415))))

(define-testcase test-v3b4 ()
  (with-testsuite-acceptor
      ((make-instance 'constant-resource
                      :path "/test/v3b4-payload-too-large"
                      :name 'test/v3b4-payload-too-large
                      :payload-too-large-p t)
       (make-instance 'constant-resource
                      :path "/test/v3b4-payload-acceptably-large"
                      :name 'test/v3b4-payload-acceptably-large
                      :payload-too-large-p nil))
    (with-http-reply ("/test/v3b4-payload-acceptably-large")
      (assert-http-status 200))
    (with-http-reply ("/test/v3b4-payload-too-large")
      (assert-http-status 413))))

(define-testcase test-v3b3 ()
  (with-testsuite-acceptor
      ((make-instance 'constant-resource
                      :path "/test/v3b3-options"
                      :name 'test/v3b3-options
                      :allowed-methods '(:options :get :head)
                      :options '((:x-webmachine-test . "Hello!"))))
    (with-http-reply ("/test/v3b3-options" :method :options)
      (assert-http-status 204)
      (assert-http-header-match :x-webmachine-test "Hello!"))
    (with-http-reply ("/test/v3b3-options")
      (assert-http-status 200)
      (assert-http-header-undefined :x-webmachine-test))))

(define-testcase test-v3c3 ()
  (with-testsuite-acceptor
      ((make-instance 'constant-resource
                      :path "/test/v3c3"
                      :name 'test/v3c3
                      :content-types-provided
                      '((:text/plain . "Hello, world!")
                        (:text/html . "<html>Hello, world!</html>")
                        (:application/json . "\"Hello, world!\""))))
    (with-http-reply ("/test/v3c3" :accept "text/*;q=0.6")
      (assert-http-status 200)
      (assert-http-header-match :content-type "text/plain")
      (assert-http-body "Hello, world!"))
    (with-http-reply ("/test/v3c3" :accept "text/html;q=1, text/*;q=0.6, application/json;q=0.8")
      (assert-http-status 200)
      (assert-http-header-match :content-type "text/html.*")
      (assert-http-body "<html>Hello, world!</html>"))))

(define-testcase test-v3d5 ()
  (with-testsuite-acceptor
      ((make-instance 'constant-resource
                      :path "/test/v3d5"
                      :name 'test/v3d5
                      :languages-provided '("en")))
    (with-http-reply ("/test/v3d5" :additional-headers '(("accept-language" . "fr_FR;q=1, en;q=0.6")))
      (assert-http-status 200)
      (assert-http-header-match :content-type "text/html.*")
      (assert-http-body "<html>A constant resource!</html>"))))

(define-testcase test-v3e6 ()
  (with-testsuite-acceptor
      ((make-instance 'constant-resource
                      :path "/test/v3e6"
                      :name 'test/v3e6
                      :charsets-provided
                      '(:utf-8 :iso-8859-1)
                      :content-types-provided
                      '((:text/plain . "“Hello, world!”")
                        (:text/html . "<html>“Hello, world!”</html>")
                        (:application/json . "\"“Hello, world!”\""))))
    (with-http-reply ("/test/v3e6" :accept "text/html"
				   :additional-headers '(("accept-charset" . "utf-8")))
      (assert-http-status 200)
      (assert-http-header-charset :utf-8)
      (assert-http-body "<html>“Hello, world!”</html>"))
    (with-http-reply ("/test/v3e6" :accept "text/plain"
				   :additional-headers '(("accept-charset" . "utf-8")))
      (assert-http-status 200)
      (assert-http-header-charset :utf-8)
      (assert-http-body "“Hello, world!”"))))

(define-testcase test-v3f7 ()
  (with-testsuite-acceptor
      ((make-instance 'constant-resource
                      :path "/test/v3f7"
                      :name 'test/v3f7
                      :encodings-provided
                      '(:identity)))
    (with-http-reply ("/test/v3f7" :accept "text/html"
				   :additional-headers
				   '(("accept-encoding" . "invalid-encoding")))
      (assert-http-status 406))
    (with-http-reply ("/test/v3f7" :accept "text/html"
				   :additional-headers
				   '(("accept-encoding" .
				      "invalid-encoding;q=1, identity;q=0.1")))
      (assert-http-status 200)
      (assert-http-header-match :encoding "identity")
      (assert-http-body "<html>A constant resource!</html>"))))

(define-testcase test-resource-hello ()
  (with-testsuite-acceptor
      ((make-instance 'test-resource-hello
                      :name 'hello
                      :path "/hello/:who"))
    (with-http-reply ("/" :accept "text/plain")
      (assert-http-status 404)
      (assert-http-header-charset :utf-8)
      (assert-http-header-match :server "Webmachine"))
    (with-http-reply ("/hello/world" :accept "text/html")
      (assert-http-status 200)
      (assert-http-header-charset :utf-8)
      (assert-http-header-match :server "Webmachine")
      (assert-http-body "<html>Hello, world!</html>"))))

(define-testcase testsuite-resource ()
  (test-resource-hello)
  (test-v3b13)
  (unless "The extension of allowed method list is not supported by Hunchentoot."
    (test-v3b12))
  (test-v3b11)
  (test-v3b10)
  (test-v3b9)
  (test-v3b8)
  (test-v3b7)
  (test-v3b6)
  (test-v3b5)
  (test-v3b4)
  (test-v3b3)
  (test-v3c3)
  (test-v3d5)
  (test-v3e6)
  (test-v3f7))

;;;; End of file `resource.lisp'
