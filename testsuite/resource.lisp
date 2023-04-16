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


;;;;
;;;; Header Analysis
;;;;

(define-testcase testsuite-parse-header-accept-* ()
  "Validate the parse `PARSE-HEADER-ACEEPT-*' function."
  (assert-equal '(("text/*" . 0.6))
		(webmachine::parse-header-accept-* :media-type "text/*;q=0.6"))
  (assert-equal '(("text/plain" . 0))
		(webmachine::parse-header-accept-* :media-type "text/plain"))
  (assert-equal '(("text/html" . 1) ("application/json" . 0.8) ("text/*" . 0.6))
		(webmachine::parse-header-accept-*
		 :media-type
		 "text/html;q=1, text/*;q=0.6, application/json;q=0.8"))
  (assert-equal '(("text/html" . 0.9)
		  ("application/xhtml+xml" . 0.9)
		  ("application/xml" . 0.9)
		  ("*/*" . 0.8))
		(webmachine::parse-header-accept-*
		 :media-type "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"))
  (assert-equal '(("application/signed-exchange" . 0.9)
		  ("text/html" . 0.9)
		  ("application/xhtml+xml" . 0.9)
		  ("application/xml" . 0.9)
		  ("image/avif" . 0.8)
		  ("image/webp" . 0.8)
		  ("image/apng" . 0.8)
		  ("*/*" . 0.8))
		(webmachine::parse-header-accept-*
		 :media-type
		 #.(concatenate
		    'string
		    "text/html,application/xhtml+xml,application/xml;q=0.9,"
		    "image/avif,image/webp,image/apng,*/*;q=0.8,"
		    "application/signed-exchange;v=b3;q=0.9")))
  (assert-equal '(("fr-FR" . 1) ("fr" . 1) ("en" . 0.6))
		(webmachine::parse-header-accept-*
		 :language
		 "fr-FR,fr;q=1, en;q=0.6"))
  (assert-equal '(("en-GB" . 0.9) ("en" . 0.9))
		(webmachine::parse-header-accept-*
		 :language
		 "en-GB,en;q=0.9"))
  (assert-equal '(("utf-8" . 0))
		(webmachine::parse-header-accept-*
		 :charset
		 "utf-8"))
  (assert-equal '(("deflate" . 1) ("identity" . 0.1))
		(webmachine::parse-header-accept-*
		 :encoding
		 "deflate;q=1, identity;q=0.1"))
  (assert-equal '(("gzip" . 0) ("deflate" . 0))
		(webmachine::parse-header-accept-*
		 :encoding
		 "gzip, deflate")))


;;;;
;;;; TESTSUITE-RESOURCE-HANDLE-REQUEST
;;;;

(define-testcase test-v3b13 ()
  (with-testsuite-acceptor
      ((webmachine:make-constant-resource
	:path "/test/v3b13"
	:name 'test/v3b13
	:available-p nil))
    (with-http-reply ("/test/v3b13" :content-type :text/plain)
      (assert-http-status 503))))

(define-testcase test-v3b12 ()
  "The extension of allowed method list is not supported by Hunchentoot."
  (with-testsuite-acceptor
      ((webmachine:make-constant-resource
        :path "/test/v3b12-report"
        :name 'test/v3b12-report
        :known-methods '(:report)
        :allowed-methods '(:get :head :report))
       (webmachine:make-constant-resource
        :path "/test/v3b12-standard"
        :name 'test/v3b12-standard))
    (with-http-reply ("/test/v3b12-report" :method :report)
      (assert-http-status 200))
    (with-http-reply ("/test/v3b12-standard" :method :report)
      (assert-http-status 501))))

(define-testcase test-v3b11 ()
  (with-testsuite-acceptor
      ((webmachine:make-constant-resource
        :path "/test/v3b11-uri-too-long"
        :name 'test/v3b11-uri-too-long
        :uri-too-long-p t)
       (webmachine:make-constant-resource
        :path "/test/v3b11-uri-acceptably-long"
        :name 'test/v3b11-uri-acceptably-long
        :uri-too-long-p nil))
    (with-http-reply ("/test/v3b11-uri-acceptably-long")
      (assert-http-status 200))
    (with-http-reply ("/test/v3b11-uri-too-long")
      (assert-http-status 414))))

(define-testcase test-v3b10 ()
  (with-testsuite-acceptor
      ((webmachine:make-constant-resource
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
      ((webmachine:make-constant-resource
        :path "/test/v3b9-malformed"
        :name 'v3b9-malformed
        :valid-request-p nil)
       (webmachine:make-constant-resource
        :path "/test/v3b9-wellformed"
        :name 'v3b9-wellformed
        :valid-request-p t))
    (with-http-reply ("/test/v3b9-wellformed")
      (assert-http-status 200))
    (with-http-reply ("/test/v3b9-malformed")
      (assert-http-status 400))))

(define-testcase test-v3b8 ()
  (with-testsuite-acceptor
      ((webmachine:make-constant-resource
        :path "/test/v3b8-forbidden"
        :name 'v3b8-forbidden
        :authorized-p nil)
       (webmachine:make-constant-resource
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
      ((webmachine:make-constant-resource
        :path "/test/v3b7-forbidden"
        :name 'v3b7-forbidden
        :forbidden-p t)
       (webmachine:make-constant-resource
        :path "/test/v3b7-authorized"
        :name 'v3b7-authorized
        :forbidden-p nil))
    (with-http-reply ("/test/v3b7-authorized")
      (assert-http-status 200))
    (with-http-reply ("/test/v3b7-forbidden")
      (assert-http-status 403))))

(define-testcase test-v3b6 ()
  (with-testsuite-acceptor
      ((webmachine:make-constant-resource
        :path "/test/v3b6-invalid"
        :name 'v3b6-invalid
        :valid-content-headers-p nil)
       (webmachine:make-constant-resource
        :path "/test/v3b6-valid"
        :name 'v3b6-valid
        :valid-content-headers-p t))
    (with-http-reply ("/test/v3b6-valid")
      (assert-http-status 200))
    (with-http-reply ("/test/v3b6-invalid")
      (assert-http-status 501))))

(define-testcase test-v3b5 ()
  (with-testsuite-acceptor
      ((webmachine:make-constant-resource
        :path "/test/v3b5-invalid"
        :name 'v3b5-invalid
        :valid-content-type-p nil)
       (webmachine:make-constant-resource
        :path "/test/v3b5-valid"
        :name 'v3b5-valid
        :valid-content-type-p t))
    (with-http-reply ("/test/v3b5-valid")
      (assert-http-status 200))
    (with-http-reply ("/test/v3b5-invalid")
      (assert-http-status 415))))

(define-testcase test-v3b4 ()
  (with-testsuite-acceptor
      ((webmachine:make-constant-resource
        :path "/test/v3b4-payload-too-large"
        :name 'test/v3b4-payload-too-large
        :payload-too-large-p t)
       (webmachine:make-constant-resource
        :path "/test/v3b4-payload-acceptably-large"
        :name 'test/v3b4-payload-acceptably-large
        :payload-too-large-p nil))
    (with-http-reply ("/test/v3b4-payload-acceptably-large")
      (assert-http-status 200))
    (with-http-reply ("/test/v3b4-payload-too-large")
      (assert-http-status 413))))

(define-testcase test-v3b3 ()
  (with-testsuite-acceptor
      ((webmachine:make-constant-resource
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
      ((webmachine:make-constant-resource
        :path "/test/v3c3"
        :name 'test/v3c3
        :content-types-provided
	'(:text/plain :text/html :application/json)
	:response
        '((:text/plain . "Hello, world!")
          (:text/html . "<html>Hello, world!</html>")
          (:application/json . "\"Hello, world!\""))))
    (with-http-reply ("/test/v3c3" :accept "text/*;q=0.6")
      (assert-http-status 200)
      (assert-http-header-match :content-type "text/plain")
      (assert-http-body "Hello, world!"))
    (with-http-reply ("/test/v3c3"
		      :accept "text/html;q=1, text/*;q=0.6, application/json;q=0.8")
      (assert-http-status 200)
      (assert-http-header-match :content-type "text/html.*")
      (assert-http-body "<html>Hello, world!</html>"))))

(define-testcase test-v3d5 ()
  (with-testsuite-acceptor
      ((webmachine:make-constant-resource
        :path "/test/v3d5"
        :name 'test/v3d5
        :languages-provided '("en")))
    (with-http-reply ("/test/v3d5"
		      :additional-headers '(("accept-language" . "fr-FR,fr;q=1, en;q=0.6")))
      (assert-http-status 200)
      (assert-http-header-match :content-type "text/plain.*")
      (assert-http-header-match :content-language "en")
      (assert-http-body "A constant resource."))))

(define-testcase test-v3e6 ()
  (with-testsuite-acceptor
      ((webmachine:make-constant-resource
        :path "/test/v3e6"
        :name 'test/v3e6
        :charsets-provided
        '(:utf-8 :iso-8859-1)
        :content-types-provided
	'(:text/plain :text/html :application/json)
        :response
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
      ((webmachine:make-constant-resource
        :path "/test/v3f7"
        :name 'test/v3f7
        :encodings-provided
        '(:identity)))
    (with-http-reply ("/test/v3f7" :accept "text/plain"
				   :additional-headers
				   '(("accept-encoding" . "invalid-encoding")))
      (assert-http-status 406))
    (with-http-reply ("/test/v3f7" :accept "text/plain"
				   :additional-headers
				   '(("accept-encoding" .
				      "deflate;q=1, identity;q=0.1")))
      (assert-http-status 200)
      (assert-http-header-match :encoding "identity")
      (assert-http-body "A constant resource."))))

(define-testcase testsuite-resource-handle-request ()
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


;;;;
;;;; TEST-COMPATIBILITY-WITH-COMMON-BROWSERS
;;;;

(defclass test-compatibility-style (webmachine:constant-resource) nil
  (:default-initargs
   :path "/compatibility.css"
   :content-types-provided '(:text/css)
   :language-provided '(:en-US)
   :flexible-negotiation-p t
   :response "html { font-family: monospace; }"))

(defclass test-compatibility-document (webmachine:constant-resource) nil
  (:default-initargs
   :path "/compatibility.html"
   :content-types-provided '(:text/html)
   :language-provided '(:en-US)
   :flexible-negotiation-p t
   :response "<!DOCTYPE html>
<html>
  <head>
    <link rel=\"stylesheet\" href=\"/compatibility.css\">
    <script src =\"/compatibility.js\"></script>
  </head>
  <body>
    <h1>My First Heading</h1>
    <p>My first paragraph.</p>
  </body>
</html>"))

(defclass test-compatibility-script (webmachine:constant-resource) nil
  (:default-initargs
   :path "/compatibility.js"
   :content-types-provided '(:text/javascript)
   :language-provided '(:en-US)
   :flexible-negotiation-p t
   :response "var a = 1"))

(define-testcase test-compatibility-of-request (uri accept headers body)
  "Test that URI is succesfully retrieved and the returned body matches BODY."
  (with-testsuite-acceptor
      ((make-instance 'test-compatibility-style :name 'style)
       (make-instance 'test-compatibility-document :name 'document)
       (make-instance 'test-compatibility-script :name 'script))
    (with-http-reply (uri :accept accept :additional-headers headers)
      (assert-http-status 200)
      (assert-http-header-match :encoding "identity")
      (assert-http-header-match :content-language "en-US")
      (assert-http-body body))))

(define-testcase test-compatbility-with-safari-16.3 ()
  (let ((additional-headers
	  '(("Upgrade-Insecure-Requests" . "1")
	    ("Host" . "localhost:8000")
	    ("User-Agent" .
	     #.(concatenate 'string
		"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
		"AppleWebKit/605.1.15 (KHTML, like Gecko) "
		"Version/16.3 Safari/605.1.15"))
	    ("Accept-Language" . "en-GB,en;q=0.9")
	    ("Accept-Encoding" . "gzip, deflate")
	    ("Connection" . "keep-alive"))))
    (test-compatibility-of-request
     "/compatibility.html"
     "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
     additional-headers
     ".*<h1>My First Heading</h1>.*")
    (test-compatibility-of-request
     "/compatibility.css"
     "text/css,*/*;q=0.1"
     additional-headers
     ".*font-family: monospace.*")
    (test-compatibility-of-request
     "/compatibility.js"
     "*/*"
     additional-headers "var a")))

(define-testcase test-compatibility-with-chrome-107.0.5304.110 ()
  (let ((additional-headers
	  `(("Accept-Encoding" . "gzip, deflate, br")
	    ("Accept-Language" . "en-GB,en-US;q=0.9,en;q=0.8")
	    ("Cache-Control" . "no-cache")
	    ("Connection" . "keep-alive")
	    ("Host" . "localhost:8000")
	    ("Pragma" . "no-cache")
	    ("Upgrade-Insecure-Requests" . "1")
	    ("User-Agent" .
              #.(concatenate 'string
	       "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
	       "AppleWebKit/537.36 (KHTML, like Gecko) "
	       "Chrome/107.0.0.0 Safari/537.36"))
	    ("sec-ch-ua" .
	     "\"Google Chrome\";v=\"107\", \"Chromium\";v=\"107\", \"Not=A?Brand\";v=\"24\"")
	    ("sec-ch-ua-mobile" . "?0")
	    ("sec-ch-ua-platform" . "\"macOS\""))))
    (test-compatibility-of-request
     "/compatibility.html"
     #.(concatenate
	'string
	"text/html,application/xhtml+xml,a,pplication/xml;q=0.9,"
	"image/avif,image/webp,image/apng,*/*;q=0.8,"
	"application/signed-exchange;v=b3;q=0.9")
     (append additional-headers
	     '(("Sec-Fetch-Dest" . "document")
	       ("Sec-Fetch-Mode" . "navigate")
	       ("Sec-Fetch-Site" . "none")
	       ("Sec-Fetch-User" . "?1")))
     ".*<h1>My First Heading</h1>.*")
    (test-compatibility-of-request
     "/compatibility.css"
     "text/css,*/*;q=0.1"
     (append additional-headers
	     '(("Sec-Fetch-Dest" . "style")
	       ("Sec-Fetch-Mode" . "no-cors")
	       ("Sec-Fetch-Site" . "same-origin")))
     ".*font-family: monospace.*")
    (test-compatibility-of-request
     "/compatibility.js"
     "*/*"
     (append additional-headers
	     '(("Sec-Fetch-Dest" . "script")
	       ("Sec-Fetch-Mode" . "no-cors")
	       ("Sec-Fetch-Site" . "same-origin")))
     "var a")))

(define-testcase test-compatibility-with-firefox-111.0 ()
  (let ((additional-headers
	  `(("Accept-Encoding" . "gzip, deflate, br")
	    ("Accept-Language" . "en-US,en;q=0.5")
	    ("Cache-Control" . "no-cache")
	    ("Connection" . "keep-alive")
	    ("Host" . "localhost:8000")
	    ("Pragma" . "no-cache")
	    ("Upgrade-Insecure-Requests" . "1")
	    ("User-Agent" .
	      #.(concatenate
		 'string
		 "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:109.0) "
		 "Gecko/20100101 Firefox/111.0")))))
    (test-compatibility-of-request
     "/compatibility.html"
     #.(concatenate
	'string
	"text/html,application/xhtml+xml,application/xml;q=0.9,"
	"image/avif,image/webp,*/*;q=0.8")
     (append additional-headers
	     '(("Sec-Fetch-Dest" . "document")
	       ("Sec-Fetch-Mode" . "navigate")
	       ("Sec-Fetch-Site" . "cross-site")))
     ".*<h1>My First Heading</h1>.*")
    (test-compatibility-of-request
     "/compatibility.css"
     "text/css,*/*;q=0.1"
     (append additional-headers
	     '(("Referer" . "http://localhost:8000/compatibility.html")
	       ("Sec-Fetch-Dest" . "style")
	       ("Sec-Fetch-Mode" . "no-cors")
	       ("Sec-Fetch-Site" . "same-origin")))
     ".*font-family: monospace.*")
    (test-compatibility-of-request
     "/compatibility.js"
     "*/*"
     (append additional-headers
	     '(("Referer" . "http://localhost:8000/compatibility.html")
	       ("Sec-Fetch-Dest" . "script")
	       ("Sec-Fetch-Mode" . "no-cors")
	       ("Sec-Fetch-Site" . "same-origin")))
     "var a")))

(define-testcase testsuite-compatibility-with-common-browsers ()
  (test-compatbility-with-safari-16.3)
  (test-compatibility-with-chrome-107.0.5304.110)
  (test-compatibility-with-firefox-111.0))


;;;;
;;;; TESTSUITE-RESOURCE
;;;;

(define-testcase testsuite-resource ()
  (testsuite-parse-header-accept-*)
  (testsuite-resource-handle-request)
  (testsuite-compatibility-with-common-browsers))

;;;; End of file `resource.lisp'
