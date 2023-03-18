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

(in-package #:org.melusina.webmachine)


;;;
;;; Header Analysis
;;;

(defparameter *header-accept-regex*
  (let* ((media-type-word
	   '(:alternation
	     (:greedy-repetition 1 nil :word-char-class)
	     (:char-class #\*)))
	 (media-type
	   `(:sequence ,media-type-word #\/ ,media-type-word))
	 (language
	   `(:sequence
	     (:char-class (:range #\a #\z)) (:char-class (:range #\a #\z))
	     (:greedy-repetition
	      0 1
	      (:sequence #\_ (:char-class (:range #\A #\Z)) (:char-class (:range #\A #\Z))))))
	 (charset
	   `(:alternation "UTF-8" "utf-8"))
	 (encoding
	   "identity")
	 (accept
	   `(:register
	     (:alternation ,media-type ,language ,charset ,encoding)))
	 (quality
	  `(:sequence ";q="
		      (:register
		       (:alternation
			(:sequence
			 #\0 (:char-class #\.) (:char-class (:range #\0 #\9))
			 (:greedy-repetition 0 1 (:char-class (:range #\0 #\9))))
			#\1
			"1.0" )))))
    `(:sequence :start-anchor ,accept (:greedy-repetition 0 1 ,quality) :end-anchor)))
 
	   

(defun parse-header-accept-* (text)
  "Parse an :ACCEPT-* header.
The response is an alist of (DESIGNATOR . QUALITY) sorted by decreasing
quality. (This is a stable sort.)"
  (labels ((read-accept-preference (word)
             (ppcre:register-groups-bind (designator quality)
                 (*header-accept-regex* word)
               (cons designator
                     (parse-number:parse-real-number
		      (or quality "1.0")))))
	   (split-accept-header (text)
	     (loop :for word :in (ppcre:split ",\\s*" text)
		   :collect (read-accept-preference word))))
    (let ((accept
	    (split-accept-header text)))
      (setf accept (delete-if-not #'car accept))
      (setf accept (stable-sort accept #'> :key #'cdr))
      accept)))

(defun negotiate-accept (accept offer)
  "Negotiate the :ACCEPT content type.
The ACCEPT parameter is an alist of (MEDIA-TYPE . QUALITY) sorted by decreasing
quality.  The OFFER parameter is an alist of (MEDIA-TYPE . HANDLER). The result
of the negotiation is the cell (MEDIA-TYPE . HANDLER) that best matches the
ACCEPT parameter, or NIL."
  (flet ((match-media-type-p (accept offer)
	   "Predicate recognising when the media type OFFER matches the string ACCEPT."
	   (string-match accept (slot-value (find-media-type offer) 'name))))
    (loop :for (accepted-media-type . _ ) :in accept
          :thereis (assoc accepted-media-type offer :test #'match-media-type-p))))

(defun negotiate-accept-language (accept offer)
  "Negotiate the :ACCEPT-LANGUAGE language.
The ACCEPT parameter is an alist of (LANGUAGE . QUALITY) sorted by
decreasing quality.  The OFFER parameter is a list of LANGUAGE
designators. The result of the negotiation is a LANGUAGE designator that
best matches the ACCEPT parameter, or NIL."
  (loop :for (accepted-language . _ ) :in accept
        :thereis (find accepted-language offer :test #'string=)))

(defun negotiate-accept-charset (accept offer)
  "Negotiate the :ACCEPT-CHARSET charset.
The ACCEPT parameter is an alist of (CHARSET . QUALITY) sorted by
decreasing quality.  The OFFER parameter is a list of CHARSET. The
result of the negotiation is the CHARSET that best matches the ACCEPT
parameter, or NIL."
  (loop :for (accepted-charset . _ ) :in accept
        :thereis (find accepted-charset offer :test #'string-equal)))

(defun negotiate-accept-encoding (accept offer)
  "Negotiate the :ACCEPT-ENCODING encoding.
The ACCEPT parameter is an alist of (ENCODING . QUALITY) sorted by
decreasing quality.  The OFFER parameter is a list of ENCODING. The
result of the negotiation is the ENCODING that best matches the ACCEPT
parameter, or NIL."
  (loop :for (accepted-encoding . _ ) :in accept
	:thereis (find accepted-encoding offer :test #'string-equal)))

;;;
;;; Resources
;;;

(defclass resource ()
  ((name
    :reader resource-name
    :initarg :name
    :initform (error "No resource NAME.")
    :documentation "The NAME of the resource.
This NAME is used in logging artefacts and introspective
presentations, to ease maintenance of the program.")
   (path
    :initarg :path
    :initform nil
    :documentation "The URI PATH where the resource is located.
The PATH is a list of strings and keyword. A string matches itself in
the URI PATH and a keyword matches a non-empty string made of characters
in the range [A-Za-z0-9_-] whose value shoud be bound to the parameter
denoted by the keyword."))
  (:documentation "Resources with HTTP Protocol semantics."))

(defvar *resource-repository* (make-hash-table)
  "The table of all resources.
When a resource is defined, it is added to this table, using
the function symbol as the key.")

(defun resource-p (thing)
  "Predicate recognising resources."
  (typep thing 'resource))

(defun find-resource (designator &key filter)
  "Find a resource whose name matches DESIGNATOR.
The DESIGNATOR can be a string, a keyword, a resource, or a symbol.

The argument FILTER can be set to a predicate that the found resource
must satisfy."
  (cond
    ((or (stringp designator)
         (keywordp designator))
     (flet ((name-equal-p (key designator)
	      (string-equal (string key) (string designator)))
	    (filter-p (resource)
	      (if filter
		  (funcall filter resource)
		  t)))
       (maphash (lambda (key resource)
                  (when (and (name-equal-p key designator)
			     (filter-p resource))
                    (return-from find-resource resource)))
		*resource-repository*)))
    ((resource-p designator)
     (find-resource (resource-name designator) :filter filter))
    ((and designator (symbolp designator))
     (let ((probe
	     (gethash designator *resource-repository*)))
       (when (or (not filter)
                 (funcall filter probe))
         probe)))
    (t
     (error "~A is not a resource designator." designator))))

(defun remove-resource (designator)
  "Remove a resource from resource repository."
  (let ((resource (find-resource designator)))
    (when resource
      (makunbound (resource-name resource))
      (remhash (resource-name resource) *resource-repository*))))


;;;
;;; Webmachine Logic Customisation
;;;

(defgeneric resource-available-p (resource)
  (:documentation
   "This predicate recognises if a resource is available.
When a resource is not available, requests it handles are answered
with a 503 Service Unavailable status code. (V3B13)

The method combination for this generic function is AND, so that
compound resources are unavailable if a participating resource
is unavailable.

Generic resources are always available.")
  (:method-combination and)
  (:method and (resource) t))

(defgeneric resource-known-methods (resource)
  (:documentation
   "The list of HTTP methods supported by the resource.
When a resource does not support the HTTP method of a request, that
request is answered with a 501 Not Implemented status code. (V3B12)

The HyperText Transfer Protocol (HTTP) 501 Not Implemented server error
response code indicates that the server does not support the
functionality required to fulfill the request. This is the appropriate
response when the server does not recognize the request method and is
not capable of supporting it for any resource. The only request methods
that servers are required to support (and therefore that must not return
this code) are :GET and :HEAD.

The method combination for this generic function is APPEND, so that
compound resources support all methods supported by participating
resources.

The default value presents the de facto standard list of HTTP methods
but could be extendend to implement HTTP extensions.  The actual
list is:

  :GET :HEAD :POST :PUT :DELETE :TRACE :CONNECT and :OPTIONS

This must not be confused with the `resource-allowed-methods' method.")
  (:method-combination append)
  (:method append (resource)
    '(:get :head :post :put :delete :trace :connect :options)))

(defgeneric resource-uri-too-long-p (resource request)
  (:documentation
   "This predicate recognises if a URI is too long.
When a URI of request is too long, the resource answers that request
with a 414 Request-URI Too Long. (V3B11)

The HTTP 414 URI Too Long response status code indicates that the URI
requested by the client is longer than the server is willing to
interpret.

There are a few rare conditions when this might occur:

  - when a client has improperly converted a POST request to a GET
    request with long query information,

  - when the client has descended into a loop of redirection (for
    example, a redirected URI prefix that points to a suffix of itself),

  - or when the server is under attack by a client attempting to exploit
    potential security holes.

Generic resources accept all URI resources length.")
  (:method (resource request) nil))

(defgeneric resource-payload-too-large-p (resource request)
  (:documentation
   "This predicate recognises if a request entity is too large.
When a request entity is too large, the resource answers that request
with a 413 Payload Too Large. (V3B4)

The HTTP 413 Payload Too Large response status code indicates that the
request entity is larger than limits defined by server; the server might
close the connection or return a Retry-After header field.")
  (:method (resource request) nil))

(defgeneric resource-allowed-methods (resource)
  (:documentation
   "The list of allowed methods on a resource.
A request to this resource whose method is not included in the returned
list will result in a 405 Method Not Allowed. The response will include
an :ALLOW header that lists the allowed methods. (V3B10)

The HyperText Transfer Protocol (HTTP) 405 Method Not Allowed response
status code indicates that the request method is known by the server but
is not supported by the target resource.

The server MUST generate an Allow header field in a 405 response
containing a list of the target resource's currently supported methods.

The method combination for this generic function is the standard
one. Generic resources allow :GET and :HEAD resources.")
  (:method (resource) '(:get :head)))

(defgeneric resource-valid-request-p (resource request)
  (:documentation
   "This predicate recognises if a request is valid.
When a request is invalid, the resource answers that request with a 400
Bad Request. (V3B9)

The HyperText Transfer Protocol (HTTP) 400 Bad Request response status
code indicates that the server cannot or will not process the request
due to something that is perceived to be a client error (e.g., malformed
request syntax, invalid request message framing, or deceptive request
routing).

The method combination for this generic function is AND, so that a
request is valid for a compound resource if all participating resources
consider the request valid.

Generic resources accept all requests.")
  (:method-combination and)
  (:method and (resource request) t))

(defgeneric resource-authorized-p (resource request)
  (:documentation
   "This predicate recognises if a request is authorized.
When a request is unauthorized, the resource answers that request with a
401 Unauthorized. (V3B8)

The HTTP 401 Unauthorized client error status response code indicates
that the request has not been applied because it lacks valid
authentication credentials for the target resource.

This status is sent with a WWW-Authenticate header that contains
information on how to authorize correctly.

This status is similar to 403, but in this case, authentication is possible.

Generic resources accept all requests.")
  (:method (resource request) t))

(defgeneric resource-forbidden-p (resource request)
  (:documentation
   "This predicate recognises if a request tries to access a forbidden resource.
When access to a resource is forbidden, the resource answers that
request with a 403 Forbidden. (V3B7)

The HTTP 403 Forbidden client error status response code indicates that
the server understood the request but refuses to authorize it.

This status is similar to 401, but in this case, re-authenticating will
make no difference. The access is permanently forbidden and tied to the
application logic, such as insufficient rights to a resource.

The method combination for this generic function is OR, so that an
access is forbidden for a compound resource if a participating resource
considers the access is forbidden.

Generic resources consider no access to be forbidden.")
  (:method-combination or)
  (:method or (resource request) nil))

(defgeneric resource-valid-content-headers-p (resource request)
  (:documentation
   "This predicate recognises if a request bears valid Content-* headers.
When a request has invalid content headers, the resource answers that
request with a 501 Not Implemented. (V3B6)

The HTTP 501 Not Implemented server error response code indicates that
the server does not support the functionality required to fulfill the
request. This is the appropriate response when the server does not
recognize the request method and is not capable of supporting it for any
resource. The only request methods that servers are required to
support (and therefore that must not return this code) are :GET and
:HEAD.

The method combination for this generic function is AND, so that a
request has valid content headers for a compound resource if all
participating resources consider the request to have valid content
headers.

Generic resources accept all requests.")
  (:method-combination and)
  (:method and (resource request) t))

(defgeneric resource-valid-content-type-p (resource request)
  (:documentation
   "This predicate recognises if a request bears valid content type.
When a request has invalid content type, the resource answers that
request with a 415 Unsupported Media Type. (V3B5)

The HTTP 415 Unsupported Media Type client error response code indicates
that the server refuses to accept the request because the payload format
is in an unsupported format.

The format problem might be due to the request's indicated Content-Type
or Content-Encoding, or as a result of inspecting the data directly.

The method combination for this generic function is AND, so that
a request has valid content type for a compound resource if
all participating resources consider the request to have valid
content type.

Generic resources accept all requests.")
  (:method-combination and)
  (:method and (resource request) t))

(defgeneric resource-options (resource)
  (:documentation
   "Headers to put in the answer of an :OPTIONS request.
If the OPTIONS method is supported and is used, the return value of
this function is expected to be an alist of pairs representing header
names and values that should appear in the response.

The method combination for this generic function is APPEND, so that
compound resources support all methods supported by participating
resources. When several values for the same header are provided,
only the header value defined by the most specialised class is used.")
  (:method-combination append)
  (:method append (resource) nil))

(defgeneric resource-languages-provided (resource)
  (:documentation
   "Content negociation for accepted languages.
This should return or list of languages, where each language is
identified by a string.

When a request has invalid accept language headers, the resource answers
that request with a 406 Not Acceptable. (V3D5)")
  (:method (resource) nil))

(defgeneric resource-content-types-provided (resource)
  (:documentation
   "Content negociation for accepted content types.
This should return a list of pairs where each pair is of the form
(MEDIA-TYPE . HANDLER) where MEDIA-TYPE is a string of content-type
format and the HANDLER is an atom naming the function which can
provide a resource representation in that media type. Content
negotiation is driven by this return value. For example, if a client
request includes an :ACCEPT header with a value that does not appear
as a first element in any of the return tuples, then a
406 Not Acceptable will be sent.

Default: (:text/html . 'resource-to-html)")
  (:method (resource) '((:text/html . resource-to-text/html))))

(defgeneric resource-to-text/html (resource request destination)
  (:documentation
   "Provide a text/html representation of a resource.")
  (:method (resource request destination)
    (http-error
     501
     "Cannot provide a text/html representation of this resource")))

(defgeneric resource-charsets-provided (resource)
  (:documentation
   "Content negociation for accepted charsets.
This should return a list of charsets or external formats designators.
Content negotiation is driven by this return value. For example, if a
client request includes an :ACCEPT-CHARSET header with a value that does
not appear as a first element in any of the return tuples, then a 406
Not Acceptable will be sent.

Default: (:utf-8)")
  (:method (resource) '(:utf-8)))

(defgeneric resource-encodings-provided (resource)
  (:documentation
   "Content negociation for accepted encodings.
This should return a list of encoding designators.  Content negotiation
is driven by this return value. For example, if a client request
includes an :ACCEPT-ENCODING header with a value that does not appear as
a first element in any of the return tuples, then a 406 Not Acceptable
will be sent.

Default: (:identity)")
  (:method (resource) '(:identity)))


;;;;
;;;; Handle Request
;;;;

(defun resource-handle-request (resource request reply)
  "Handle the REQUEST using RESOURCE and sending back the REPLY.
This walks down the decision graph of the Webmachine."
  (declare (optimize (debug 3) (safety 3)))
  (labels
      ((response ()
         (let ((external-format
		 (when (slot-value request 'charset)
                   (flexi-streams:make-external-format (slot-value request 'charset)))))
           (setf (hunchentoot:return-code* reply) 200)
           (with-content-output-to-sequence (response-body
                                             :external-format external-format
                                             :encoding (slot-value request 'encoding))
             (setf (hunchentoot:header-out :content-type reply)
                   (hunchentoot::maybe-add-charset-to-content-type-header
                    (hunchentoot:header-out :content-type reply)
                    external-format))
             (typecase (slot-value reply 'content-handler)
               (string
		(write-string (slot-value reply 'content-handler) response-body))
               (t
		(funcall (slot-value reply 'content-handler) resource request response-body))))))
       (make-allow-header (allowed-methods)
         (with-output-to-string (allow)
           (format allow "~{~A~^, ~}" allowed-methods)))
       (set-header-out (headers reply)
	 "Set outgoing HEADERS on REPLY, without overwriting them."
	 (mapcar (lambda (key-value)
                   (destructuring-bind (key . value) key-value
                     (unless (hunchentoot:header-out key reply)
                       (setf (hunchentoot:header-out key reply) value))))
		 headers))
       (halt (status-code)
         (http-error status-code))
       (choose-content-type (&optional content-type)
	 (let ((chosen
		 (or content-type
		     (negotiate-accept
		      (parse-header-accept-* (hunchentoot:header-in :accept request))
		      (resource-content-types-provided resource)))))
	   (when chosen
	     (destructuring-bind (media-type . content-handler) chosen
	       (setf media-type
		     (find-media-type media-type))
               (setf (hunchentoot:header-out :content-type reply)
		     (media-type-name media-type))
	       (setf (slot-value reply 'content-handler)
		     content-handler)))
	   chosen))
       (choose-language ()
	 (let ((language
                 (negotiate-accept-language
                  (parse-header-accept-* (hunchentoot:header-in :accept-language request))
                  (resource-languages-provided resource))))
           (when language
             (setf (slot-value request 'language) language))))
       (choose-charset (&optional charset)
	 (let ((chosen
		 (or charset
                     (negotiate-accept-charset
                      (parse-header-accept-* (hunchentoot:header-in :accept-charset request))
                      (resource-charsets-provided resource)))))
           (setf (slot-value request 'charset) chosen)))
       (choose-encoding ()
	 (let ((encoding
                 (negotiate-accept-encoding
                  (parse-header-accept-* (hunchentoot:header-in :accept-encoding request))
                  (resource-encodings-provided resource))))
	   (when encoding
             (setf (slot-value request 'encoding) encoding)
	     (setf (hunchentoot:header-out :encoding reply)
		   (string-downcase encoding)))))
       (v3b13 ()
         (if (resource-available-p resource)
	     (v3b12)
             (halt 503)))
       (v3b12 ()
         (if (position (hunchentoot:request-method request) (resource-known-methods resource))
	     (v3b11)
             (halt 501)))
       (v3b11 ()
         (if (resource-uri-too-long-p resource request)
             (halt 414)
             (v3b10)))
       (v3b10 ()
         (if (position (hunchentoot:request-method request) (resource-allowed-methods resource))
             (progn
               (setf (hunchentoot:header-out :allow reply)
                     (make-allow-header (resource-allowed-methods resource)))
               (v3b9))
             (halt 405)))
       (v3b9 ()
         (if (resource-valid-request-p resource request)
             (v3b8)
             (halt 400)))
       (v3b8 ()
         (multiple-value-bind (authorized www-authenticate)
             (resource-authorized-p resource request)
           (if authorized
               (v3b7)
               (progn
                 (setf (hunchentoot:header-out :www-authenticate reply) www-authenticate)
                 (halt 401)))))
       (v3b7 ()
         (if (resource-forbidden-p resource request)
             (halt 403)
             (v3b6)))
       (v3b6 ()
         (if (resource-valid-content-headers-p resource request)
             (v3b5)
             (halt 501)))
       (v3b5 ()
         (if (resource-valid-content-type-p resource request)
             (v3b4)
             (halt 415)))
       (v3b4 ()
         (if (resource-payload-too-large-p resource request)
             (halt 413)
             (v3b3)))
       (v3b3 ()
         (if (eq :options (hunchentoot:request-method request))
             (progn
	       (set-header-out (resource-options resource) reply)
               (setf (hunchentoot:return-code* reply) 204)
               nil)
             (v3c3)))
       (v3c3 ()
         (let ((content-types-provided
                 (resource-content-types-provided resource))
               (content-types-accepted
                 (hunchentoot:header-in :accept request)))
           (cond
             ((and (null content-types-accepted) (null content-types-provided))
              (halt 500))
             ((null content-types-accepted)
              (choose-content-type (first content-types-provided))
              (v3d4))
             (t
              (v3c4)))))
       (v3c4 ()
         (if (choose-content-type)
	     (v3d4)
	     (halt 406)))
       (v3d4 ()
         (if (hunchentoot:header-in :accept-language request)
             (v3d5)
             (v3e5)))
       (v3d5 ()
         (if (choose-language)
             (v3e5)
             (halt 406)))
       (v3e5 ()
         (let ((charsets-provided
                 (resource-charsets-provided resource))
               (charsets-accepted
                 (hunchentoot:header-in :accept-charset request)))
           (if charsets-accepted
               (v3e6)
               (progn
		 (choose-charset (first charsets-provided))
		 (v3f6)))))
       (v3e6 ()
         (if (choose-charset)
             (v3f6)
             (halt 406)))
       (v3f6 ()
         (if (hunchentoot:header-in :accept-encoding request)
             (v3f7)
             (v3g7)))
       (v3f7 ()
         (if (choose-encoding)
             (v3g7)
             (halt 406)))
       (v3g7 ()
         (response)))
    (v3b13)))

;;;; End of file `resource.lisp'
