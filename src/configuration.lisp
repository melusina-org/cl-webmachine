;;;; configuration.lisp — Configuration for Webmachine

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

(defparameter *catch-errors* t
  "If non-NIL, catch any error and explain them to the client.

If T (the default), *any* erroneous conditions that are a *fault of
the server and not of the user-agent*, including errors *and* HTTP
500-class conditions voluntarily signalled by the program logic, will
still result in a response to the user-agent.

To compose this response, Webmachine will first try EXPLAIN-CONDITION to
“politely” explain the condition in a format accepted by the user-agent, as
indicated in the :ACCEPT header. If that fails, the error is presented very
succintly to the client.

If set to NIL, errors will bubble up out of Webmachine and possible land you in
the debugger. This option is also useful during development if you prefer an
interactive debugger")

;;;; End of file `configuration.lisp'
