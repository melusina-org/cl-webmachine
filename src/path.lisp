;;;; path.lisp — URI Path for Webmachine

;;;; Webmachine (https://github.com/melusina-org/cl-webmachine)
;;;; This file is part of Webmachine.
;;;;
;;;; Copyright © 2018–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:webmachine)


;;;
;;; URI Paths
;;;
(defun tokenize (token-spec text &key (start 0) (end (length text)))
  "Tokenize TEXT according to TOKEN-SPEC.
The TOKEN-SPEC is an alist mapping token class names to a regular
expression matching tokens of that class.  The tokenization process
scans repeatedly the text using the provided regular expressions, and
returns a list of pairs whose first element is the token class name and
the second element the token text."
  (labels ((scan-token (token-name token-regex text &key (start 0) (end (length text)))
	     (multiple-value-bind (match-start match-end)
		 (ppcre:scan token-regex text :start start :end end)
	       (if (and match-start (= match-start start) (< match-start match-end))
		   (values (cons token-name (subseq text match-start match-end)) match-end)
		   (values nil match-start))))
	   (scan (token-spec text &key (start 0) (end (length text)))
	     (loop :with token :and next-start
		   :for (token-name . token-regex) :in token-spec
		   :do (setf (values token next-start)
			     (scan-token token-name token-regex text :start start :end end))
		   :until token
		   :finally (return (values token next-start)))))
  (loop :with token :and next-start = start
        :do (setf (values token next-start) (scan token-spec text :start next-start :end end))
        :when token
        :collect token :into tokens
        :when (= end next-start)
        :do (return tokens)
        :when (not token)
        :do (error "Invalid token starting at position ~A (~A) in ~S."
		   next-start (char text next-start) text))))

(defun parse-path (concrete-path)
  "Parse the CONCRETE-PATH into a list of PATH rules.
A path rule is either

- A STRING, matching itself;
- A KEYWORD, matching a safe string in the URI Path;
- A BOOLEAN, only allowed in the last place, indicating a greedy path.

Example valid concrete paths are

  \"/\"
  \"/chrome/bootstrap/*\"
  \"/user/:id\"
  \"/user/:id/activity/*\"

Note: Safe strings are made of the characters in the range [A-Za-z0-9_-].
Note: It is an error to use :RELATIVE-PATH as a keword in the CONCRETE-PATH.
"
  (let ((tokens
	  (handler-case
              (tokenize '((:star . "[*]$")
                          (:literal . "[^:*]*")
                          (:parameter . ":[0-9A-Za-z_-]*"))
                        concrete-path)
            (error (c)
              (declare (ignore c))
              (error "The CONCRETE-PATH ~S is not valid." concrete-path)))))
    (loop :for (token-name . token-text) :in tokens
          :collect
          (ecase token-name
            (:literal
             token-text)
            (:parameter
             (intern (string-upcase (subseq token-text 1))
                     (find-package :keyword)))
            (:star
             :relative-path)))))

(defun path-regex (path)
  "Map the symbolic PATH to a PPCRE matching corresponding URI paths.
Parameters are transformed to registers."
  (flet ((path-regex-1 (path-1)
           (cond
             ((stringp path-1)
              path-1)
             ((eq path-1 :relative-path)
              '(:register (:greedy-repetition 1 nil :everything)))
             ((keywordp path-1)
              '(:register (:greedy-repetition
			   1 nil
			   (:char-class
                            (:range #\A #\Z)
                            (:range #\a #\z)
                            (:range #\0 #\9)
                            #\_
                            #\-))))
             (t
              (error "~S: This expression is not supported." path-1)))))
    (cons :sequence (mapcar #'path-regex-1 path))))

(defun path-parameters (path)
  "The list of keyword parameters bound in a PATH."
  (loop :for path-1 :in path
        :when (keywordp path-1)
        :collect path-1))

(defun match-path (path uri)
  "Match URI against the PATH pattern.
If the URI matches PATH then T and an alist mapping parameters to
the corresponding text are returned as multiple values. In this
alist, the character name is used as a key.  The key to a star
parameter is the keyword :RELATIVE-PATH.

If the URI does not match PATH, then NIL is returned."
  (let ((actual-path
	  (if (stringp path) (parse-path path) path)))
    (flet ((full-match-p (match-start match-end)
	     (and match-start
		  (eql match-start 0)
		  (eql match-end (length uri))))
	   (parameter-values (reg-starts reg-ends)
	     (loop :for parameter-name :in (path-parameters actual-path)
		   :for reg-start :across reg-starts
		   :for reg-end :across reg-ends
		   :collect (cons parameter-name
				  (subseq uri reg-start reg-end)))))
      (multiple-value-bind (match-start match-end reg-starts reg-ends)
          (ppcre:scan (path-regex actual-path) uri)
	(when (full-match-p match-start match-end)
	  (values t (parameter-values reg-starts reg-ends)))))))

;;;; End of file `path.lisp'
