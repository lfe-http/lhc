(defmodule lhc-url
  (export all))

(include-lib "lhc/include/lhc-records.lfe")

;;; Constants

(defun netloc-regex () "(([^:]+)(:([^:@]+))?@)?([^:@]+)(:([^:]+))?")
(defun netloc-regex-opts () '(global #(capture (2 4 5 7) list)))

;;; Constructors

(defun new ()
  (new '() '()))

(defun new (named-args)
  (new named-args '()))

(defun new (named-args opts)
  (make-urldata
    scheme (proplists:get_value 'scheme named-args)
    netloc (make-netloc
              username (proplists:get_value 'username named-args)
              password (proplists:get_value 'password named-args)
              host (proplists:get_value 'host named-args)
              port (proplists:get_value 'port named-args))
    path (proplists:get_value 'path named-args)
    params (proplists:get_value 'params named-args)
    query (proplists:get_value 'query named-args)
    fragment (proplists:get_value 'fragment named-args)))

(defun new (scheme netloc path params query fragment opts)
  (new `(#(scheme ,scheme)
         #(netloc ,netloc)
         #(path ,path)
         #(params ,params)
         #(query ,query)
         #(fragment ,fragment))
       opts))

(defun new (scheme host port path params query fragment opts)
  (new `(#(scheme ,scheme)
         #(host ,host)
         #(port ,port)
         #(path ,path)
         #(params ,params)
         #(query ,query)
         #(fragment ,fragment))
        opts))

(defun new (scheme username password host port path params query fragment opts)
  (new `(#(scheme ,scheme)
         #(username ,username)
         #(password ,password)
         #(host ,host)
         #(port ,port)
         #(path ,path)
         #(params ,params)
         #(query ,query)
         #(fragment ,fragment))
        opts))

;;; Accessors

(defun get-scheme (urldata)
  (urldata-scheme urldata))

(defun get-netloc (urldata)
  (urldata-netloc urldata))

(defun get-username (urldata)
  (netloc-username (urldata-netloc urldata)))

(defun get-password (urldata)
  (netloc-password (urldata-netloc urldata)))

(defun get-host (urldata)
  (netloc-host (urldata-netloc urldata)))

(defun get-port (urldata)
  (netloc-port (urldata-netloc urldata)))

(defun get-path (urldata)
  (urldata-path urldata))

(defun get-params (urldata)
  (urldata-params urldata))

(defun get-query (urldata)
  (urldata-query urldata))

(defun get-fragment (urldata)
  (urldata-fragment urldata))

;;; Utility functions

(defun encode ()
  'noop)

(defun decode (path)
  (decode path (file:native_name_encoding)))

(defun decode (path encoding)
  'noop)

;;; Support functions

(defun parse-netloc (netloc-str)
  (case (re:run netloc-str (netloc-regex) (netloc-regex-opts))
    (`#(match ((,user ,pass ,host ,port)))
      (make-netloc username user password pass host host port port))
    ('nomatch
      (make-netloc))))
