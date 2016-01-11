(defmodule lhc-url
  (export all))

(include-lib "lhc/include/lhc-records.lfe")

(defun new (named-args opts)
  (make-urldata
    scheme (proplists:get_value 'scheme named-args)
    netloc (make-netloc
              username (proplists:get_value 'username named-args)
              password (proplists:get_value 'password named-args)
              host (proplists:get_value 'host named-args)
              port (proplists:get_value 'port named-args))
    params (proplists:get_value 'params named-args)
    query (proplists:get_value 'query named-args)
    fragment (proplists:get_value 'fragment named-args)))

(defun new (scheme netloc params query fragment opts)
  (new `(#(scheme ,scheme)
         #(netloc ,netloc)
         #(params ,params)
         #(query ,query)
         #(fragment ,fragment))
       opts))

(defun new (scheme host port params query fragment opts)
  (new `(#(scheme ,scheme)
         #(host ,host)
         #(port ,port)
         #(params ,params)
         #(query ,query)
         #(fragment ,fragment))
        opts))

(defun new (scheme username password host port params query fragment opts)
  (new `(#(scheme ,scheme)
         #(username ,username)
         #(password ,password)
         #(host ,host)
         #(port ,port)
         #(params ,params)
         #(query ,query)
         #(fragment ,fragment))
        opts))

(defun encode ()
  'noop)

(defun decode (path)
  (decode path (file:native_name_encoding)))

(defun decode (path encoding)
  'noop)
