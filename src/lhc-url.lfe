(defmodule lhc-url
  (export all))

(include-lib "clj/include/predicates.lfe")
(include-lib "lhc/include/lhc-records.lfe")

;;; Constants

(defun user-regex () "([^:]+)")
(defun pass-regex () "([^:@]+)")
(defun user-pass-regex () (++ "(" (user-regex) "(:" (pass-regex) ")?@)?"))
(defun host-regex () "([^:@/#?]+)")
(defun port-regex () "([^:/#?]+)")
(defun host-port-regex () (++ (host-regex) "(:" (port-regex) ")?"))
(defun netloc-regex () (++ (user-pass-regex) (host-port-regex)))
(defun netloc-regex-opts () '(global #(capture (2 4 5 7) list)))

(defun scheme-regex () "([^:]+)://")
(defun path-regex () "(/[^?#]*)?")
(defun query-regex () "(\\?([^#]*))?")
(defun fragment-regex () "(#.*)?")
(defun url-regex ()
  (++ (scheme-regex)
      (netloc-regex)
      (path-regex)
      (query-regex)
      (fragment-regex)))
(defun url-regex-opts () '(global #(capture (1 3 5 6 8 9 11 12) list)))

(defun rfc1738-safe () "$-_.+!*'(),:/")
(defun rfc3986-safe ()
  (++ "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      "abcdefghijklmnopqrstuvwxyz"
      "0123456789"
      "_.-~"))

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

(defun encode (chars)
  (encode chars (++ (rfc3986-safe) (rfc1738-safe))))

(defun encode
  ;; This was translated from yaws_api:url_encode, with changes.
  (('() _)
    '())
  (((cons head tail) safe) (when (is_list head))
    (cons (encode head safe) (encode tail safe)))
  (((cons head tail) safe)
    (if (lists:member head safe)
      (cons head (encode tail safe))
      (case (erlang:integer_to_list head 16)
        (`(,x ,y)
          `(#\% ,x ,y . ,(encode tail safe)))
        (`(,x)
          `(#\% #\0 ,x . ,(encode tail safe)))))))

(defun decode (path)
  (decode path (file:native_name_encoding)))

(defun decode (path encoding)
  'noop)

(defun ->url ()
  'noop)

(defun ->qstring
  ((term) (when (is_map term))
    (map->qstring term))
  ((term) (when (is_list term))
    (if (proplist? term)
        (plist->qstring term)
        #(error unsupported-type)))
  ((term) (when (is_tuple term))
    (if (is_record term 'urldata)
      (->qstring (urldata-query term))
      #(error unsupported-type)))
  ((_)
    #(error unsupported-type)))

(defun parse (url-str)
  (parse url-str '()))

(defun parse (url-str opts)
  (case (re:run url-str (url-regex) (url-regex-opts))
    (`#(match ((,scheme ,user ,pass ,host ,port ,path ,query ,fragment)))
      (make-urldata
        scheme scheme
        netloc (make-netloc
                 username user
                 password pass
                 host host
                 port port)
        path path
        query query
        fragment fragment))
    ('nomatch
      #(error unparsable-string))))

;;; Support functions

(defun parse-netloc (netloc-str)
  (case (re:run netloc-str (netloc-regex) (netloc-regex-opts))
    (`#(match ((,user ,pass ,host ,port)))
      (make-netloc username user password pass host host port port))
    ('nomatch
      #(error unparsable-string))))

(defun plist->qstring (plist-data)
  (lists:flatten
    (lists:foldl
      (match-lambda
        ((`#(,k ,v) acc)
          (get-query-pair k v acc)))
      ""
      plist-data)))

(defun map->qstring (map-data)
  (lists:flatten
    (maps:fold #'get-query-pair/3 "" map-data)))

(defun get-query-pair
  ((k v "")
    (format-pair k v))
  ((k v acc)
    (++ acc "&" (format-pair k v))))

(defun format-pair (k v)
  (io_lib:format "~p=~p" `(,k ,v)))
