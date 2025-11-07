(defmodule lhc-util
  "Utility functions for LHC library.

  This module provides various utility functions for header manipulation,
  URI parsing, data conversion, and other common operations used throughout
  the lhc library."
  (export all))

;;; ===================================================================
;;; Preserved from v0.4.x
;;; ===================================================================

(defun user-agent ()
  "Generate a User-Agent string for HTTP requests.

  Returns:
    A string identifying the LFE Little HTTP Client with version info"
  ;; The following code renders a user agent string along these lines:
  ;;    "LFE Little HTTP Client/2.0.0 (LFE 2.1.1; Erlang 27)"
  (++ "LFE Little HTTP Client/"
      (lhc-vsn:version)
      " (LFE " (lhc-vsn:get 'lfe)
      "; Erlang " (erlang:system_info 'otp_release)
      ") (+http://github.com/lfe-http/lhc)"))

(defun utf8-encode
  ((characters) (when (is_binary characters))
    characters)
  ((characters) (when (is_list characters))
    (if (io_lib:printable_unicode_list characters)
      (unicode:characters_to_binary characters 'utf8)
      (list_to_binary characters))))

(defun utf8-encode
  ((characters #(hex))
    (lists:map
      (lambda (x)
        (integer_to_list x 16))
      (utf8-encode characters #(list))))
  ((characters #(list))
    (binary_to_list (utf8-encode characters)))
  ((characters #(binary))
    (utf8-encode characters)))

(defun utf8-decode (binary)
  (binary_to_list binary))

;; TODO: Move to lutil-types.lfe
(defun proplist?
  ((data) (when (is_list data))
    (if (lists:all #'proplist-kv?/1 data)
      'true
      'false))
  ((_)
    'false))

;; TODO: Move to lutil-types.lfe
(defun proplist-kv?
  ((`#(,key ,_)) (when (is_atom key))
    'true)
  ((bool-key) (when (is_atom bool-key))
    'true)
  ((_)
    'false))

;;; ===================================================================
;;; New Utilities for v2.0
;;; ===================================================================

(defun ensure-binary
  "Convert various data types to binary.

  Args:
    data: String, binary, iolist, or atom to convert

  Returns:
    Binary representation of the data"
  ((data) (when (is_binary data))
   data)
  ((data) (when (is_list data))
   (list_to_binary data))
  ((data) (when (is_atom data))
   (atom_to_binary data 'utf8))
  ((data) (when (is_integer data))
   (integer_to_binary data))
  ((data)
   ;; Fallback for other types
   (list_to_binary (io_lib:format "~p" `(,data)))))

(defun lowercase
  "Convert a binary string to lowercase.

  Args:
    binary: Binary string to convert

  Returns:
    Lowercased binary string"
  ((binary) (when (is_binary binary))
   (list_to_binary (string:lowercase (binary_to_list binary))))
  ((string) (when (is_list string))
   (string:lowercase string)))

(defun header-value
  "Find a header value by name (case-insensitive).

  Args:
    headers: List of {name, value} tuples
    name: Header name to find (binary, string, or atom)

  Returns:
    Binary value if found, 'undefined otherwise"
  ((headers name)
   (let ((name-lower (lowercase (ensure-binary name))))
     (header-value-loop headers name-lower))))

(defun header-value-loop
  (('() _name)
   'undefined)
  ((`(#(,h-name ,h-value) . ,rest) name)
   (case (=:= (lowercase (ensure-binary h-name)) name)
     ('true (ensure-binary h-value))
     ('false (header-value-loop rest name)))))

(defun merge-headers
  "Merge two header lists, with h2 overriding h1.

  Headers are matched case-insensitively. If a header appears in both
  lists, the value from h2 is used.

  Args:
    h1: First header list (base headers)
    h2: Second header list (override headers)

  Returns:
    Merged header list with binary keys and values"
  ((h1 h2)
   (let* ((h1-normalized (normalize-headers h1))
          (h2-normalized (normalize-headers h2))
          (h1-map (headers-to-map h1-normalized))
          (h2-map (headers-to-map h2-normalized))
          (merged (maps:merge h1-map h2-map)))
     (maps:to_list merged))))

(defun headers-to-map
  "Convert header list to a map with lowercase keys."
  ((headers)
   (lists:foldl
    (lambda (header acc)
      (case header
        (`#(,name ,value)
         (maps:put (lowercase (ensure-binary name))
                   (ensure-binary value)
                   acc))
        (_ acc)))
    (map)
    headers)))

(defun timestamp ()
  "Get current timestamp in milliseconds.

  Returns:
    Integer timestamp in milliseconds since Unix epoch"
  (let ((`#(,mega ,sec ,micro) (erlang:timestamp)))
    (+ (* (+ (* mega 1000000) sec) 1000)
       (div micro 1000))))

(defun parse-content-type
  "Parse a Content-Type header value into components.

  Args:
    value: Content-Type header value (binary or string)

  Returns:
    Map with keys: type, subtype, params
    Example: #{type => #\"text\", subtype => #\"html\",
              params => #{#\"charset\" => #\"utf-8\"}}"
  ((value)
   (let* ((value-bin (ensure-binary value))
          (parts (binary:split value-bin #";" '(global trim_all)))
          (media-type (car parts))
          (params (parse-ct-params (cdr parts))))
     (case (binary:split media-type #"/")
       (`(,type ,subtype)
        (maps:from_list
         `(#(type ,(string:trim type))
           #(subtype ,(string:trim subtype))
           #(params ,params))))
       (_
        (maps:from_list
         `(#(type ,media-type)
           #(subtype ,(binary))
           #(params ,params))))))))

(defun parse-ct-params
  "Parse Content-Type parameters into a map."
  ((params)
   (lists:foldl
    (lambda (param acc)
      (case (binary:split param #"=")
        (`(,key ,value)
         (maps:put (string:trim key)
                   (string:trim value #"\"")
                   acc))
        (_
         acc)))
    (map)
    params)))

(defun normalize-headers
  "Convert various header formats to standard binary tuples.

  Accepts:
    - List of tuples: [{name, value}, ...]
    - Map: #{name => value}
    - Mixed string/binary/atom keys and values

  Returns:
    List of {binary(), binary()} tuples with lowercase names"
  ((headers) (when (is_map headers))
   (lists:map
    (lambda (header)
      (let ((`#(,name ,value) header))
        `#(,(lowercase (ensure-binary name))
           ,(ensure-binary value))))
    (maps:to_list headers)))

  ((headers) (when (is_list headers))
   (lists:filtermap
    (lambda (header)
      (case header
        (`#(,name ,value)
         `#(true #(,(lowercase (ensure-binary name))
                   ,(ensure-binary value))))
        ;; Skip invalid entries
        (_ 'false)))
    headers))

  ((_)
   '()))

(defun parse-uri
  "Parse a URI string into component map.

  Args:
    uri: URI string (binary or string)

  Returns:
    {ok, uri-map} or {error, error-map}
    where uri-map contains: scheme, host, port, path, query, fragment"
  ((uri) (when (is_binary uri))
   (parse-uri (binary_to_list uri)))

  ((uri) (when (is_list uri))
   (case (uri_string:parse uri)
     (uri-map (when (is_map uri-map))
      (let ((normalized (normalize-uri-map uri-map)))
        `#(ok ,normalized)))
     (`#(error ,reason ,_term)
      (lhc-error:normalize 'lhc `#(invalid-uri ,reason)))
     (_
      (lhc-error:normalize 'lhc `#(invalid-uri ,uri)))))

  ((_)
   (lhc-error:normalize 'lhc '#(invalid-uri "Invalid URI type"))))

(defun normalize-uri-map
  "Normalize URI map from uri_string:parse to standard format."
  ((uri-map)
   (maps:from_list
    `(#(scheme ,(ensure-binary (maps:get 'scheme uri-map "")))
      #(host ,(ensure-binary (maps:get 'host uri-map "")))
      #(port ,(maps:get 'port uri-map 'undefined))
      #(path ,(ensure-binary (maps:get 'path uri-map "/")))
      #(query ,(ensure-binary (maps:get 'query uri-map "")))
      #(fragment ,(ensure-binary (maps:get 'fragment uri-map "")))))))

(defun build-uri
  "Build a URI string from component map.

  Args:
    uri-map: Map with optional keys: scheme, host, port, path, query, fragment

  Returns:
    {ok, binary} or {error, error-map}"
  ((uri-map) (when (is_map uri-map))
   (let ((erlang-uri-map (uri-map-to-erlang uri-map)))
     (case (uri_string:recompose erlang-uri-map)
       (uri (when (is_list uri))
        `#(ok ,(list_to_binary uri)))
       (uri (when (is_binary uri))
        `#(ok ,uri))
       (`#(error ,reason ,_term)
        (lhc-error:normalize 'lhc `#(invalid-uri ,reason)))
       (_
        (lhc-error:normalize 'lhc '#(invalid-uri "URI recomposition failed"))))))

  ((_)
   (lhc-error:normalize 'lhc '#(invalid-uri "Invalid URI map"))))

(defun uri-map-to-erlang
  "Convert our URI map format to Erlang's uri_string format."
  ((uri-map)
   (let ((m (map)))
     (lists:foldl
      (lambda (kv acc)
        (case kv
          (`#(,k ,v) (when (=/= v 'undefined) (=/= v #""))
           (maps:put k (binary_to_list (ensure-binary v)) acc))
          (`#(port ,p) (when (is_integer p))
           (maps:put 'port p acc))
          (_ acc)))
      m
      (maps:to_list uri-map)))))

(defun extract-path
  "Extract path (and query) from a URI.

  Args:
    uri: Full URI or just a path

  Returns:
    Binary path including query string if present"
  ((uri) (when (is_binary uri))
   (extract-path (binary_to_list uri)))

  ((uri) (when (is_list uri))
   (case (parse-uri uri)
     (`#(ok ,uri-map)
      (let ((path (maps:get 'path uri-map #"/"))
            (query (maps:get 'query uri-map #"")))
        (if (=:= query #"")
          path
          (list_to_binary (++ (binary_to_list path)
                              "?"
                              (binary_to_list query))))))
     (_
      ;; If parsing fails, assume it's already just a path
      (list_to_binary uri))))

  ((path)
   (ensure-binary path)))
