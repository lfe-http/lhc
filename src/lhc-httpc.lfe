(defmodule lhc-httpc
  "httpc backend implementation for LHC.

  This module provides the httpc backend for the LHC library. It wraps
  Erlang's built-in httpc client and normalizes its responses to the
  standard LHC format.

  Features:
  - Synchronous and asynchronous HTTP requests
  - Profile management for connection pooling
  - Options conversion from LHC format to httpc format
  - Response and error normalization

  Limitations:
  - No PATCH method support (httpc limitation)
  - No WebSocket support
  - Limited HTTP/2 support"
  (export
   ;; Core request execution
   (do-request 5)
   (do-request-sync 5)
   (do-request-async 5)

   ;; Profile management
   (create-profile 2)
   (set-profile-options 2)
   (get-profile-options 1)

   ;; Response normalization
   (normalize-response 1)
   (normalize-headers 1)

   ;; Options conversion
   (make-http-options 1)
   (make-httpc-options 1)
   (make-request 3)))

;;; ===================================================================
;;; Core Request Execution
;;; ===================================================================

(defun do-request
  "Execute an HTTP request, dispatching to sync or async based on mode.

  Args:
    method: HTTP method atom (GET, POST, etc.)
    uri: Request URI (binary or string)
    body: Request body (binary or iolist)
    headers: Request headers (list of tuples)
    opts: Request options map

  Returns:
    Sync mode: {ok, response-map} or {error, error-map}
    Async mode: {ok, request-id}"
  ((method uri body headers opts)
   (case (maps:get 'mode opts 'sync)
     ('sync (do-request-sync method uri body headers opts))
     ('async (do-request-async method uri body headers opts)))))

(defun do-request-sync
  "Execute a synchronous HTTP request.

  Args:
    method: HTTP method atom
    uri: Request URI
    body: Request body
    headers: Request headers
    opts: Request options map

  Returns:
    {ok, response-map} or {error, error-map}"
  ((method uri body headers opts)
   ;; Check for unsupported methods
   (case method
     ('PATCH
      (lhc-error:normalize 'httpc 'patch_not_supported))
     (_
      (let* ((request (make-request uri body headers))
             (http-opts (make-http-options opts))
             (httpc-opts (make-httpc-options opts))
             (httpc-method (method->httpc method))
             (profile (get-profile opts)))
        (case (httpc:request httpc-method request http-opts httpc-opts profile)
          (`#(ok ,response)
           (normalize-response response))
          (`#(error ,reason)
           (lhc-error:normalize 'httpc reason))))))))

(defun do-request-async
  "Execute an asynchronous HTTP request.

  Args:
    method: HTTP method atom
    uri: Request URI
    body: Request body
    headers: Request headers
    opts: Request options map

  Returns:
    {ok, request-id} or {error, error-map}"
  ((method uri body headers opts)
   ;; Check for unsupported methods
   (case method
     ('PATCH
      (lhc-error:normalize 'httpc 'patch_not_supported))
     (_
      (let* ((receiver (maps:get 'receiver opts (self)))
             (request (make-request uri body headers))
             (http-opts (make-http-options opts))
             (httpc-opts (make-httpc-options
                          (maps:put 'receiver receiver
                                    (maps:put 'sync 'false opts))))
             (httpc-method (method->httpc method))
             (profile (get-profile opts)))
        (case (httpc:request httpc-method request http-opts httpc-opts profile)
          (`#(ok ,request-id)
           `#(ok ,request-id))
          (`#(error ,reason)
           (lhc-error:normalize 'httpc reason))))))))

;;; ===================================================================
;;; Request Construction
;;; ===================================================================

(defun make-request
  "Build an httpc request tuple from components.

  Args:
    uri: Request URI (binary or string)
    body: Request body (binary or iolist)
    headers: List of header tuples

  Returns:
    httpc request tuple: {url, headers, content-type, body} or {url, headers}"
  ((uri body headers)
   (let* ((url (binary_to_list (lhc-util:ensure-binary uri)))
          (norm-headers (headers-to-httpc headers)))
     (if (or (is_binary body) (is_list body))
       ;; Request with body
       (let* ((content-type (get-content-type headers))
              (body-str (body-to-httpc body)))
         `#(,url ,norm-headers ,content-type ,body-str))
       ;; Request without body (GET, HEAD, etc.)
       `#(,url ,norm-headers)))))

(defun headers-to-httpc
  "Convert LHC headers to httpc format (list of string tuples)."
  ((headers)
   (lists:map
    (lambda (header)
      (case header
        (`#(,name ,value)
         `#(,(binary_to_list (lhc-util:ensure-binary name))
            ,(binary_to_list (lhc-util:ensure-binary value))))
        (_ header)))
    headers)))

(defun get-content-type
  "Extract Content-Type from headers, default to application/octet-stream."
  ((headers)
   (case (lhc-util:header-value headers "content-type")
     ('undefined "application/octet-stream")
     (ct (binary_to_list ct)))))

(defun body-to-httpc
  "Convert body to httpc format (string or binary)."
  ((body) (when (is_binary body))
   (binary_to_list body))
  ((body) (when (is_list body))
   ;; Could be iolist or already a string
   (try
     (iolist_to_binary body)
     (catch
       ((tuple _ _ _) body)))))

;;; ===================================================================
;;; HTTP Options Conversion
;;; ===================================================================

(defun make-http-options
  "Convert LHC options to httpc http_options().

  Args:
    opts: LHC options map

  Returns:
    httpc http_options() proplist"
  ((opts)
   (let* ((httpc-opts (maps:get 'httpc-opts opts (map)))
          (timeout (maps:get 'timeout opts 5000))
          (connect-timeout (maps:get 'connect-timeout opts timeout))
          (ssl-opts (maps:get 'ssl-opts opts '()))
          (autoredirect (maps:get 'autoredirect httpc-opts
                                   (maps:get 'follow-redirects opts 'true)))
          (max-redirect (maps:get 'max-redirect httpc-opts
                                  (maps:get 'max-redirects opts 5))))
     (lists:flatten
      `(#(timeout ,timeout)
        #(connect_timeout ,connect-timeout)
        ,(if (> (length ssl-opts) 0)
           `(#(ssl ,ssl-opts))
           '())
        #(autoredirect ,autoredirect)
        #(max_redirect ,max-redirect))))))

(defun make-httpc-options
  "Convert LHC options to httpc options().

  Args:
    opts: LHC options map

  Returns:
    httpc options() proplist"
  ((opts)
   (let* ((httpc-opts (maps:get 'httpc-opts opts (map)))
          (stream-to (maps:get 'stream-to opts 'none))
          (sync (maps:get 'sync opts 'true))
          (receiver (maps:get 'receiver opts (self))))
     (lists:flatten
      `(#(body_format binary)
        ,(build-stream-opt stream-to)
        ,(if (=:= sync 'false)
           `(#(sync false) #(receiver ,receiver))
           '()))))))

(defun build-stream-opt
  "Build the stream option for httpc."
  (('none) '())
  ((`#(file ,path))
   `(#(stream ,(binary_to_list (lhc-util:ensure-binary path)))))
  (('self)
   `(#(stream self)))
  ((_)
   '()))

;;; ===================================================================
;;; Profile Management
;;; ===================================================================

(defun create-profile
  "Create or configure an httpc profile.

  Args:
    profile: Profile name atom
    opts: LHC options map

  Returns:
    ok or {error, error-map}"
  ((profile opts)
   (let ((profile-opts (make-profile-options opts)))
     (case (httpc:set_options profile-opts profile)
       ('ok `#(ok ,profile))
       (`#(error ,reason)
        (lhc-error:normalize 'httpc reason))))))

(defun set-profile-options
  "Set options for an httpc profile.

  Args:
    profile: Profile name atom
    opts: Options to set

  Returns:
    ok or {error, error-map}"
  ((profile opts)
   (case (httpc:set_options opts profile)
     ('ok 'ok)
     (`#(error ,reason)
      (lhc-error:normalize 'httpc reason)))))

(defun get-profile-options
  "Get current options for an httpc profile.

  Args:
    profile: Profile name atom

  Returns:
    {ok, options} or {error, error-map}"
  ((profile)
   (case (httpc:get_options '(all) profile)
     (`#(ok ,opts)
      `#(ok ,opts))
     (`#(error ,reason)
      (lhc-error:normalize 'httpc reason)))))

(defun make-profile-options
  "Convert LHC options to httpc profile options."
  ((opts)
   (let ((httpc-opts (maps:get 'httpc-opts opts (map))))
     (lists:filtermap
      (lambda (opt-spec)
        (case opt-spec
          (`#(,key ,default)
           (case (maps:get key httpc-opts 'undefined)
             ('undefined
              (if (=/= default 'undefined)
                `#(true #(,key ,default))
                'false))
             (value
              `#(true #(,key ,value)))))))
      `(#(max_sessions 10)
        #(max_keep_alive_length 5)
        #(keep_alive_timeout 120000)
        #(max_pipeline_length 2)
        #(pipeline_timeout 0))))))

(defun get-profile
  "Extract profile from options or use default."
  ((opts)
   (let ((httpc-opts (maps:get 'httpc-opts opts (map))))
     (maps:get 'profile httpc-opts 'default))))

;;; ===================================================================
;;; Response Normalization
;;; ===================================================================

(defun normalize-response
  "Normalize httpc response to LHC format.

  Args:
    response: httpc response tuple

  Returns:
    {ok, response-map} or {error, error-map}"
  ;; Full response with status, headers, and body
  ((`#(#(,_version ,status ,_reason) ,headers ,body))
   `#(ok ,(maps:from_list
           `(#(status ,status)
             #(headers ,(normalize-headers headers))
             #(body ,body)))))

  ;; Saved to file response
  ((`#(#(,_version ,status ,_reason) ,headers))
   `#(ok ,(maps:from_list
           `(#(status ,status)
             #(headers ,(normalize-headers headers))
             #(body ,(binary))))))

  ;; Request ID (shouldn't happen in sync, but handle it)
  ((request-id) (when (is_reference request-id))
   `#(ok ,request-id))

  ;; Unknown response format
  ((other)
   (lhc-error:normalize 'httpc `#(unknown-response ,other))))

(defun normalize-headers
  "Convert httpc headers to LHC format (binary tuples with lowercase names).

  Args:
    headers: httpc headers (list of string tuples)

  Returns:
    List of {binary, binary} tuples with lowercase names"
  ((headers)
   (lists:map
    (lambda (header)
      (case header
        (`#(,name ,value)
         `#(,(lhc-util:lowercase (lhc-util:ensure-binary name))
            ,(lhc-util:ensure-binary value)))
        (_ header)))
    headers)))

;;; ===================================================================
;;; Method Conversion
;;; ===================================================================

(defun method->httpc
  "Convert HTTP method atom to httpc method atom.

  Args:
    method: LHC HTTP method (uppercase atom)

  Returns:
    httpc method (lowercase atom)"
  (('GET) 'get)
  (('HEAD) 'head)
  (('POST) 'post)
  (('PUT) 'put)
  (('DELETE) 'delete)
  (('TRACE) 'trace)
  (('OPTIONS) 'options)
  (('CONNECT) 'connect)
  ;; PATCH is not supported by httpc
  (('PATCH) 'patch)
  ;; Fallback to lowercase for any other method
  ((method) (when (is_atom method))
   (list_to_atom (string:lowercase (atom_to_list method)))))
