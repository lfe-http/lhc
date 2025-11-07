(defmodule lhc
  "LFE Little HTTP Client - Main public API.

  This is the main entry point for the LHC library. It provides a unified
  interface for HTTP requests that automatically dispatches to either
  synchronous or asynchronous operations based on the options provided.

  Quick Start:
    ;; Simple GET request
    (lhc:get \"https://api.example.com/users\" (map))

    ;; POST with JSON body
    (lhc:post \"https://api.example.com/users\"
              #\"{\\\"name\\\": \\\"Alice\\\"}\"
              (map 'headers '(#(\"content-type\" \"application/json\"))))

    ;; Async request
    (let ((`#(ok ,req-id) (lhc:get \"https://api.example.com/users\"
                                   (map 'mode 'async))))
      (lhc:await req-id 5000))

    ;; Using gun backend with HTTP/2
    (lhc:get \"https://example.com\"
             (map 'backend 'gun
                  'gun-opts (map 'protocols '(http2))))"
  (export
   ;; Lifecycle
   (start 0) (start 1)
   (stop 0)

   ;; Connection management
   (open 2)
   (close 1)
   (shutdown 1)

   ;; HTTP methods (auto-dispatch sync/async)
   (get 2)
   (head 2)
   (post 3)
   (put 3)
   (patch 3)
   (delete 2)
   (options 2)
   (trace 2)

   ;; Generic request
   (request 5)
   (request-with-conn 6)

   ;; Async utilities
   (await 2)
   (await 3)
   (cancel 1)
   (flush 1)

   ;; Utility functions
   (normalize-headers 1)
   (parse-uri 1)
   (build-uri 1)

   ;; Metadata
   (version 0)
   (versions 0)))

;;; ===================================================================
;;; Lifecycle
;;; ===================================================================

(defun start ()
  "Start the LHC application with default backend.

  Returns:
    {ok, [started-apps]} or {error, reason}"
  (application:ensure_all_started 'lhc))

(defun start
  "Start the LHC application with specified backend.

  Args:
    backend: 'httpc or 'gun

  Returns:
    {ok, [started-apps]} or {error, reason}"
  ((backend)
   (application:set_env 'lhc 'default_backend backend)
   (application:ensure_all_started 'lhc)))

(defun stop ()
  "Stop the LHC application.

  Returns:
    ok or {error, reason}"
  (application:stop 'lhc))

;;; ===================================================================
;;; Connection Management
;;; ===================================================================

(defun open
  "Open a connection to a server.

  For httpc: Creates/configures a profile
  For gun: Opens a TCP/TLS connection

  Args:
    uri: URI to connect to
    opts: Options map (including backend selection)

  Returns:
    {ok, conn-ref} or {error, error-map}

  Example:
    (let ((`#(ok ,conn) (lhc:open \"https://api.example.com\"
                                  (map 'backend 'gun))))
      (lhc:get-with-conn conn \"/users\" (map))
      (lhc:close conn))"
  ((uri opts)
   (let ((backend (get-backend opts)))
     (case backend
       ('httpc
        ;; For httpc, create a profile
        (let ((profile (maps:get 'profile
                                 (maps:get 'httpc-opts opts (map))
                                 (make_ref))))
          (case (lhc-httpc:create-profile profile opts)
            (`#(ok ,prof) `#(ok ,prof))
            (error error))))
       ('gun
        ;; For gun, open a connection
        (lhc-gun:open-connection uri opts))
       (_
        (lhc-error:normalize 'lhc `#(unknown-backend ,backend)))))))

(defun close
  "Close a connection.

  Args:
    conn: Connection reference (gun pid or httpc profile)

  Returns:
    ok"
  ((conn)
   (cond
     ((is_pid conn)
      ;; Gun connection
      (lhc-gun:close-connection conn))
     ((is_atom conn)
      ;; httpc profile - no explicit close needed
      'ok)
     ('true 'ok))))

(defun shutdown
  "Shutdown a connection immediately (gun only).

  For httpc profiles, this is the same as close.

  Args:
    conn: Connection reference

  Returns:
    ok"
  ((conn)
   (if (is_pid conn)
     (lhc-gun:shutdown-connection conn)
     'ok)))

;;; ===================================================================
;;; HTTP Methods
;;; ===================================================================

(defun get
  "Perform an HTTP GET request.

  Mode is determined by options:
  - Explicit: (map 'mode 'sync) or (map 'mode 'async)
  - Implicit: async if 'receiver or 'stream-to is set

  Args:
    uri: Request URI
    opts: Options map

  Returns:
    Sync: {ok, response-map} or {error, error-map}
    Async: {ok, request-id}

  Examples:
    ;; Synchronous
    (lhc:get \"https://example.com\" (map))

    ;; Asynchronous
    (lhc:get \"https://example.com\" (map 'mode 'async))"
  ((uri opts)
   (dispatch-request 'get `(,uri ,opts))))

(defun head
  "Perform an HTTP HEAD request.

  Args:
    uri: Request URI
    opts: Options map

  Returns:
    Sync: {ok, response-map} or {error, error-map}
    Async: {ok, request-id}"
  ((uri opts)
   (dispatch-request 'head `(,uri ,opts))))

(defun post
  "Perform an HTTP POST request.

  Args:
    uri: Request URI
    body: Request body (binary or iolist)
    opts: Options map

  Returns:
    Sync: {ok, response-map} or {error, error-map}
    Async: {ok, request-id}"
  ((uri body opts)
   (dispatch-request 'post `(,uri ,body ,opts))))

(defun put
  "Perform an HTTP PUT request.

  Args:
    uri: Request URI
    body: Request body
    opts: Options map

  Returns:
    Sync: {ok, response-map} or {error, error-map}
    Async: {ok, request-id}"
  ((uri body opts)
   (dispatch-request 'put `(,uri ,body ,opts))))

(defun patch
  "Perform an HTTP PATCH request.

  Note: Requires gun backend. httpc does not support PATCH.

  Args:
    uri: Request URI
    body: Request body
    opts: Options map

  Returns:
    Sync: {ok, response-map} or {error, error-map}
    Async: {ok, request-id}"
  ((uri body opts)
   (dispatch-request 'patch `(,uri ,body ,opts))))

(defun delete
  "Perform an HTTP DELETE request.

  Args:
    uri: Request URI
    opts: Options map

  Returns:
    Sync: {ok, response-map} or {error, error-map}
    Async: {ok, request-id}"
  ((uri opts)
   (dispatch-request 'delete `(,uri ,opts))))

(defun options
  "Perform an HTTP OPTIONS request.

  Args:
    uri: Request URI
    opts: Options map

  Returns:
    Sync: {ok, response-map} or {error, error-map}
    Async: {ok, request-id}"
  ((uri opts)
   (dispatch-request 'options `(,uri ,opts))))

(defun trace
  "Perform an HTTP TRACE request.

  Args:
    uri: Request URI
    opts: Options map

  Returns:
    Sync: {ok, response-map} or {error, error-map}
    Async: {ok, request-id}"
  ((uri opts)
   (dispatch-request 'trace `(,uri ,opts))))

;;; ===================================================================
;;; Generic Request
;;; ===================================================================

(defun request
  "Perform an HTTP request with full control.

  Args:
    method: HTTP method atom (GET, POST, etc.)
    uri: Request URI
    body: Request body
    headers: Request headers (list or map)
    opts: Options map

  Returns:
    Sync: {ok, response-map} or {error, error-map}
    Async: {ok, request-id}"
  ((method uri body headers opts)
   (dispatch-request 'request `(,method ,uri ,body ,headers ,opts))))

(defun request-with-conn
  "Perform an HTTP request with an existing connection.

  Args:
    conn: Connection reference
    method: HTTP method
    uri: Request URI or path
    body: Request body
    headers: Request headers
    opts: Options map

  Returns:
    Sync: {ok, response-map} or {error, error-map}
    Async: {ok, request-id}"
  ((conn method uri body headers opts)
   (dispatch-request-with-conn conn 'request-with-conn
                                `(,conn ,method ,uri ,body ,headers ,opts))))

;;; ===================================================================
;;; Async Utilities
;;; ===================================================================

(defun await
  "Wait for an async response.

  Args:
    request-id: Request identifier from async request
    timeout: Timeout in milliseconds

  Returns:
    {ok, response-map} or {error, error-map}"
  ((request-id timeout)
   (lhc-async:await request-id timeout)))

(defun await
  "Wait for an async response with auto-close option.

  Args:
    request-id: Request identifier
    timeout: Timeout in milliseconds
    auto-close: Boolean - auto-close gun connections

  Returns:
    {ok, response-map} or {error, error-map}"
  ((request-id timeout auto-close)
   (lhc-async:await request-id timeout auto-close)))

(defun cancel
  "Cancel an async request.

  Args:
    request-id: Request identifier

  Returns:
    ok"
  ((request-id)
   (lhc-async:cancel request-id)))

(defun flush
  "Flush messages for an async request.

  Args:
    request-id: Request identifier

  Returns:
    ok"
  ((request-id)
   (lhc-async:flush request-id)))

;;; ===================================================================
;;; Utility Functions
;;; ===================================================================

(defun normalize-headers
  "Normalize headers to standard binary tuple format.

  Args:
    headers: Headers (list of tuples or map)

  Returns:
    List of {binary, binary} tuples with lowercase names"
  ((headers)
   (lhc-util:normalize-headers headers)))

(defun parse-uri
  "Parse a URI string into component map.

  Args:
    uri: URI string (binary or string)

  Returns:
    {ok, uri-map} or {error, error-map}"
  ((uri)
   (lhc-util:parse-uri uri)))

(defun build-uri
  "Build a URI string from component map.

  Args:
    uri-map: Map with scheme, host, port, path, query, fragment

  Returns:
    {ok, binary} or {error, error-map}"
  ((uri-map)
   (lhc-util:build-uri uri-map)))

;;; ===================================================================
;;; Metadata
;;; ===================================================================

(defun version ()
  "Get LHC version string.

  Returns:
    Version string"
  (lhc-vsn:version))

(defun versions ()
  "Get version information for LHC and dependencies.

  Returns:
    Map of component versions"
  (lhc-vsn:all))

;;; ===================================================================
;;; Request Dispatching
;;; ===================================================================

(defun dispatch-request
  "Dispatch a request to sync or async module based on mode.

  Args:
    method: Method name atom
    args: Argument list

  Returns:
    Result from sync or async module"
  ((method args)
   (let* ((opts (extract-opts args))
          (mode (determine-mode opts)))
     (case mode
       ('sync
        (apply 'lhc-sync method args))
       ('async
        (apply 'lhc-async method args))))))

(defun dispatch-request-with-conn
  "Dispatch a connection-based request to sync or async module.

  Args:
    conn: Connection reference
    method: Method name atom
    args: Argument list

  Returns:
    Result from sync or async module"
  ((conn method args)
   (let* ((opts (extract-opts args))
          (mode (determine-mode opts)))
     (case mode
       ('sync
        (apply 'lhc-sync method args))
       ('async
        (apply 'lhc-async method args))))))

(defun extract-opts
  "Extract options map from argument list.

  Args:
    args: Argument list

  Returns:
    Options map"
  ((args)
   (let ((last-arg (lists:last args)))
     (if (is_map last-arg)
       last-arg
       (map)))))

(defun determine-mode
  "Determine sync or async mode from options.

  Args:
    opts: Options map

  Returns:
    'sync or 'async"
  ((opts)
   ;; Explicit mode takes precedence
   (case (maps:get 'mode opts 'undefined)
     ('sync 'sync)
     ('async 'async)
     ('undefined
      ;; Infer from options
      (if (has-async-options? opts)
        'async
        'sync)))))

(defun has-async-options?
  "Check if options indicate async mode.

  Args:
    opts: Options map

  Returns:
    Boolean"
  ((opts)
   (or (maps:is_key 'receiver opts)
       (case (maps:get 'stream-to opts 'none)
         ('none 'false)
         ('self 'true)
         (`#(file ,_) 'false)
         (_ 'false)))))

(defun get-backend
  "Get backend from options or application environment.

  Args:
    opts: Options map

  Returns:
    Backend atom"
  ((opts)
   (case (maps:get 'backend opts 'undefined)
     ('undefined
      (case (application:get_env 'lhc 'default_backend)
        (`#(ok ,backend) backend)
        ('undefined 'httpc)))
     (backend backend))))
