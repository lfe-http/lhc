(defmodule lhc-sync
  "Synchronous HTTP operations for LHC.

  This module provides synchronous HTTP request methods. All functions
  in this module block until the response is received or an error occurs.

  The backend (httpc or gun) is selected via the options map. If not
  specified, the default backend from application environment is used
  (typically httpc).

  All functions return {ok, response-map} or {error, error-map}."
  (export
   ;; HTTP methods
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
   (request-with-conn 6)))

;;; ===================================================================
;;; HTTP Methods
;;; ===================================================================

(defun get
  "Perform a synchronous HTTP GET request.

  Args:
    uri: Request URI (binary or string)
    opts: Options map

  Returns:
    {ok, response-map} or {error, error-map}

  Example:
    (lhc-sync:get \"https://api.example.com/users\"
                  (map 'timeout 5000))"
  ((uri opts)
   (request 'GET uri (binary) '() opts)))

(defun head
  "Perform a synchronous HTTP HEAD request.

  Args:
    uri: Request URI
    opts: Options map

  Returns:
    {ok, response-map} or {error, error-map}"
  ((uri opts)
   (request 'HEAD uri (binary) '() opts)))

(defun post
  "Perform a synchronous HTTP POST request.

  Args:
    uri: Request URI
    body: Request body (binary or iolist)
    opts: Options map

  Returns:
    {ok, response-map} or {error, error-map}

  Example:
    (lhc-sync:post \"https://api.example.com/users\"
                   #\"{\\\"name\\\": \\\"Alice\\\"}\"
                   (map 'headers '(#(\"content-type\" \"application/json\"))))"
  ((uri body opts)
   (request 'POST uri body '() opts)))

(defun put
  "Perform a synchronous HTTP PUT request.

  Args:
    uri: Request URI
    body: Request body
    opts: Options map

  Returns:
    {ok, response-map} or {error, error-map}"
  ((uri body opts)
   (request 'PUT uri body '() opts)))

(defun patch
  "Perform a synchronous HTTP PATCH request.

  Note: PATCH is not supported by httpc backend. Use gun backend
  for PATCH requests.

  Args:
    uri: Request URI
    body: Request body
    opts: Options map

  Returns:
    {ok, response-map} or {error, error-map}"
  ((uri body opts)
   (request 'PATCH uri body '() opts)))

(defun delete
  "Perform a synchronous HTTP DELETE request.

  Args:
    uri: Request URI
    opts: Options map

  Returns:
    {ok, response-map} or {error, error-map}"
  ((uri opts)
   (request 'DELETE uri (binary) '() opts)))

(defun options
  "Perform a synchronous HTTP OPTIONS request.

  Args:
    uri: Request URI
    opts: Options map

  Returns:
    {ok, response-map} or {error, error-map}"
  ((uri opts)
   (request 'OPTIONS uri (binary) '() opts)))

(defun trace
  "Perform a synchronous HTTP TRACE request.

  Args:
    uri: Request URI
    opts: Options map

  Returns:
    {ok, response-map} or {error, error-map}"
  ((uri opts)
   (request 'TRACE uri (binary) '() opts)))

;;; ===================================================================
;;; Generic Request
;;; ===================================================================

(defun request
  "Perform a synchronous HTTP request with full control.

  Args:
    method: HTTP method atom (GET, POST, etc.)
    uri: Request URI
    body: Request body
    headers: Additional headers (list of tuples or map)
    opts: Options map

  Returns:
    {ok, response-map} or {error, error-map}

  Options:
    backend: 'httpc or 'gun (default from app env)
    timeout: Request timeout in ms (default 5000)
    connect-timeout: Connection timeout in ms
    headers: Headers to merge with provided headers
    follow-redirects: Boolean (default true)
    max-redirects: Max redirect count (default 5)
    ssl-opts: SSL options list
    httpc-opts: Backend-specific httpc options
    gun-opts: Backend-specific gun options

  Example:
    (lhc-sync:request 'POST
                      \"https://api.example.com/data\"
                      #\"{\\\"foo\\\": \\\"bar\\\"}\"
                      '(#(\"content-type\" \"application/json\"))
                      (map 'backend 'gun
                           'timeout 10000))"
  ((method uri body headers opts)
   ;; Merge headers from parameters and options
   (let* ((opt-headers (maps:get 'headers opts '()))
          (merged-headers (lhc-util:merge-headers opt-headers headers))
          (merged-opts (maps:put 'headers merged-headers opts))
          (backend (get-backend merged-opts)))
     ;; Dispatch to appropriate backend
     (case backend
       ('httpc
        (dispatch-httpc method uri body merged-headers merged-opts))
       ('gun
        (dispatch-gun method uri body merged-headers merged-opts))
       (_
        (lhc-error:normalize 'lhc `#(unknown-backend ,backend)))))))

(defun request-with-conn
  "Perform a synchronous HTTP request using an existing connection.

  This is useful when you want to reuse a connection for multiple requests,
  especially with the gun backend.

  Args:
    conn: Connection reference (gun pid or httpc profile atom)
    method: HTTP method atom
    uri: Request URI (for gun, can be just path)
    body: Request body
    headers: Request headers
    opts: Options map

  Returns:
    {ok, response-map} or {error, error-map}

  Example:
    ;; With gun
    (let ((`#(ok ,conn) (lhc:open \"https://api.example.com\" (map 'backend 'gun))))
      (lhc-sync:request-with-conn conn 'GET \"/users\" (binary) '() (map))
      (lhc-sync:request-with-conn conn 'GET \"/posts\" (binary) '() (map))
      (lhc:close conn))"
  ((conn method uri body headers opts)
   (let* ((opt-headers (maps:get 'headers opts '()))
          (merged-headers (lhc-util:merge-headers opt-headers headers))
          (merged-opts (maps:put 'headers merged-headers opts))
          (backend (determine-backend-from-conn conn merged-opts)))
     (case backend
       ('httpc
        ;; For httpc, conn is a profile name
        (let ((profile-opts (maps:put 'httpc-opts
                                      (maps:put 'profile conn
                                                (maps:get 'httpc-opts merged-opts (map)))
                                      merged-opts)))
          (dispatch-httpc method uri body merged-headers profile-opts)))
       ('gun
        ;; For gun, conn is a pid - extract path from URI
        (let ((path (lhc-util:extract-path uri)))
          (lhc-gun:do-request-sync conn method path body merged-headers merged-opts)))
       (_
        (lhc-error:normalize 'lhc `#(unknown-backend ,backend)))))))

;;; ===================================================================
;;; Backend Dispatch
;;; ===================================================================

(defun dispatch-httpc
  "Dispatch a synchronous request to the httpc backend.

  Args:
    method: HTTP method
    uri: Request URI
    body: Request body
    headers: Request headers
    opts: Options map

  Returns:
    {ok, response-map} or {error, error-map}"
  ((method uri body headers opts)
   (lhc-httpc:do-request-sync method uri body headers opts)))

(defun dispatch-gun
  "Dispatch a synchronous request to the gun backend.

  For gun, we need to open a connection, make the request, and close it
  unless a connection is already provided.

  Args:
    method: HTTP method
    uri: Request URI
    body: Request body
    headers: Request headers
    opts: Options map

  Returns:
    {ok, response-map} or {error, error-map}"
  ((method uri body headers opts)
   ;; Open a temporary connection
   (case (lhc-gun:open-connection uri opts)
     (`#(ok ,conn)
      (let* ((path (lhc-util:extract-path uri))
             (result (lhc-gun:do-request-sync conn method path body headers opts)))
        ;; Close connection after request
        (lhc-gun:close-connection conn)
        result))
     (error error))))

;;; ===================================================================
;;; Backend Selection
;;; ===================================================================

(defun get-backend
  "Get backend from options or application environment.

  Args:
    opts: Options map

  Returns:
    Backend atom ('httpc or 'gun)"
  ((opts)
   (case (maps:get 'backend opts 'undefined)
     ('undefined
      ;; Try to get from application environment
      (case (application:get_env 'lhc 'default_backend)
        (`#(ok ,backend) backend)
        ('undefined 'httpc)))
     (backend backend))))

(defun determine-backend-from-conn
  "Determine backend type from connection reference.

  Args:
    conn: Connection reference
    opts: Options map (may override with explicit backend)

  Returns:
    Backend atom"
  ((conn opts)
   ;; If backend is explicitly set in opts, use it
   (case (maps:get 'backend opts 'undefined)
     ('undefined
      ;; Infer from connection type
      (cond
        ((is_pid conn) 'gun)
        ((is_atom conn) 'httpc)
        ('true (get-backend opts))))
     (backend backend))))
