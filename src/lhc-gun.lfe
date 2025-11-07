(defmodule lhc-gun
  "gun backend implementation for LHC.

  This module provides the gun backend for the LHC library. Gun is a
  modern HTTP/1.1, HTTP/2, and WebSocket client for Erlang/OTP with
  support for connection pooling, flow control, and advanced features.

  Features:
  - HTTP/1.1 and HTTP/2 support
  - WebSocket support
  - Connection pooling and management
  - Flow control for streaming
  - Synchronous and asynchronous requests
  - CONNECT tunneling

  Note: Gun is connection-based, unlike httpc. Connections must be
  explicitly opened and closed."
  (export
   ;; Connection management
   (open-connection 2)
   (close-connection 1)
   (shutdown-connection 1)
   (await-up 2)

   ;; Request execution
   (do-request 6)
   (do-request-sync 6)
   (do-request-async 6)

   ;; Response handling
   (await-response 3)
   (collect-body 4)

   ;; Streaming
   (stream-body 4)
   (update-flow 3)

   ;; Connection info
   (connection-info 1)
   (stream-info 2)
   (ping 1)

   ;; Normalization
   (normalize-response 1)
   (normalize-headers 1)
   (make-gun-options 2)

   ;; Utilities
   (get-port 1)
   (get-transport 1)))

;;; ===================================================================
;;; Connection Management
;;; ===================================================================

(defun open-connection
  "Open a gun connection to a server.

  Args:
    uri: URI string or binary
    opts: LHC options map

  Returns:
    {ok, conn-pid} or {error, error-map}"
  ((uri opts)
   (case (lhc-util:parse-uri uri)
     (`#(ok ,uri-map)
      (let* ((host (binary_to_list (maps:get 'host uri-map)))
             (port (get-port uri-map))
             (gun-opts (make-gun-options uri-map opts)))
        (case (gun:open host port gun-opts)
          (`#(ok ,conn-pid)
           ;; Wait for connection to be established
           (await-up conn-pid opts))
          (`#(error ,reason)
           (lhc-error:normalize 'gun reason)))))
     (error error))))

(defun await-up
  "Wait for gun connection to be ready.

  Args:
    conn: Connection pid
    opts: Options map

  Returns:
    {ok, conn-pid} or {error, error-map}"
  ((conn opts)
   (let ((timeout (maps:get 'connect-timeout opts 5000)))
     (case (gun:await_up conn timeout)
       (`#(ok ,_protocol)
        `#(ok ,conn))
       (`#(error ,reason)
        (gun:close conn)
        (lhc-error:normalize 'gun reason))))))

(defun close-connection
  "Close a gun connection gracefully.

  Args:
    conn: Connection pid

  Returns:
    ok"
  ((conn)
   (gun:close conn)
   'ok))

(defun shutdown-connection
  "Shutdown a gun connection immediately.

  Args:
    conn: Connection pid

  Returns:
    ok"
  ((conn)
   (gun:shutdown conn)
   'ok))

;;; ===================================================================
;;; Request Execution
;;; ===================================================================

(defun do-request
  "Execute an HTTP request using an existing connection.

  Args:
    conn: Connection pid
    method: HTTP method atom
    path: Request path (binary or string)
    body: Request body
    headers: Request headers
    opts: Request options map

  Returns:
    Sync: {ok, response-map} or {error, error-map}
    Async: {ok, stream-ref}"
  ((conn method path body headers opts)
   (case (maps:get 'mode opts 'sync)
     ('sync (do-request-sync conn method path body headers opts))
     ('async (do-request-async conn method path body headers opts)))))

(defun do-request-sync
  "Execute a synchronous HTTP request.

  Args:
    conn: Connection pid
    method: HTTP method atom
    path: Request path
    body: Request body
    headers: Request headers
    opts: Request options map

  Returns:
    {ok, response-map} or {error, error-map}"
  ((conn method path body headers opts)
   (let* ((gun-method (method->gun method))
          (gun-headers (headers-to-gun headers))
          (gun-path (lhc-util:extract-path path))
          (timeout (maps:get 'timeout opts 5000)))
     (if (has-body? method)
       ;; Request with body
       (let ((stream-ref (gun:request conn gun-method gun-path gun-headers body)))
         (await-response conn stream-ref timeout))
       ;; Request without body
       (let ((stream-ref (gun:request conn gun-method gun-path gun-headers)))
         (await-response conn stream-ref timeout))))))

(defun do-request-async
  "Execute an asynchronous HTTP request.

  Args:
    conn: Connection pid
    method: HTTP method atom
    path: Request path
    body: Request body
    headers: Request headers
    opts: Request options map

  Returns:
    {ok, stream-ref}"
  ((conn method path body headers opts)
   (let* ((gun-method (method->gun method))
          (gun-headers (headers-to-gun headers))
          (gun-path (lhc-util:extract-path path)))
     (if (has-body? method)
       ;; Request with body
       (let ((stream-ref (gun:request conn gun-method gun-path gun-headers body)))
         `#(ok ,stream-ref))
       ;; Request without body
       (let ((stream-ref (gun:request conn gun-method gun-path gun-headers)))
         `#(ok ,stream-ref))))))

(defun has-body?
  "Check if an HTTP method typically has a body."
  (('GET) 'false)
  (('HEAD) 'false)
  (('DELETE) 'false)
  (('OPTIONS) 'false)
  (('TRACE) 'false)
  ((_) 'true))

;;; ===================================================================
;;; Response Handling
;;; ===================================================================

(defun await-response
  "Wait for and collect a complete HTTP response.

  Args:
    conn: Connection pid
    stream-ref: Stream reference
    timeout: Timeout in milliseconds

  Returns:
    {ok, response-map} or {error, error-map}"
  ((conn stream-ref timeout)
   (case (gun:await conn stream-ref timeout)
     ;; Response with no body (fin)
     (`#(response fin ,status ,headers)
      `#(ok ,(maps:from_list
              `(#(status ,status)
                #(headers ,(normalize-headers headers))
                #(body ,(binary))))))

     ;; Response with body (nofin)
     (`#(response nofin ,status ,headers)
      (case (gun:await_body conn stream-ref timeout)
        (`#(ok ,body)
         `#(ok ,(maps:from_list
                 `(#(status ,status)
                   #(headers ,(normalize-headers headers))
                   #(body ,body)))))
        (`#(error ,reason)
         (lhc-error:normalize 'gun reason))))

     ;; Error
     (`#(error ,reason)
      (lhc-error:normalize 'gun reason)))))

(defun collect-body
  "Collect body data from gun messages (for manual async handling).

  Args:
    conn: Connection pid
    stream-ref: Stream reference
    timeout: Timeout in milliseconds
    acc: Accumulator binary

  Returns:
    {ok, body-binary} or {error, error-map}"
  ((conn stream-ref timeout acc)
   (receive
     ;; Final data
     (`#(gun_data ,conn-pid ,s-ref fin ,data)
      (when (=:= conn-pid conn) (=:= s-ref stream-ref))
      `#(ok ,(binary acc data)))

     ;; More data coming
     (`#(gun_data ,conn-pid ,s-ref nofin ,data)
      (when (=:= conn-pid conn) (=:= s-ref stream-ref))
      (collect-body conn stream-ref timeout (binary acc data)))

     ;; Trailers (end of response)
     (`#(gun_trailers ,conn-pid ,s-ref ,_trailers)
      (when (=:= conn-pid conn) (=:= s-ref stream-ref))
      `#(ok ,acc))

     ;; Error
     (`#(gun_error ,conn-pid ,s-ref ,reason)
      (when (=:= conn-pid conn) (=:= s-ref stream-ref))
      (lhc-error:normalize 'gun reason))

     (after timeout
       (lhc-error:normalize 'gun 'timeout)))))

;;; ===================================================================
;;; Streaming Support
;;; ===================================================================

(defun stream-body
  "Stream body data on an existing request.

  Args:
    conn: Connection pid
    stream-ref: Stream reference
    data: Data to send (binary or iolist)
    opts: Options (fin: boolean)

  Returns:
    ok"
  ((conn stream-ref data opts)
   (let ((fin (maps:get 'fin opts 'nofin)))
     (gun:data conn stream-ref fin data)
     'ok)))

(defun update-flow
  "Update flow control for a stream.

  Args:
    conn: Connection pid
    stream-ref: Stream reference
    increment: Number of data messages to receive

  Returns:
    ok"
  ((conn stream-ref increment)
   (gun:update_flow conn stream-ref increment)
   'ok))

;;; ===================================================================
;;; Connection Information
;;; ===================================================================

(defun connection-info
  "Get information about a gun connection.

  Args:
    conn: Connection pid

  Returns:
    {ok, info-map} or {error, error-map}"
  ((conn)
   (case (gun:info conn)
     ('undefined
      (lhc-error:normalize 'gun '#(connection_closed "Connection not found")))
     (info
      `#(ok ,info)))))

(defun stream-info
  "Get information about a stream.

  Args:
    conn: Connection pid
    stream-ref: Stream reference

  Returns:
    {ok, info-map} or undefined"
  ((conn stream-ref)
   (case (gun:stream_info conn stream-ref)
     ('undefined 'undefined)
     (info `#(ok ,info)))))

(defun ping
  "Send a ping on the connection.

  Args:
    conn: Connection pid

  Returns:
    {ok, reference}"
  ((conn)
   (let ((ref (gun:ping conn)))
     `#(ok ,ref))))

;;; ===================================================================
;;; Options Conversion
;;; ===================================================================

(defun make-gun-options
  "Convert LHC options to gun options.

  Args:
    uri-map: Parsed URI map
    opts: LHC options map

  Returns:
    gun options map"
  ((uri-map opts)
   (let* ((gun-opts (maps:get 'gun-opts opts (map)))
          (transport (get-transport uri-map))
          (protocols (maps:get 'protocols gun-opts
                               (get-default-protocols transport)))
          (retry (maps:get 'retry gun-opts 5))
          (retry-timeout (maps:get 'retry-timeout gun-opts 5000))
          (connect-timeout (maps:get 'connect-timeout opts 5000)))
     (maps:merge
      (maps:from_list
       `(#(transport ,transport)
         #(protocols ,protocols)
         #(retry ,retry)
         #(retry_timeout ,retry-timeout)
         #(connect_timeout ,connect-timeout)))
      (get-protocol-options gun-opts)))))

(defun get-transport
  "Determine transport from URI map.

  Args:
    uri-map: Parsed URI map

  Returns:
    'tcp or 'tls"
  ((uri-map)
   (case (maps:get 'scheme uri-map)
     (#"https" 'tls)
     (#"wss" 'tls)
     (_ 'tcp))))

(defun get-default-protocols
  "Get default protocols for transport.

  Args:
    transport: 'tcp or 'tls

  Returns:
    List of protocol atoms"
  (('tcp) '(http))
  (('tls) '(http2 http)))

(defun get-protocol-options
  "Extract and convert protocol-specific options.

  Args:
    gun-opts: Gun options from LHC options

  Returns:
    Map with http_opts, http2_opts, ws_opts"
  ((gun-opts)
   (let ((http-opts (maps:get 'http-opts gun-opts (map)))
         (http2-opts (maps:get 'http2-opts gun-opts (map)))
         (ws-opts (maps:get 'ws-opts gun-opts (map))))
     (maps:from_list
      (lists:flatten
       `(,(if (> (map_size http-opts) 0)
            `(#(http_opts ,http-opts))
            '())
         ,(if (> (map_size http2-opts) 0)
            `(#(http2_opts ,http2-opts))
            '())
         ,(if (> (map_size ws-opts) 0)
            `(#(ws_opts ,ws-opts))
            '())))))))

(defun get-port
  "Get port from URI map or use default based on scheme.

  Args:
    uri-map: Parsed URI map

  Returns:
    Integer port number"
  ((uri-map)
   (case (maps:get 'port uri-map 'undefined)
     ('undefined
      (case (maps:get 'scheme uri-map)
        (#"https" 443)
        (#"wss" 443)
        (_ 80)))
     (port port))))

;;; ===================================================================
;;; Response Normalization
;;; ===================================================================

(defun normalize-response
  "Normalize gun response to LHC format.

  Note: This is for response maps, not gun messages.
  Use await-response for message handling.

  Args:
    response: Response data

  Returns:
    {ok, response-map}"
  ((response) (when (is_map response))
   `#(ok ,response))
  ((response)
   `#(ok ,(maps:from_list `(#(data ,response))))))

(defun normalize-headers
  "Convert gun headers to LHC format.

  Gun headers are already binary tuples, but we ensure lowercase names.

  Args:
    headers: List of {binary, binary} tuples

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
;;; Header Conversion
;;; ===================================================================

(defun headers-to-gun
  "Convert LHC headers to gun format.

  Gun expects binary tuples {binary, binary}.

  Args:
    headers: LHC headers (list of tuples or map)

  Returns:
    List of {binary, binary} tuples"
  ((headers)
   (lhc-util:normalize-headers headers)))

;;; ===================================================================
;;; Method Conversion
;;; ===================================================================

(defun method->gun
  "Convert HTTP method atom to gun format.

  Gun uses uppercase binary method names.

  Args:
    method: LHC method atom (e.g., 'GET)

  Returns:
    Binary method name (e.g., <<\"GET\">>)"
  (('GET) #"GET")
  (('HEAD) #"HEAD")
  (('POST) #"POST")
  (('PUT) #"PUT")
  (('PATCH) #"PATCH")
  (('DELETE) #"DELETE")
  (('OPTIONS) #"OPTIONS")
  (('CONNECT) #"CONNECT")
  (('TRACE) #"TRACE")
  ((method) (when (is_atom method))
   (atom_to_binary method 'utf8))
  ((method) (when (is_binary method))
   method))
