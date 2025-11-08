(defmodule lhc-async
  "Asynchronous HTTP operations for LHC.

  This module provides asynchronous HTTP request methods. All request
  functions return immediately with {ok, request-id}, and the response
  is sent as a message to the receiver process.

  Use await/2 to block and wait for the response, or handle the messages
  manually in your receive block.

  Message Formats:
    httpc: {http, {request-id, result}}
    gun: {gun_response, conn, stream-ref, fin, status, headers}
          {gun_data, conn, stream-ref, fin, data}
          {gun_error, conn, stream-ref, reason}"
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
   (request-with-conn 6)

   ;; Response handling
   (await 2)
   (await 3)
   (cancel 1)
   (flush 1)))

;;; ===================================================================
;;; HTTP Methods
;;; ===================================================================

(defun get
  "Perform an asynchronous HTTP GET request.

  Args:
    uri: Request URI
    opts: Options map (should include 'receiver or defaults to self())

  Returns:
    {ok, request-id}

  Example:
    (let ((`#(ok ,req-id) (lhc-async:get \"https://api.example.com/users\"
                                         (map 'receiver (self)))))
      (lhc-async:await req-id 5000))"
  ((uri opts)
   (request 'GET uri (binary) '() opts)))

(defun head
  "Perform an asynchronous HTTP HEAD request.

  Args:
    uri: Request URI
    opts: Options map

  Returns:
    {ok, request-id}"
  ((uri opts)
   (request 'HEAD uri (binary) '() opts)))

(defun post
  "Perform an asynchronous HTTP POST request.

  Args:
    uri: Request URI
    body: Request body
    opts: Options map

  Returns:
    {ok, request-id}"
  ((uri body opts)
   (request 'POST uri body '() opts)))

(defun put
  "Perform an asynchronous HTTP PUT request.

  Args:
    uri: Request URI
    body: Request body
    opts: Options map

  Returns:
    {ok, request-id}"
  ((uri body opts)
   (request 'PUT uri body '() opts)))

(defun patch
  "Perform an asynchronous HTTP PATCH request.

  Args:
    uri: Request URI
    body: Request body
    opts: Options map

  Returns:
    {ok, request-id}"
  ((uri body opts)
   (request 'PATCH uri body '() opts)))

(defun delete
  "Perform an asynchronous HTTP DELETE request.

  Args:
    uri: Request URI
    opts: Options map

  Returns:
    {ok, request-id}"
  ((uri opts)
   (request 'DELETE uri (binary) '() opts)))

(defun options
  "Perform an asynchronous HTTP OPTIONS request.

  Args:
    uri: Request URI
    opts: Options map

  Returns:
    {ok, request-id}"
  ((uri opts)
   (request 'OPTIONS uri (binary) '() opts)))

(defun trace
  "Perform an asynchronous HTTP TRACE request.

  Args:
    uri: Request URI
    opts: Options map

  Returns:
    {ok, request-id}"
  ((uri opts)
   (request 'TRACE uri (binary) '() opts)))

;;; ===================================================================
;;; Generic Request
;;; ===================================================================

(defun request
  "Perform an asynchronous HTTP request.

  Args:
    method: HTTP method atom
    uri: Request URI
    body: Request body
    headers: Request headers
    opts: Options map

  Returns:
    {ok, request-id} or {error, error-map}

  The request-id can be used with await/2 or cancel/1."
  ((method uri body headers opts)
   ;; Ensure mode is async
   (let* ((opt-headers (maps:get 'headers opts '()))
          (merged-headers (lhc-util:merge-headers opt-headers headers))
          (async-opts (maps:put 'mode 'async
                                (maps:put 'headers merged-headers opts)))
          (backend (get-backend async-opts)))
     ;; Dispatch to appropriate backend
     (case backend
       ('httpc
        (dispatch-httpc method uri body merged-headers async-opts))
       ('gun
        (dispatch-gun method uri body merged-headers async-opts))
       (_
        (lhc-error:normalize 'lhc `#(unknown-backend ,backend)))))))

(defun request-with-conn
  "Perform an asynchronous HTTP request with an existing connection.

  Args:
    conn: Connection reference
    method: HTTP method
    uri: Request URI or path
    body: Request body
    headers: Request headers
    opts: Options map

  Returns:
    {ok, request-id} or {error, error-map}"
  ((conn method uri body headers opts)
   (let* ((opt-headers (maps:get 'headers opts '()))
          (merged-headers (lhc-util:merge-headers opt-headers headers))
          (async-opts (maps:put 'mode 'async
                                (maps:put 'headers merged-headers opts)))
          (backend (determine-backend-from-conn conn async-opts)))
     (case backend
       ('httpc
        (let ((profile-opts (maps:put 'httpc-opts
                                      (maps:put 'profile conn
                                                (maps:get 'httpc-opts async-opts (map)))
                                      async-opts)))
          (dispatch-httpc method uri body merged-headers profile-opts)))
       ('gun
        (let ((path (lhc-util:extract-path uri)))
          (case (lhc-gun:do-request-async conn method path body merged-headers async-opts)
            (`#(ok ,stream-ref)
             ;; Store mapping for later use
             (let ((request-id (make_ref)))
               (store-request-info request-id backend conn stream-ref)
               `#(ok ,request-id)))
            (error error))))
       (_
        (lhc-error:normalize 'lhc `#(unknown-backend ,backend)))))))

;;; ===================================================================
;;; Backend Dispatch
;;; ===================================================================

(defun dispatch-httpc
  "Dispatch async request to httpc backend.

  Args:
    method: HTTP method
    uri: Request URI
    body: Request body
    headers: Request headers
    opts: Options map

  Returns:
    {ok, request-id} or {error, error-map}"
  ((method uri body headers opts)
   (case (lhc-httpc:do-request-async method uri body headers opts)
     (`#(ok ,request-id)
      ;; Store request info for tracking
      (store-request-info request-id 'httpc 'undefined 'undefined)
      `#(ok ,request-id))
     (error error))))

(defun dispatch-gun
  "Dispatch async request to gun backend.

  Opens a temporary connection for the request.

  Args:
    method: HTTP method
    uri: Request URI
    body: Request body
    headers: Request headers
    opts: Options map

  Returns:
    {ok, request-id} or {error, error-map}"
  ((method uri body headers opts)
   (case (lhc-gun:open-connection uri opts)
     (`#(ok ,conn)
      (let ((path (lhc-util:extract-path uri)))
        (case (lhc-gun:do-request-async conn method path body headers opts)
          (`#(ok ,stream-ref)
           (let ((request-id (make_ref)))
             ;; Store request info for tracking
             (store-request-info request-id 'gun conn stream-ref)
             `#(ok ,request-id)))
          (error
           (lhc-gun:close-connection conn)
           error))))
     (error error))))

;;; ===================================================================
;;; Response Handling
;;; ===================================================================

(defun await
  "Wait for and collect an async HTTP response.

  This blocks until the response is received or timeout occurs.

  Args:
    request-id: Request identifier returned from async request
    timeout: Timeout in milliseconds

  Returns:
    {ok, response-map} or {error, error-map}"
  ((request-id timeout)
   (await request-id timeout 'true)))

(defun await
  "Wait for async response with option to auto-close gun connections.

  Args:
    request-id: Request identifier
    timeout: Timeout in milliseconds
    auto-close: Boolean - close gun connection after receiving response

  Returns:
    {ok, response-map} or {error, error-map}"
  ((request-id timeout auto-close)
   (case (get-request-info request-id)
     (`#(ok ,backend ,conn ,stream-ref)
      (case backend
        ('httpc
         (await-httpc request-id timeout))
        ('gun
         (let ((result (await-gun conn stream-ref timeout)))
           (if (and auto-close (is_pid conn))
             (lhc-gun:close-connection conn)
             'ok)
           (clear-request-info request-id)
           result))
        (_
         (lhc-error:normalize 'lhc `#(unknown-backend ,backend)))))
     ('undefined
      (lhc-error:normalize 'lhc '#(request-not-found "Request ID not tracked"))))))

(defun await-httpc
  "Wait for httpc async response.

  Args:
    request-id: Request identifier
    timeout: Timeout in milliseconds

  Returns:
    {ok, response-map} or {error, error-map}"
  ((request-id timeout)
   (receive
     ;; Success response
     (`#(http #(,req-id ,result))
      (when (=:= req-id request-id))
      (clear-request-info request-id)
      (case result
        (`#(,_status-line ,headers ,body)
         (lhc-httpc:normalize-response result))
        (`#(error ,reason)
         (lhc-error:normalize 'httpc reason))
        (_
         (lhc-httpc:normalize-response result))))

     (after timeout
       (clear-request-info request-id)
       (lhc-error:normalize 'lhc
                           `#(timeout ,(list_to_binary
                                       (++ "Request timeout after "
                                           (integer_to_list timeout)
                                           "ms"))))))))

(defun await-gun
  "Wait for gun async response.

  Args:
    conn: Connection pid
    stream-ref: Stream reference
    timeout: Timeout in milliseconds

  Returns:
    {ok, response-map} or {error, error-map}"
  ((conn stream-ref timeout)
   (receive
     ;; Response with no body
     (`#(gun_response ,c ,s-ref fin ,status ,headers)
      (when (=:= c conn) (=:= s-ref stream-ref))
      `#(ok ,(maps:from_list
              `(#(status ,status)
                #(headers ,(lhc-gun:normalize-headers headers))
                #(body ,(binary))))))

     ;; Response with body
     (`#(gun_response ,c ,s-ref nofin ,status ,headers)
      (when (=:= c conn) (=:= s-ref stream-ref))
      (await-gun-body conn stream-ref status headers timeout (binary)))

     ;; Error
     (`#(gun_error ,c ,s-ref ,reason)
      (when (=:= c conn) (=:= s-ref stream-ref))
      (lhc-error:normalize 'gun reason))

     ;; Connection down
     (`#(gun_down ,c ,_protocol ,reason ,_killed-streams)
      (when (=:= c conn))
      (lhc-error:normalize 'gun reason))

     (after timeout
       (lhc-error:normalize 'gun 'timeout)))))

(defun await-gun-body
  "Collect gun response body data.

  Args:
    conn: Connection pid
    stream-ref: Stream reference
    status: HTTP status code
    headers: Response headers
    timeout: Timeout in milliseconds
    acc: Accumulated body data

  Returns:
    {ok, response-map} or {error, error-map}"
  ((conn stream-ref status headers timeout acc)
   (receive
     ;; Final data
     (`#(gun_data ,c ,s-ref fin ,data)
      (when (=:= c conn) (=:= s-ref stream-ref))
      `#(ok ,(maps:from_list
              `(#(status ,status)
                #(headers ,(lhc-gun:normalize-headers headers))
                #(body ,(binary (acc binary) (data binary)))))))

     ;; More data
     (`#(gun_data ,c ,s-ref nofin ,data)
      (when (=:= c conn) (=:= s-ref stream-ref))
      (await-gun-body conn stream-ref status headers timeout (binary (acc binary) (data binary))))

     ;; Trailers (end of response)
     (`#(gun_trailers ,c ,s-ref ,trailers)
      (when (=:= c conn) (=:= s-ref stream-ref))
      `#(ok ,(maps:from_list
              `(#(status ,status)
                #(headers ,(++ (lhc-gun:normalize-headers headers)
                              (lhc-gun:normalize-headers trailers)))
                #(body ,acc)))))

     ;; Error during body collection
     (`#(gun_error ,c ,s-ref ,reason)
      (when (=:= c conn) (=:= s-ref stream-ref))
      (lhc-error:normalize 'gun reason))

     (after timeout
       (lhc-error:normalize 'gun
                           `#(timeout ,(binary "Body collection timeout")))))))

;;; ===================================================================
;;; Request Management
;;; ===================================================================

(defun cancel
  "Cancel an async request.

  For httpc: Calls httpc:cancel_request
  For gun: Calls gun:cancel on the stream

  Args:
    request-id: Request identifier

  Returns:
    ok"
  ((request-id)
   (case (get-request-info request-id)
     (`#(ok httpc ,_conn ,_stream)
      (httpc:cancel_request request-id)
      (clear-request-info request-id)
      'ok)
     (`#(ok gun ,conn ,stream-ref)
      (when (is_pid conn) (is_reference stream-ref))
      (gun:cancel conn stream-ref)
      (clear-request-info request-id)
      'ok)
     (_
      'ok))))

(defun flush
  "Flush any remaining messages for a request.

  Args:
    request-id: Request identifier

  Returns:
    ok"
  ((request-id)
   (case (get-request-info request-id)
     (`#(ok httpc ,_conn ,_stream)
      (flush-httpc request-id))
     (`#(ok gun ,conn ,stream-ref)
      (flush-gun conn stream-ref))
     (_
      'ok))
   (clear-request-info request-id)
   'ok))

(defun flush-httpc
  "Flush httpc messages for a request."
  ((request-id)
   (receive
     (`#(http #(,req-id ,_result))
      (when (=:= req-id request-id))
      (flush-httpc request-id))
     (after 0 'ok))))

(defun flush-gun
  "Flush gun messages for a stream."
  ((conn stream-ref)
   (receive
     (`#(gun_response ,c ,s-ref ,_fin ,_status ,_headers)
      (when (=:= c conn) (=:= s-ref stream-ref))
      (flush-gun conn stream-ref))
     (`#(gun_data ,c ,s-ref ,_fin ,_data)
      (when (=:= c conn) (=:= s-ref stream-ref))
      (flush-gun conn stream-ref))
     (`#(gun_trailers ,c ,s-ref ,_trailers)
      (when (=:= c conn) (=:= s-ref stream-ref))
      (flush-gun conn stream-ref))
     (`#(gun_error ,c ,s-ref ,_reason)
      (when (=:= c conn) (=:= s-ref stream-ref))
      (flush-gun conn stream-ref))
     (after 0 'ok))))

;;; ===================================================================
;;; Request Tracking (using process dictionary)
;;; ===================================================================

(defun store-request-info
  "Store request information for later retrieval.

  Uses process dictionary with key: {lhc-async-request, request-id}

  Args:
    request-id: Request identifier
    backend: Backend atom
    conn: Connection reference (or undefined)
    stream-ref: Stream reference (or undefined)"
  ((request-id backend conn stream-ref)
   (erlang:put `#(lhc-async-request ,request-id)
               `#(,backend ,conn ,stream-ref))
   'ok))

(defun get-request-info
  "Retrieve request information.

  Args:
    request-id: Request identifier

  Returns:
    {ok, backend, conn, stream-ref} or undefined"
  ((request-id)
   (case (erlang:get `#(lhc-async-request ,request-id))
     (`#(,backend ,conn ,stream-ref)
      `#(ok ,backend ,conn ,stream-ref))
     ('undefined
      'undefined))))

(defun clear-request-info
  "Clear request information from tracking.

  Args:
    request-id: Request identifier

  Returns:
    ok"
  ((request-id)
   (erlang:erase `#(lhc-async-request ,request-id))
   'ok))

;;; ===================================================================
;;; Backend Selection
;;; ===================================================================

(defun get-backend
  "Get backend from options or application environment."
  ((opts)
   (case (maps:get 'backend opts 'undefined)
     ('undefined
      (case (application:get_env 'lhc 'default_backend)
        (`#(ok ,backend) backend)
        ('undefined 'httpc)))
     (backend backend))))

(defun determine-backend-from-conn
  "Determine backend type from connection reference."
  ((conn opts)
   (case (maps:get 'backend opts 'undefined)
     ('undefined
      (cond
        ((is_pid conn) 'gun)
        ((is_atom conn) 'httpc)
        ('true (get-backend opts))))
     (backend backend))))
