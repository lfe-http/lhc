(defmodule lhc-websocket
  "WebSocket support for LHC (gun backend only).

  This module provides WebSocket functionality using the gun HTTP client.
  WebSocket support is only available with the gun backend - httpc does
  not support WebSocket connections.

  WebSocket Lifecycle:
    1. Open gun connection to WebSocket endpoint
    2. Upgrade connection to WebSocket protocol
    3. Send and receive frames
    4. Close connection

  Frame Types:
    - text: Text data frame
    - binary: Binary data frame
    - ping: Ping control frame
    - pong: Pong control frame
    - close: Close connection frame

  Usage:
    ;; Open connection and upgrade to WebSocket
    (let ((`#(ok ,conn) (lhc:open \"wss://echo.websocket.org\"
                                  (map 'backend 'gun))))
      ;; Upgrade to WebSocket
      (let ((`#(ok ,ws) (lhc-websocket:upgrade conn \"/\" (map))))
        ;; Send text frame
        (lhc-websocket:send conn ws (map 'type 'text 'data #\"Hello!\"))
        ;; Receive echo
        (receive
          (`#(gun_ws ,c ,w ,frame)
           (when (=:= c conn) (=:= w ws))
           (io:format \"Received: ~p~n\" `(,frame))))
        ;; Close
        (lhc-websocket:close conn ws (map))))"
  (export
   ;; WebSocket lifecycle
   (upgrade 3)
   (upgrade 4)
   (close 3)

   ;; Frame operations
   (send 3)
   (send-text 3)
   (send-binary 3)
   (ping 2)
   (pong 2)

   ;; Frame handling
   (await-upgrade 3)
   (await-frame 3)
   (normalize-frame 1)

   ;; Frame construction
   (text-frame 1)
   (binary-frame 1)
   (close-frame 0)
   (close-frame 2)
   (ping-frame 0)
   (pong-frame 0)))

;;; ===================================================================
;;; WebSocket Lifecycle
;;; ===================================================================

(defun upgrade
  "Upgrade a gun connection to WebSocket.

  Args:
    conn: Gun connection pid
    path: WebSocket path (e.g., \"/\")
    opts: Options map

  Returns:
    Sync mode: {ok, stream-ref} or {error, error-map}
    Async mode: {ok, stream-ref}

  The stream-ref is used to identify this WebSocket stream for
  sending and receiving frames."
  ((conn path opts)
   (upgrade conn path '() opts)))

(defun upgrade
  "Upgrade a gun connection to WebSocket with custom headers.

  Args:
    conn: Gun connection pid
    path: WebSocket path
    headers: Additional headers for upgrade request
    opts: Options map
      - mode: 'sync or 'async (default: sync)
      - protocols: List of WebSocket subprotocols
      - extensions: WebSocket extensions
      - timeout: Upgrade timeout in ms (sync mode only)

  Returns:
    Sync mode: {ok, stream-ref} or {error, error-map}
    Async mode: {ok, stream-ref}

  In sync mode, waits for gun_upgrade message.
  In async mode, returns immediately - caller must handle gun_upgrade."
  ((conn path headers opts)
   (let* ((gun-headers (lhc-util:normalize-headers headers))
          (ws-opts (build-ws-opts opts))
          (stream-ref (gun:ws_upgrade conn (binary_to_list (lhc-util:ensure-binary path))
                                      gun-headers ws-opts))
          (mode (maps:get 'mode opts 'sync)))
     (case mode
       ('sync
        (await-upgrade conn stream-ref opts))
       ('async
        `#(ok ,stream-ref))))))

(defun await-upgrade
  "Wait for WebSocket upgrade to complete.

  Args:
    conn: Gun connection pid
    stream-ref: Stream reference from ws_upgrade
    opts: Options map (timeout key)

  Returns:
    {ok, stream-ref} or {error, error-map}"
  ((conn stream-ref opts)
   (let ((timeout (maps:get 'timeout opts 5000)))
     (receive
       ;; Upgrade successful
       (`#(gun_upgrade ,c ,s-ref ,protocols ,_headers)
        (when (=:= c conn) (=:= s-ref stream-ref))
        `#(ok ,stream-ref))

       ;; Upgrade failed
       (`#(gun_response ,c ,s-ref ,_fin ,status ,_headers)
        (when (=:= c conn) (=:= s-ref stream-ref))
        (lhc-error:normalize 'gun `#(ws-upgrade-failed ,status)))

       ;; Connection error
       (`#(gun_error ,c ,s-ref ,reason)
        (when (=:= c conn) (=:= s-ref stream-ref))
        (lhc-error:normalize 'gun reason))

       (after timeout
         (lhc-error:normalize 'gun `#(timeout "WebSocket upgrade timeout")))))))

(defun close
  "Close a WebSocket connection.

  Args:
    conn: Gun connection pid
    stream-ref: WebSocket stream reference
    opts: Options map
      - code: Close code (default: 1000 normal closure)
      - reason: Close reason string

  Returns:
    ok"
  ((conn stream-ref opts)
   (let* ((code (maps:get 'code opts 1000))
          (reason (maps:get 'reason opts (binary)))
          (close-frame (if (> (byte_size reason) 0)
                         `#(close ,code ,reason)
                         'close)))
     (gun:ws_send conn stream-ref close-frame)
     'ok)))

;;; ===================================================================
;;; Frame Operations
;;; ===================================================================

(defun send
  "Send a WebSocket frame.

  Args:
    conn: Gun connection pid
    stream-ref: WebSocket stream reference
    frame: Frame map with keys:
      - type: 'text, 'binary, 'ping, 'pong, or 'close
      - data: Frame data (for text/binary)
      - code: Close code (for close frames)
      - reason: Close reason (for close frames)

  Returns:
    ok

  Examples:
    ;; Text frame
    (lhc-websocket:send conn ws (map 'type 'text 'data #\"Hello\"))

    ;; Binary frame
    (lhc-websocket:send conn ws (map 'type 'binary 'data (binary 1 2 3)))

    ;; Close with reason
    (lhc-websocket:send conn ws (map 'type 'close 'code 1000 'reason #\"Goodbye\"))"
  ((conn stream-ref frame)
   (let ((gun-frame (frame-to-gun frame)))
     (gun:ws_send conn stream-ref gun-frame)
     'ok)))

(defun send-text
  "Send a text frame.

  Args:
    conn: Gun connection pid
    stream-ref: WebSocket stream reference
    text: Text data (binary or string)

  Returns:
    ok"
  ((conn stream-ref text)
   (send conn stream-ref (text-frame text))))

(defun send-binary
  "Send a binary frame.

  Args:
    conn: Gun connection pid
    stream-ref: WebSocket stream reference
    data: Binary data

  Returns:
    ok"
  ((conn stream-ref data)
   (send conn stream-ref (binary-frame data))))

(defun ping
  "Send a ping frame.

  Args:
    conn: Gun connection pid
    stream-ref: WebSocket stream reference

  Returns:
    ok"
  ((conn stream-ref)
   (send conn stream-ref (ping-frame))))

(defun pong
  "Send a pong frame.

  Args:
    conn: Gun connection pid
    stream-ref: WebSocket stream reference

  Returns:
    ok"
  ((conn stream-ref)
   (send conn stream-ref (pong-frame))))

;;; ===================================================================
;;; Frame Handling
;;; ===================================================================

(defun await-frame
  "Wait for a WebSocket frame.

  Args:
    conn: Gun connection pid
    stream-ref: WebSocket stream reference
    timeout: Timeout in milliseconds

  Returns:
    {ok, frame-map} or {error, error-map}

  The frame-map contains:
    - type: Frame type atom
    - data: Frame data (for text/binary)
    - code: Close code (for close frames)
    - reason: Close reason (for close frames)"
  ((conn stream-ref timeout)
   (receive
     ;; WebSocket frame
     (`#(gun_ws ,c ,s-ref ,frame)
      (when (=:= c conn) (=:= s-ref stream-ref))
      `#(ok ,(normalize-frame frame)))

     ;; Connection closed
     (`#(gun_down ,c ,_protocol ,reason ,_killed)
      (when (=:= c conn))
      (lhc-error:normalize 'gun reason))

     ;; Error
     (`#(gun_error ,c ,s-ref ,reason)
      (when (=:= c conn) (=:= s-ref stream-ref))
      (lhc-error:normalize 'gun reason))

     (after timeout
       (lhc-error:normalize 'gun 'timeout)))))

(defun normalize-frame
  "Normalize gun WebSocket frame to LHC format.

  Args:
    frame: Gun WebSocket frame

  Returns:
    Frame map with standardized keys"
  ;; Text frame
  ((`#(text ,data))
   (maps:from_list `(#(type text) #(data ,data))))

  ;; Binary frame
  ((`#(binary ,data))
   (maps:from_list `(#(type binary) #(data ,data))))

  ;; Ping frame
  (('ping)
   (maps:from_list `(#(type ping))))

  ;; Pong frame
  (('pong)
   (maps:from_list `(#(type pong))))

  ;; Close frame without code/reason
  (('close)
   (maps:from_list `(#(type close) #(code 1000) #(reason ,(binary)))))

  ;; Close frame with code and reason
  ((`#(close ,code ,reason))
   (maps:from_list `(#(type close) #(code ,code) #(reason ,reason))))

  ;; Unknown frame type
  ((frame)
   (maps:from_list `(#(type unknown) #(data ,frame)))))

;;; ===================================================================
;;; Frame Construction
;;; ===================================================================

(defun text-frame
  "Create a text frame map.

  Args:
    text: Text data (binary or string)

  Returns:
    Frame map"
  ((text)
   (maps:from_list `(#(type text) #(data ,(lhc-util:ensure-binary text))))))

(defun binary-frame
  "Create a binary frame map.

  Args:
    data: Binary data

  Returns:
    Frame map"
  ((data)
   (maps:from_list `(#(type binary) #(data ,data)))))

(defun close-frame ()
  "Create a close frame map with default code.

  Returns:
    Frame map"
  (maps:from_list `(#(type close) #(code 1000) #(reason ,(binary)))))

(defun close-frame
  "Create a close frame map with code and reason.

  Args:
    code: Close code integer
    reason: Close reason (binary or string)

  Returns:
    Frame map"
  ((code reason)
   (maps:from_list `(#(type close)
                     #(code ,code)
                     #(reason ,(lhc-util:ensure-binary reason))))))

(defun ping-frame ()
  "Create a ping frame map.

  Returns:
    Frame map"
  (maps:from_list `(#(type ping))))

(defun pong-frame ()
  "Create a pong frame map.

  Returns:
    Frame map"
  (maps:from_list `(#(type pong))))

;;; ===================================================================
;;; Helper Functions
;;; ===================================================================

(defun frame-to-gun
  "Convert LHC frame map to gun frame format.

  Args:
    frame: LHC frame map

  Returns:
    Gun frame tuple or atom"
  ((frame)
   (case (maps:get 'type frame)
     ('text
      `#(text ,(maps:get 'data frame)))

     ('binary
      `#(binary ,(maps:get 'data frame)))

     ('ping 'ping)

     ('pong 'pong)

     ('close
      (let ((code (maps:get 'code frame 1000))
            (reason (maps:get 'reason frame (binary))))
        (if (> (byte_size reason) 0)
          `#(close ,code ,reason)
          'close)))

     (_ 'close))))

(defun build-ws-opts
  "Build gun WebSocket options from LHC options.

  Args:
    opts: LHC options map

  Returns:
    Gun WebSocket options map"
  ((opts)
   (let* ((protocols (maps:get 'protocols opts '()))
          (extensions (maps:get 'extensions opts (map)))
          (gun-opts (maps:get 'gun-opts opts (map)))
          (ws-opts (maps:get 'ws-opts gun-opts (map))))
     (maps:merge
      (maps:from_list
       (lists:flatten
        `(,(if (> (length protocols) 0)
             `(#(protocols ,protocols))
             '())
          ,(if (> (map_size extensions) 0)
             `(#(extensions ,extensions))
             '()))))
      ws-opts))))

;;; ===================================================================
;;; WebSocket Close Codes (for reference)
;;; ===================================================================
;;
;; Standard close codes:
;; 1000 - Normal closure
;; 1001 - Going away
;; 1002 - Protocol error
;; 1003 - Unsupported data
;; 1005 - No status received (reserved)
;; 1006 - Abnormal closure (reserved)
;; 1007 - Invalid frame payload data
;; 1008 - Policy violation
;; 1009 - Message too big
;; 1010 - Mandatory extension
;; 1011 - Internal server error
;; 1015 - TLS handshake failure (reserved)
