;; Integration tests for LHC WebSocket functionality
;; Uses echo.websocket.org for WebSocket echo testing
(defmodule lhc_websocket_SUITE
  (export (all 0) (suite 0)
          (init_per_suite 1) (end_per_suite 1)
          (init_per_testcase 2) (end_per_testcase 2))
  (export (test_websocket_upgrade_sync 1)
          (test_websocket_upgrade_async 1)
          (test_text_frame_echo 1)
          (test_binary_frame_echo 1)
          (test_multiple_text_frames 1)
          (test_ping_pong 1)
          (test_close_handling 1)))

(include-file "test_server.lfe")

;;; Suite Configuration

(defun all ()
  "Return list of all test cases"
  '(test_websocket_upgrade_sync
    test_websocket_upgrade_async
    test_text_frame_echo
    test_binary_frame_echo
    test_multiple_text_frames
    test_ping_pong
    test_close_handling))

(defun suite () ())

(defun init_per_suite (config)
  "Initialize suite"
  (application:ensure_all_started 'gun)
  (application:ensure_all_started 'ssl)
  ;; Start local WebSocket test server
  (let ((`#(ok ,port) (ws_test_server:start 8080)))
    ;; Use local server for testing
    (cons `#(ws_url ,(binary "ws://localhost:8080")) config)))

(defun end_per_suite (config)
  "Clean up suite - stop WebSocket test server"
  (ws_test_server:stop)
  'ok)

(defun init_per_testcase (func config)
  (let ((dog (: test_server timetrap (: test_server seconds 30))))
    (cons `#(watchdog ,dog) config)))

(defun end_per_testcase (func config)
  (let ((dog (config 'watchdog config)))
    (: test_server timetrap_cancel dog)))

;;; WebSocket Upgrade Tests

(defun test_websocket_upgrade_sync
  (['suite] ())
  (['doc] "Test synchronous WebSocket upgrade")
  ([config] (when (is_list config))
   (let* ((ws-url (config 'ws_url config))
          ;; Open connection to WebSocket server
          (`#(ok ,conn) (lhc:open ws-url (map 'backend 'gun)))
          ;; Upgrade to WebSocket (sync mode)
          (`#(ok ,ws-pid) (lhc-websocket:upgrade conn "/" (map) (map 'mode 'sync))))

     ;; Verify we got a WebSocket reference
     (line (test-pat 'true (is_reference ws-pid)))

     ;; Close connection
     (lhc:close conn)
     'ok)))

(defun test_websocket_upgrade_async
  (['suite] ())
  (['doc] "Test asynchronous WebSocket upgrade")
  ([config] (when (is_list config))
   (let* ((ws-url (config 'ws_url config))
          (`#(ok ,conn) (lhc:open ws-url (map 'backend 'gun)))
          ;; Upgrade to WebSocket (async mode)
          (`#(ok ,stream-ref) (lhc-websocket:upgrade conn "/" (map) (map 'mode 'async))))

     ;; Verify we got a stream reference
     (line (test-pat 'true (is_reference stream-ref)))

     ;; Wait for upgrade response
     (receive
       (`#(gun_upgrade ,c ,s ,_ ,_) (when (=:= c conn) (=:= s stream-ref))
        'ok)
       (msg
        (error `#(unexpected_message ,msg)))
       (after 5000
         (error 'upgrade_timeout)))

     ;; Close connection
     (lhc:close conn)
     'ok)))

;;; Text Frame Tests

(defun test_text_frame_echo
  (['suite] ())
  (['doc] "Test sending and receiving text frames")
  ([config] (when (is_list config))
   (let* ((ws-url (config 'ws_url config))
          (`#(ok ,conn) (lhc:open ws-url (map 'backend 'gun)))
          ;; Upgrade to WebSocket
          (`#(ok ,ws) (lhc-websocket:upgrade conn "/" (map) (map 'mode 'sync))))

     ;; Send text frame
     (let ((text-data #"Hello WebSocket!"))
       (line (test-pat 'ok
                       (lhc-websocket:send-text conn ws text-data (map))))

       ;; Await echo response
       (let ((`#(ok ,frame) (lhc-websocket:await-frame conn ws 5000)))

         ;; Verify we got text frame back
         (line (test-pat 'text (maps:get 'type frame)))
         (line (test-pat text-data (maps:get 'data frame)))))

     ;; Close connection
     (lhc:close conn)
     'ok)))

;;; Binary Frame Tests

(defun test_binary_frame_echo
  (['suite] ())
  (['doc] "Test sending and receiving binary frames")
  ([config] (when (is_list config))
   (let* ((ws-url (config 'ws_url config))
          (`#(ok ,conn) (lhc:open ws-url (map 'backend 'gun)))
          (`#(ok ,ws) (lhc-websocket:upgrade conn "/" (map) (map 'mode 'sync))))

     ;; Send binary frame
     (let ((binary-data (binary 1 2 3 4 5 6 7 8)))
       (line (test-pat 'ok
                       (lhc-websocket:send-binary conn ws binary-data (map))))

       ;; Await echo response
       (let ((`#(ok ,frame) (lhc-websocket:await-frame conn ws 5000)))

         ;; Verify we got binary frame back
         (line (test-pat 'binary (maps:get 'type frame)))
         (line (test-pat binary-data (maps:get 'data frame)))))

     ;; Close connection
     (lhc:close conn)
     'ok)))

;;; Multiple Frames Tests

(defun test_multiple_text_frames
  (['suite] ())
  (['doc] "Test sending multiple text frames")
  ([config] (when (is_list config))
   (let* ((ws-url (config 'ws_url config))
          (`#(ok ,conn) (lhc:open ws-url (map 'backend 'gun)))
          (`#(ok ,ws) (lhc-websocket:upgrade conn "/" (map) (map 'mode 'sync))))

     ;; Send multiple text frames
     (line (test-pat 'ok
                     (lhc-websocket:send-text conn ws #"Frame 1" (map))))
     (line (test-pat 'ok
                     (lhc-websocket:send-text conn ws #"Frame 2" (map))))
     (line (test-pat 'ok
                     (lhc-websocket:send-text conn ws #"Frame 3" (map))))

     ;; Receive all frames
     (let ((`#(ok ,frame1) (lhc-websocket:await-frame conn ws 5000)))
       (line (test-pat #"Frame 1" (maps:get 'data frame1)))

       (let ((`#(ok ,frame2) (lhc-websocket:await-frame conn ws 5000)))
         (line (test-pat #"Frame 2" (maps:get 'data frame2)))

         (let ((`#(ok ,frame3) (lhc-websocket:await-frame conn ws 5000)))
           (line (test-pat #"Frame 3" (maps:get 'data frame3)))

           ;; Close connection
           (lhc:close conn)
           'ok))))))

;;; Ping/Pong Tests

(defun test_ping_pong
  (['suite] ())
  (['doc] "Test WebSocket ping/pong")
  ([config] (when (is_list config))
   (let* ((ws-url (config 'ws_url config))
          (`#(ok ,conn) (lhc:open ws-url (map 'backend 'gun)))
          (`#(ok ,ws) (lhc-websocket:upgrade conn "/" (map) (map 'mode 'sync))))

     ;; Send ping frame
     ;; Note: Both Gun and Cowboy handle ping/pong automatically at the protocol level
     ;; The pong response is not surfaced to the application layer
     (line (test-pat 'ok
                     (lhc-websocket:send-ping conn ws #"ping-data" (map))))

     ;; Send a text frame to verify connection is still active after ping
     (line (test-pat 'ok
                     (lhc-websocket:send-text conn ws #"ping-test" (map))))

     ;; Verify we get the text echo back (proves ping didn't break connection)
     (let ((`#(ok ,frame) (lhc-websocket:await-frame conn ws 5000)))
       (line (test-pat 'text (maps:get 'type frame)))
       (line (test-pat #"ping-test" (maps:get 'data frame))))

     ;; Close connection
     (lhc:close conn)
     'ok)))

;;; Close Handling Tests

(defun test_close_handling
  (['suite] ())
  (['doc] "Test WebSocket close handling")
  ([config] (when (is_list config))
   (let* ((ws-url (config 'ws_url config))
          (`#(ok ,conn) (lhc:open ws-url (map 'backend 'gun)))
          (`#(ok ,ws) (lhc-websocket:upgrade conn "/" (map) (map 'mode 'sync))))

     ;; Send close frame
     (line (test-pat 'ok
                     (lhc-websocket:send-close conn ws 1000 #"Normal close" (map))))

     ;; Await close response
     ;; Note: Server should respond with close frame
     (let ((`#(ok ,frame) (lhc-websocket:await-frame conn ws 5000)))

       ;; Verify we got close frame
       (line (test-pat 'close (maps:get 'type frame)))

       ;; Close connection
       (lhc:close conn)
       'ok))))
