;;;; WebSocket handler for HTTP test server
;;;; Simply echoes back messages received
(defmodule lhc-test-ws-handler
  (behavior cowboy_websocket)
  (export
   (init 2)
   (websocket_init 1)
   (websocket_handle 2)
   (websocket_info 2)
   (terminate 3)))

(defun init (req state)
  `#(cowboy_websocket ,req ,state))

(defun websocket_init (state)
  `#(ok ,state))

;; Handle text frames - echo back
(defun websocket_handle
  ([`#(text ,msg) state]
   `#(reply #(text ,msg) ,state))
  ;; Handle binary frames - echo back
  ([`#(binary ,data) state]
   `#(reply #(binary ,data) ,state))
  ;; Handle ping frames - respond with pong
  (['ping state]
   `#(reply pong ,state))
  ;; Handle pong frames - ignore
  (['pong state]
   `#(ok ,state))
  ;; Handle close frames
  ([`#(close ,code ,reason) state]
   `#(reply #(close ,code ,reason) ,state))
  ;; Catch-all
  ([_frame state]
   `#(ok ,state)))

;; Handle Erlang messages (none expected)
(defun websocket_info (_info state)
  `#(ok ,state))

(defun terminate (_reason _req _state)
  'ok)
