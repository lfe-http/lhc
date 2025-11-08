(defmodule lhc-test-server
  "Simple HTTP test server for integration testing.

  This module provides a lightweight HTTP server for testing HTTP client
  functionality. It can handle various test scenarios including:
  - Basic GET/POST/PUT/DELETE requests
  - Cookie setting and verification
  - Custom headers
  - Delayed responses
  - Error conditions
  - WebSocket upgrade (via Cowboy)"
  (export
   (start 1)
   (stop 1)
   (get-port 1)))

(defun start (config)
  "Start a test HTTP server.

  Args:
    config: Configuration map with optional keys:
      - port: Port number (default: random)
      - handlers: List of custom handlers

  Returns:
    {ok, updated-config} with server info added"
  (let* ((port (maps:get 'port config (get-random-port)))
         (dispatch (cowboy_router:compile
                    `(#(_ ,(route-list))))))
    ;; Start Cowboy HTTP server
    (case (cowboy:start_clear 'lhc_test_http
                              `(#(port ,port))
                              (map 'env (map 'dispatch dispatch)))
      (`#(ok ,pid)
       ;; Add server info to config
       `#(ok ,(maps:merge config
                          (maps:from_list
                           `(#(test_server_pid ,pid)
                             #(test_server_port ,port)
                             #(test_server_url ,(build-url port)))))))
      (`#(error ,reason)
       `#(error ,reason)))))

(defun stop (config)
  "Stop the test HTTP server.

  Args:
    config: Configuration with server info

  Returns:
    ok"
  (cowboy:stop_listener 'lhc_test_http)
  'ok)

(defun get-port (config)
  "Get the test server port from config.

  Args:
    config: Configuration map

  Returns:
    Port number"
  (maps:get 'test_server_port config))

(defun get-random-port ()
  "Get a random port number for testing.

  Returns:
    Port number between 10000-60000"
  (+ 10000 (rem (erlang:unique_integer '(positive)) 50000)))

(defun build-url (port)
  "Build base URL for test server.

  Args:
    port: Port number

  Returns:
    Binary URL"
  (list_to_binary (++ "http://localhost:" (integer_to_list port))))

(defun route-list ()
  "Define HTTP routes for test server.

  Returns:
    List of Cowboy route specifications"
  '(
    ;; Basic echo endpoint
    #("/echo" lhc-test-handler ())

    ;; GET endpoint
    #("/get" lhc-test-handler (#(method get)))

    ;; POST endpoint
    #("/post" lhc-test-handler (#(method post)))

    ;; PUT endpoint
    #("/put" lhc-test-handler (#(method put)))

    ;; DELETE endpoint
    #("/delete" lhc-test-handler (#(method delete)))

    ;; Status code endpoints
    #("/status/:code" lhc-test-handler (#(type status)))

    ;; Delay endpoint
    #("/delay/:seconds" lhc-test-handler (#(type delay)))

    ;; Cookie endpoints
    #("/cookies/set" lhc-test-handler (#(type set_cookies)))
    #("/cookies/get" lhc-test-handler (#(type get_cookies)))
    #("/cookies" lhc-test-handler (#(type get_cookies)))

    ;; Redirect endpoint
    #("/redirect/:count" lhc-test-handler (#(type redirect)))

    ;; Headers endpoint
    #("/headers" lhc-test-handler (#(type headers)))

    ;; WebSocket endpoint
    #("/ws" lhc-test-ws-handler ())
    ))

;; Note: The actual handlers (lhc-test-handler, lhc-test-ws-handler)
;; would need to be implemented in Erlang or as separate modules.
;; For now, this provides the structure.
