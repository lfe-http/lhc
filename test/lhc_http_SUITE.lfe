;; Integration tests for LHC HTTP functionality
;; Uses local Cowboy test server for HTTP testing

(include-file "test_server.lfe")

(defmodule lhc_http_SUITE
  (export (all 0) (suite 0) (groups 0)
          (init_per_suite 1) (end_per_suite 1)
          (init_per_group 2) (end_per_group 2)
          (init_per_testcase 2) (end_per_testcase 2))
  (export (test_get_httpc 1)
          (test_get_gun 1)
          (test_post_httpc 1)
          (test_post_gun 1)
          (test_put_httpc 1)
          (test_put_gun 1)
          (test_delete_httpc 1)
          (test_delete_gun 1)
          (test_headers 1)
          (test_status_codes 1)
          (test_error_handling 1)
          (test_timeout 1)))

;;; Suite Configuration

(defun all ()
  "Return list of all test cases and groups"
  '(#(group httpc_tests)
    #(group gun_tests)
    test_headers
    test_status_codes
    test_error_handling
    test_timeout))

(defun suite ()
  "Return suite-level configuration"
  ())

(defun groups ()
  "Define test groups"
  '(#(httpc_tests ()
      (test_get_httpc test_post_httpc test_put_httpc test_delete_httpc))
    #(gun_tests ()
      (test_get_gun test_post_gun test_put_gun test_delete_gun))))

(defun init_per_suite (config)
  "Initialize suite - start required applications"
  ;; Start inets for httpc
  (application:ensure_all_started 'inets)
  ;; Start gun application
  (application:ensure_all_started 'gun)
  ;; Start ssl
  (application:ensure_all_started 'ssl)
  ;; Start cowboy for test server
  (application:ensure_all_started 'cowboy)
  ;; Start local test server
  (case (lhc_test_server:start (maps:from_list config))
    (`#(ok ,updated-config)
     (maps:to_list updated-config))
    (`#(error ,reason)
     (: ct pal "Failed to start test server: ~p" `(,reason))
     (: ct fail "Test server startup failed"))))

(defun end_per_suite (config)
  "Clean up suite resources"
  ;; Stop local test server
  (lhc_test_server:stop (maps:from_list config))
  'ok)

(defun init_per_group
  (('httpc_tests config)
   (cons `#(backend httpc) config))
  (('gun_tests config)
   (cons `#(backend gun) config))
  ((group config)
   config))

(defun end_per_group (name config)
  'ok)

(defun init_per_testcase (func config)
  "Set up individual test case with timeout"
  (let ((dog (: test_server timetrap (: test_server seconds 30))))
    (cons `#(watchdog ,dog) config)))

(defun end_per_testcase (func config)
  "Tear down individual test case"
  (let ((dog (config 'watchdog config)))
    (: test_server timetrap_cancel dog)))

;;; HTTP GET Tests

(defun test_get_httpc
  (['suite] ())
  (['doc] "Test GET request with httpc backend")
  ([config] (when (is_list config))
   (let* ((base-url (config 'test_server_url config))
          (url (list_to_binary (++ (binary_to_list base-url) "/get?foo=bar")))
          (opts (map 'backend 'httpc))
          ;; Test successful GET
          (`#(ok ,response) (lhc:get url opts)))
     ;; Verify response structure
     (line (test-pat 200 (maps:get 'status response)))
     (line (test-pat 'true (is_list (maps:get 'headers response))))
     (line (test-pat 'true (is_binary (maps:get 'body response))))
     'ok)))

(defun test_get_gun
  (['suite] ())
  (['doc] "Test GET request with gun backend")
  ([config] (when (is_list config))
   (let* ((base-url (config 'test_server_url config))
          (url (list_to_binary (++ (binary_to_list base-url) "/get?test=value")))
          (opts (map 'backend 'gun))
          ;; Test successful GET
          (`#(ok ,response) (lhc:get url opts)))
     ;; Verify response
     (line (test-pat 200 (maps:get 'status response)))
     (line (test-pat 'true (is_binary (maps:get 'body response))))
     'ok)))

;;; HTTP POST Tests

(defun test_post_httpc
  (['suite] ())
  (['doc] "Test POST request with httpc backend")
  ([config] (when (is_list config))
   (let* ((base-url (config 'test_server_url config))
          (url (list_to_binary (++ (binary_to_list base-url) "/post")))
          (body #"{\"test\": \"data\"}")
          (opts (map 'backend 'httpc
                     'headers '(#(#"content-type" #"application/json"))))
          ;; Test POST
          (`#(ok ,response) (lhc:post url body opts)))
     (line (test-pat 200 (maps:get 'status response)))
     'ok)))

(defun test_post_gun
  (['suite] ())
  (['doc] "Test POST request with gun backend")
  ([config] (when (is_list config))
   (let* ((base-url (config 'test_server_url config))
          (url (list_to_binary (++ (binary_to_list base-url) "/post")))
          (body #"{\"foo\": \"bar\"}")
          (opts (map 'backend 'gun
                     'headers '(#(#"content-type" #"application/json"))))
          ;; Test POST
          (`#(ok ,response) (lhc:post url body opts)))
     (line (test-pat 200 (maps:get 'status response)))
     'ok)))

;;; HTTP PUT Tests

(defun test_put_httpc
  (['suite] ())
  (['doc] "Test PUT request with httpc backend")
  ([config] (when (is_list config))
   (let* ((base-url (config 'test_server_url config))
          (url (list_to_binary (++ (binary_to_list base-url) "/put")))
          (body #"{\"update\": \"data\"}")
          (opts (map 'backend 'httpc
                     'headers '(#(#"content-type" #"application/json"))))
          (`#(ok ,response) (lhc:put url body opts)))
     (line (test-pat 200 (maps:get 'status response)))
     'ok)))

(defun test_put_gun
  (['suite] ())
  (['doc] "Test PUT request with gun backend")
  ([config] (when (is_list config))
   (let* ((base-url (config 'test_server_url config))
          (url (list_to_binary (++ (binary_to_list base-url) "/put")))
          (body #"{\"update\": \"value\"}")
          (opts (map 'backend 'gun
                     'headers '(#(#"content-type" #"application/json"))))
          (`#(ok ,response) (lhc:put url body opts)))
     (line (test-pat 200 (maps:get 'status response)))
     'ok)))

;;; HTTP DELETE Tests

(defun test_delete_httpc
  (['suite] ())
  (['doc] "Test DELETE request with httpc backend")
  ([config] (when (is_list config))
   (let* ((base-url (config 'test_server_url config))
          (url (list_to_binary (++ (binary_to_list base-url) "/delete")))
          (opts (map 'backend 'httpc))
          (`#(ok ,response) (lhc:delete url opts)))
     (line (test-pat 200 (maps:get 'status response)))
     'ok)))

(defun test_delete_gun
  (['suite] ())
  (['doc] "Test DELETE request with gun backend")
  ([config] (when (is_list config))
   (let* ((base-url (config 'test_server_url config))
          (url (list_to_binary (++ (binary_to_list base-url) "/delete")))
          (opts (map 'backend 'gun))
          (`#(ok ,response) (lhc:delete url opts)))
     (line (test-pat 200 (maps:get 'status response)))
     'ok)))

;;; Header Tests

(defun test_headers
  (['suite] ())
  (['doc] "Test custom headers in requests")
  ([config] (when (is_list config))
   (let* ((base-url (config 'test_server_url config))
          (url (list_to_binary (++ (binary_to_list base-url) "/headers")))
          (custom-headers '(#(#"X-Test-Header" #"test-value")
                           #(#"X-Custom" #"custom-value")))
          (opts (map 'headers custom-headers))
          ;; Test with httpc
          (`#(ok ,response1) (lhc:get url (maps:put 'backend 'httpc opts))))
     (line (test-pat 200 (maps:get 'status response1)))

     ;; Test with gun
     (let ((`#(ok ,response2) (lhc:get url (maps:put 'backend 'gun opts))))
       (line (test-pat 200 (maps:get 'status response2)))
       'ok))))

;;; Status Code Tests

(defun test_status_codes
  (['suite] ())
  (['doc] "Test various HTTP status codes")
  ([config] (when (is_list config))
   (let* ((base-url (config 'test_server_url config))
          ;; Test 404
          (`#(ok ,resp404) (lhc:get (list_to_binary (++ (binary_to_list base-url) "/status/404")) (map))))
     (line (test-pat 404 (maps:get 'status resp404)))

     ;; Test 500
     (let ((`#(ok ,resp500) (lhc:get (list_to_binary (++ (binary_to_list base-url) "/status/500")) (map))))
       (line (test-pat 500 (maps:get 'status resp500)))

       ;; Test 201
       (let ((`#(ok ,resp201) (lhc:get (list_to_binary (++ (binary_to_list base-url) "/status/201")) (map))))
         (line (test-pat 201 (maps:get 'status resp201)))
         'ok)))))

;;; Error Handling Tests

(defun test_error_handling
  (['suite] ())
  (['doc] "Test error handling for invalid URLs and connection failures")
  ([config] (when (is_list config))
   ;; Test invalid domain
   (let ((result1 (lhc:get #"http://this-domain-does-not-exist-12345.com" (map))))
     (case result1
       (`#(error ,error1)
        (line (test-pat 'true (is_map error1)))
        (line (test-pat 'connection (maps:get 'reason error1)))

        ;; Test connection refused
        (let ((result2 (lhc:get #"http://localhost:9" (map 'timeout 1000))))
          (case result2
            (`#(error ,error2)
             (line (test-pat 'true (is_map error2)))
             'ok)
            (_
             (: ct pal "ERROR: Expected error tuple, got: ~p" `(,result2))
             (: ct fail "Did not get error tuple")))))
       (_
        (: ct pal "ERROR: Expected error tuple for invalid domain, got: ~p" `(,result1))
        (: ct fail "Did not get error tuple for invalid domain"))))))

;;; Timeout Tests

(defun test_timeout
  (['suite] ())
  (['doc] "Test timeout handling")
  ([config] (when (is_list config))
   (let* ((base-url (config 'test_server_url config))
          (url (list_to_binary (++ (binary_to_list base-url) "/delay/10")))
          ;; Test timeout on slow endpoint
          ;; The /delay/{n} endpoint delays response for n seconds
          (result (lhc:get url (map 'timeout 1000))))  ; 1 second timeout
     (case result
       (`#(error ,error)
        (line (test-pat 'timeout (maps:get 'reason error)))
        'ok)
       (_
        (: ct pal "ERROR: Expected timeout error, got: ~p" `(,result))
        (: ct fail "Did not get timeout error"))))))
