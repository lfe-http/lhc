;; Integration tests for LHC asynchronous operations

(include-file "test_server.lfe")

(defmodule lhc_async_SUITE
  (export (all 0) (suite 0)
          (init_per_suite 1) (end_per_suite 1)
          (init_per_testcase 2) (end_per_testcase 2))
  (export (test_async_get 1)
          (test_async_post 1)
          (test_async_await 1)
          (test_async_timeout 1)
          (test_multiple_async_requests 1)))

;;; Suite Configuration

(defun all ()
  "Return list of all test cases"
  '(test_async_get
    test_async_post
    test_async_await
    test_async_timeout
    test_multiple_async_requests))

(defun suite () ())

(defun init_per_suite (config)
  "Initialize suite"
  (application:ensure_all_started 'inets)
  (application:ensure_all_started 'gun)
  (application:ensure_all_started 'ssl)
  (cons `#(base_url ,(binary "https://httpbin.org")) config))

(defun end_per_suite (config)
  'ok)

(defun init_per_testcase (func config)
  (let ((dog (: test_server timetrap (: test_server seconds 30))))
    (cons `#(watchdog ,dog) config)))

(defun end_per_testcase (func config)
  (let ((dog (config 'watchdog config)))
    (: test_server timetrap_cancel dog)))

;;; Async GET Tests

(defun test_async_get
  (['suite] ())
  (['doc] "Test async GET request")
  ([config] (when (is_list config))
   (let* ((base-url (config 'base_url config))
          (url (binary base-url #"/get"))
          (opts (map 'mode 'async 'backend 'httpc)))
     ;; Issue async request
     (line (test-pat `#(ok ,request-id)
                     (lhc:get url opts)))
     ;; Verify we got a request ID
     (line (test-pat 'true (is_reference request-id)))
     ;; Await response
     (line (test-pat `#(ok ,response)
                     (lhc:await request-id 10000)))
     ;; Verify response
     (line (test-pat 200 (maps:get 'status response)))
     'ok)))

;;; Async POST Tests

(defun test_async_post
  (['suite] ())
  (['doc] "Test async POST request")
  ([config] (when (is_list config))
   (let* ((base-url (config 'base_url config))
          (url (binary base-url #"/post"))
          (body #"{\"async\": \"test\"}")
          (opts (map 'mode 'async
                     'backend 'gun
                     'headers '(#(<<"content-type">> <<"application/json">>)))))
     ;; Issue async POST
     (line (test-pat `#(ok ,request-id)
                     (lhc:post url body opts)))
     ;; Await response
     (line (test-pat `#(ok ,response)
                     (lhc:await request-id 10000)))
     (line (test-pat 200 (maps:get 'status response)))
     'ok)))

;;; Await Tests

(defun test_async_await
  (['suite] ())
  (['doc] "Test await with different timeouts")
  ([config] (when (is_list config))
   (let* ((base-url (config 'base_url config))
          (url (binary base-url #"/get"))
          (opts (map 'mode 'async)))
     ;; Start async request
     (line (test-pat `#(ok ,req-id)
                     (lhc:get url opts)))
     ;; Await with long timeout
     (line (test-pat `#(ok ,response)
                     (lhc:await req-id 15000)))
     (line (test-pat 200 (maps:get 'status response)))
     'ok)))

;;; Timeout Tests

(defun test_async_timeout
  (['suite] ())
  (['doc] "Test async request timeout")
  ([config] (when (is_list config))
   (let* ((base-url (config 'base_url config))
          ;; Use delay endpoint that takes 10 seconds
          (url (binary base-url #"/delay/10"))
          (opts (map 'mode 'async 'backend 'httpc)))
     ;; Start async request
     (line (test-pat `#(ok ,req-id)
                     (lhc:get url opts)))
     ;; Await with short timeout (should timeout)
     (line (test-pat `#(error ,error)
                     (lhc:await req-id 1000)))
     (line (test-pat 'timeout (maps:get 'reason error)))
     'ok)))

;;; Multiple Async Requests

(defun test_multiple_async_requests
  (['suite] ())
  (['doc] "Test multiple concurrent async requests")
  ([config] (when (is_list config))
   (let* ((base-url (config 'base_url config))
          (url1 (binary base-url #"/get?req=1"))
          (url2 (binary base-url #"/get?req=2"))
          (url3 (binary base-url #"/get?req=3"))
          (opts (map 'mode 'async 'backend 'gun)))
     ;; Start multiple async requests
     (line (test-pat `#(ok ,req1)
                     (lhc:get url1 opts)))
     (line (test-pat `#(ok ,req2)
                     (lhc:get url2 opts)))
     (line (test-pat `#(ok ,req3)
                     (lhc:get url3 opts)))

     ;; Await all responses
     (line (test-pat `#(ok ,resp1)
                     (lhc:await req1 10000)))
     (line (test-pat `#(ok ,resp2)
                     (lhc:await req2 10000)))
     (line (test-pat `#(ok ,resp3)
                     (lhc:await req3 10000)))

     ;; Verify all succeeded
     (line (test-pat 200 (maps:get 'status resp1)))
     (line (test-pat 200 (maps:get 'status resp2)))
     (line (test-pat 200 (maps:get 'status resp3)))
     'ok)))
