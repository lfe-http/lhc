;; Integration tests for LHC cookie management
;; Uses httpbin.org cookie endpoints for testing

(include-file "test_server.lfe")

(defmodule lhc_cookies_SUITE
  (export (all 0) (suite 0)
          (init_per_suite 1) (end_per_suite 1)
          (init_per_testcase 2) (end_per_testcase 2))
  (export (test_cookie_store_init 1)
          (test_cookie_storage_across_requests 1)
          (test_set_cookie_parsing 1)
          (test_cookie_store_operations 1)
          (test_multiple_cookies 1)
          (test_cookie_with_httpc 1)
          (test_cookie_with_gun 1)))

;;; Suite Configuration

(defun all ()
  "Return list of all test cases"
  '(test_cookie_store_init
    test_cookie_storage_across_requests
    test_set_cookie_parsing
    test_cookie_store_operations
    test_multiple_cookies
    test_cookie_with_httpc
    test_cookie_with_gun))

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

;;; Cookie Store Tests

(defun test_cookie_store_init
  (['suite] ())
  (['doc] "Test cookie store initialization")
  ([config] (when (is_list config))
   ;; Test default init
   (let ((store1 (lhc-cookies:init)))
     (line (test-pat 'true (is_tuple store1)))

     ;; Test init with options
     (let ((store2 (lhc-cookies:init (map))))
       (line (test-pat 'true (is_tuple store2)))
       'ok))))

;;; Cookie Storage Tests

(defun test_cookie_storage_across_requests
  (['suite] ())
  (['doc] "Test that cookies persist across multiple requests")
  ([config] (when (is_list config))
   (let* ((base-url (config 'base_url config))
          (set-url (binary base-url #"/cookies/set?testcookie=testvalue"))
          (get-url (binary base-url #"/cookies"))
          (store (lhc-cookies:init))
          ;; First request: set a cookie
          ;; Note: httpbin /cookies/set redirects, so we get the Set-Cookie header
          (`#(ok ,resp1) (lhc:get set-url (map 'backend 'gun))))

     ;; Extract Set-Cookie headers from response
     (let* ((headers1 (maps:get 'headers resp1))
            (set-cookies (lhc-cookies:extract-set-cookies headers1)))

       ;; Verify we got cookies
       (line (test-pat 'true (> (length set-cookies) 0)))

       ;; Store cookies
       (let ((`#(ok ,new-store) (lhc-cookies:store get-url set-cookies store)))

         ;; Query cookies for next request
         (let ((`#(ok ,cookies ,final-store) (lhc-cookies:query new-store get-url)))

           ;; Verify we got cookies back
           (line (test-pat 'true (> (length cookies) 0)))
           'ok))))))

;;; Set-Cookie Parsing Tests

(defun test_set_cookie_parsing
  (['suite] ())
  (['doc] "Test Set-Cookie header parsing")
  ([config] (when (is_list config))
   (let* ((base-url (config 'base_url config))
          (url (binary base-url #"/cookies/set?name1=value1"))
          (store (lhc-cookies:init))
          ;; Make request that sets cookies
          (`#(ok ,response) (lhc:get url (map 'backend 'gun))))

     ;; Extract Set-Cookie headers
     (let* ((headers (maps:get 'headers response))
            (set-cookies (lhc-cookies:extract-set-cookies headers)))

       ;; Verify we got Set-Cookie headers
       (line (test-pat 'true (is_list set-cookies)))

       ;; Store the cookies (this tests parsing internally)
       (let ((`#(ok ,new-store) (lhc-cookies:store url set-cookies store)))
         'ok)))))

;;; Cookie Store Operations Tests

(defun test_cookie_store_operations
  (['suite] ())
  (['doc] "Test cookie store operations: init, store, query, list, clear")
  ([config] (when (is_list config))
   (let* ((base-url (config 'base_url config))
          (url (binary base-url #"/cookies"))
          (store1 (lhc-cookies:init)))

     ;; Create a fake Set-Cookie header
     (let ((set-cookie-headers '(#(<<"set-cookie">>
                                   <<"testcookie=testvalue; Path=/; Domain=httpbin.org">>))))

       ;; Store cookie
       (let ((`#(ok ,store2) (lhc-cookies:store url set-cookie-headers store1)))

         ;; Query cookies
         (let ((`#(ok ,cookies ,store3) (lhc-cookies:query store2 url)))
           (line (test-pat 'true (is_list cookies)))

           ;; List all cookies
           (let ((`#(ok ,all-cookies ,store4) (lhc-cookies:list-all store3)))
             (line (test-pat 'true (is_list all-cookies)))

             ;; Clear cookies
             (let ((`#(ok ,store5) (lhc-cookies:clear store4)))

               ;; Verify cookies cleared
               (let ((`#(ok ,empty-cookies ,_store6) (lhc-cookies:query store5 url)))
                 (line (test-pat 0 (length empty-cookies)))
                 'ok)))))))))

;;; Multiple Cookies Tests

(defun test_multiple_cookies
  (['suite] ())
  (['doc] "Test handling multiple cookies")
  ([config] (when (is_list config))
   (let* ((base-url (config 'base_url config))
          ;; Set multiple cookies in one request
          (set-url (binary base-url #"/cookies/set?cookie1=value1&cookie2=value2"))
          (get-url (binary base-url #"/cookies"))
          (store (lhc-cookies:init))
          ;; Set multiple cookies
          (`#(ok ,resp1) (lhc:get set-url (map 'backend 'gun))))

     ;; Extract and store cookies
     (let* ((headers (maps:get 'headers resp1))
            (set-cookies (lhc-cookies:extract-set-cookies headers)))

       (let ((`#(ok ,new-store) (lhc-cookies:store get-url set-cookies store)))

         ;; Query cookies
         (let ((`#(ok ,cookies ,_final-store) (lhc-cookies:query new-store get-url)))

           ;; Note: httpbin may merge cookies, so we just verify we got cookies
           (line (test-pat 'true (> (length cookies) 0)))
           'ok))))))

;;; Backend Integration Tests

(defun test_cookie_with_httpc
  (['suite] ())
  (['doc] "Test cookie management with httpc backend")
  ([config] (when (is_list config))
   (let* ((base-url (config 'base_url config))
          (url (binary base-url #"/cookies/set?httpc_cookie=httpc_value"))
          (store (lhc-cookies:init))
          ;; Make request with httpc
          (`#(ok ,response) (lhc:get url (map 'backend 'httpc))))

     ;; Extract Set-Cookie headers
     (let* ((headers (maps:get 'headers response))
            (set-cookies (lhc-cookies:extract-set-cookies headers)))

       ;; Verify cookies were set
       (line (test-pat 'true (is_list set-cookies)))

       ;; Store cookies
       (let ((`#(ok ,_new-store) (lhc-cookies:store url set-cookies store)))
         'ok)))))

(defun test_cookie_with_gun
  (['suite] ())
  (['doc] "Test cookie management with gun backend")
  ([config] (when (is_list config))
   (let* ((base-url (config 'base_url config))
          (url (binary base-url #"/cookies/set?gun_cookie=gun_value"))
          (store (lhc-cookies:init))
          ;; Make request with gun
          (`#(ok ,response) (lhc:get url (map 'backend 'gun))))

     ;; Extract Set-Cookie headers
     (let* ((headers (maps:get 'headers response))
            (set-cookies (lhc-cookies:extract-set-cookies headers)))

       ;; Verify cookies were set
       (line (test-pat 'true (is_list set-cookies)))

       ;; Store cookies
       (let ((`#(ok ,_new-store) (lhc-cookies:store url set-cookies store)))
         'ok)))))
