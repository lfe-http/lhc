;; Integration tests for LHC cookie management
;; Uses httpbin.org cookie endpoints for testing

(include-file "test_server.lfe")

(defmodule lhc_cookies_SUITE
  (export (all 0) (suite 0)
          (init_per_suite 1) (end_per_suite 1)
          (init_per_testcase 2) (end_per_testcase 2))
  (export (test_cookie_store_init 1)
          (test_automatic_cookie_handling 1)
          (test_automatic_cookie_persistence 1)
          (test_manual_cookie_inspection 1)
          (test_cookie_store_operations 1)))

;;; Suite Configuration

(defun all ()
  "Return list of all test cases"
  '(test_cookie_store_init
    test_automatic_cookie_handling
    test_automatic_cookie_persistence))

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

;;; Automatic Cookie Handling Tests (Gun's built-in system)

(defun test_automatic_cookie_handling
  (['suite] ())
  (['doc] "Test Gun's automatic cookie handling - cookies are set and sent automatically")
  ([config] (when (is_list config))
   (let* ((base-url (config 'base_url config))
          ;; Initialize cookie store
          (cookie-store (lhc-cookies:init))
          ;; Open connection WITH cookie store - Gun will handle cookies automatically
          (`#(ok ,conn) (lhc:open base-url (map 'backend 'gun
                                                'cookie-store cookie-store))))

     ;; First request: Set a cookie via httpbin's /cookies/set endpoint
     ;; Gun will automatically extract and store the Set-Cookie header
     (let ((`#(ok ,resp1) (lhc-gun:do-request-sync conn 'GET
                                                    "/cookies/set?testcookie=testvalue"
                                                    #"" '() (map))))

       ;; Second request: Get cookies - Gun should automatically send the cookie
       (let ((`#(ok ,resp2) (lhc-gun:do-request-sync conn 'GET "/cookies"
                                                      #"" '() (map))))

         ;; Verify the response body contains our cookie
         ;; httpbin's /cookies endpoint returns JSON with our cookie
         (let ((body (maps:get 'body resp2)))
           ;; Simple check: body should contain "testcookie" and "testvalue"
           ;; binary:match returns {Pos, Len} on match, nomatch on failure
           (line (test-pat 'true (=/= 'nomatch (binary:match body #"testcookie"))))
           (line (test-pat 'true (=/= 'nomatch (binary:match body #"testvalue"))))

           (lhc:close conn)
           'ok))))))

(defun test_automatic_cookie_persistence
  (['suite] ())
  (['doc] "Test that cookies persist across multiple requests in Gun")
  ([config] (when (is_list config))
   (let* ((base-url (config 'base_url config))
          (cookie-store (lhc-cookies:init))
          (`#(ok ,conn) (lhc:open base-url (map 'backend 'gun
                                                'cookie-store cookie-store))))

     ;; Set multiple cookies
     (let ((`#(ok ,_resp1) (lhc-gun:do-request-sync conn 'GET
                                                     "/cookies/set?cookie1=value1"
                                                     #"" '() (map))))

       (let ((`#(ok ,_resp2) (lhc-gun:do-request-sync conn 'GET
                                                       "/cookies/set?cookie2=value2"
                                                       #"" '() (map))))

         ;; Verify both cookies are sent on subsequent request
         (let ((`#(ok ,resp3) (lhc-gun:do-request-sync conn 'GET "/cookies"
                                                        #"" '() (map))))

           (let ((body (maps:get 'body resp3)))
             ;; Both cookies should be present in the response body
             (line (test-pat 'true (=/= 'nomatch (binary:match body #"cookie1"))))
             (line (test-pat 'true (=/= 'nomatch (binary:match body #"cookie2"))))

             (lhc:close conn)
             'ok)))))))

;;; Manual Cookie Inspection Tests (Advanced/Debugging)

(defun test_manual_cookie_inspection
  (['suite] ())
  (['doc] "Test manual cookie inspection utilities for debugging")
  ([config] (when (is_list config))
   (let* ((store (lhc-cookies:init))
          (url #"https://httpbin.org/cookies"))

     ;; Manually store a cookie (for debugging/testing purposes)
     (let ((set-cookies '(#"session=abc123; Path=/; Domain=httpbin.org")))
       (let ((`#(ok ,store2) (lhc-cookies:store store set-cookies url)))

         ;; Query what cookies would apply to this URL
         (let ((`#(ok ,cookies ,store3) (lhc-cookies:query store2 url)))
           (line (test-pat 'true (> (length cookies) 0)))

           ;; List all cookies in store
           (let ((`#(ok ,all-cookies ,store4) (lhc-cookies:list-all store3)))
             (line (test-pat 'true (is_list all-cookies)))
             (line (test-pat 'true (> (length all-cookies) 0)))

             ;; Clear cookies
             (let ((`#(ok ,store5) (lhc-cookies:clear store4)))

               ;; Verify cookies cleared
               (let ((`#(ok ,empty-list ,_store6) (lhc-cookies:list-all store5)))
                 (line (test-pat 0 (length empty-list)))
                 'ok)))))))))

;;; Cookie Store Operations Tests

(defun test_cookie_store_operations
  (['suite] ())
  (['doc] "Test cookie store operations: init, store, query, list, clear")
  ([config] (when (is_list config))
   (let* ((base-url (config 'base_url config))
          (url (list_to_binary (++ (binary_to_list base-url) "/cookies")))
          (store1 (lhc-cookies:init)))

     ;; Create a fake Set-Cookie header
     (let ((set-cookie-headers '(#"testcookie=testvalue; Path=/; Domain=httpbin.org")))

       ;; Store cookie
       (let ((`#(ok ,store2) (lhc-cookies:store store1 set-cookie-headers url)))

         ;; Query cookies
         (let ((`#(ok ,cookies ,store3) (lhc-cookies:query store2 url)))
           (line (test-pat 'true (is_list cookies)))
           (line (test-pat 'true (> (length cookies) 0)))

           ;; List all cookies
           (let ((`#(ok ,all-cookies ,store4) (lhc-cookies:list-all store3)))
             (line (test-pat 'true (is_list all-cookies)))

             ;; Clear cookies
             (let ((`#(ok ,store5) (lhc-cookies:clear store4)))

               ;; Verify cookies cleared
               (let ((`#(ok ,empty-cookies ,_store6) (lhc-cookies:query store5 url)))
                 (line (test-pat 0 (length empty-cookies)))
                 'ok)))))))))
