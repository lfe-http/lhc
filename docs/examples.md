# LHC Examples

Practical code examples and patterns for using LHC.

## Table of Contents

* [Basic Requests](#basic-requests)
* [Working with JSON](#working-with-json)
* [Authentication](#authentication)
* [Async Patterns](#async-patterns)
* [Cookie Management](#cookie-management)
* [WebSocket Examples](#websocket-examples)
* [Error Handling](#error-handling)
* [Advanced Patterns](#advanced-patterns)

## Basic Requests

### Simple GET

```lfe
;; Basic GET request
(lhc:get "https://httpbin.org/get")
;; => {ok, #{status => 200, headers => [...], body => <<"...">>}}
```

### GET with Query Parameters

```lfe
;; Build URL with query string
(let ((base-url "https://api.example.com/search")
      (query "?q=lfe&limit=10&offset=0")
      (url (list_to_binary (++ base-url query))))
  (lhc:get url))

;; Or construct programmatically
(let* ((params '(#("q" "lfe") #("limit" "10") #("offset" "0")))
       (query-string (lists:join "&"
                       (lists:map
                        (lambda (p)
                          (let ((`#(,k ,v) p))
                            (++ k "=" v)))
                        params)))
       (url (iolist_to_binary
             (list "https://api.example.com/search?" query-string))))
  (lhc:get url))
```

### POST with Form Data

```lfe
(let ((url "https://httpbin.org/post")
      (body #"username=alice&password=secret123")
      (headers '(#(#"content-type" #"application/x-www-form-urlencoded")))
      (opts (map 'headers headers)))
  (lhc:post url body opts))
```

### PUT to Update Resource

```lfe
(let ((url "https://api.example.com/users/42")
      (body #"{\"email\": \"newemail@example.com\"}")
      (headers '(#(#"content-type" #"application/json")
                 #(#"authorization" #"Bearer token123")))
      (opts (map 'headers headers)))
  (lhc:put url body opts))
```

## Working with JSON

### Sending JSON

```lfe
;; Using jsx for JSON encoding
(defun create-post (title content)
  (let* ((url "https://jsonplaceholder.typicode.com/posts")
         (data (map 'title title 'body content 'userId 1))
         (body (jsx:encode data))
         (headers '(#(#"content-type" #"application/json")))
         (opts (map 'headers headers)))
    (case (lhc:post url body opts)
      (`#(ok ,response)
       (let ((status (maps:get 'status response)))
         (if (=:= status 201)
           `#(ok ,(jsx:decode (maps:get 'body response)))
           `#(error ,status))))
      (`#(error ,error)
       `#(error ,error)))))

;; Usage
(create-post "My Title" "Post content here")
```

### Parsing JSON Response

```lfe
(defun fetch-user (user-id)
  (let ((url (iolist_to_binary
              (list "https://jsonplaceholder.typicode.com/users/"
                    (integer_to_list user-id)))))
    (case (lhc:get url)
      (`#(ok ,response)
       (case (maps:get 'status response)
         (200
          (let ((body (maps:get 'body response)))
            `#(ok ,(jsx:decode body))))
         (404
          `#(error not-found))
         (_
          `#(error ,(maps:get 'status response)))))
      (`#(error ,error)
       `#(error ,error)))))

;; Usage
(case (fetch-user 1)
  (`#(ok ,user-data)
   (io:format "User: ~p~n" (list user-data)))
  (`#(error ,reason)
   (io:format "Error: ~p~n" (list reason))))
```

## Authentication

### Bearer Token

```lfe
(defun api-request (endpoint token)
  (let ((url (++ "https://api.example.com" endpoint))
        (headers `(#(#"authorization"
                     ,(list_to_binary (++ "Bearer " token)))))
        (opts (map 'headers headers)))
    (lhc:get url opts)))

;; Usage
(api-request "/me" "eyJhbGciOiJIUzI1NiIs...")
```

### Basic Auth

```lfe
(defun basic-auth (username password)
  (let* ((credentials (++ username ":" password))
         (encoded (base64:encode_to_string credentials))
         (auth-header (++ "Basic " encoded)))
    `#(#"authorization" ,(list_to_binary auth-header))))

(defun authenticated-request (url username password)
  (let ((headers (list (basic-auth username password)))
        (opts (map 'headers headers)))
    (lhc:get url opts)))

;; Usage
(authenticated-request "https://api.example.com/protected"
                      "alice" "secret123")
```

### API Key

```lfe
(defun api-key-request (url api-key)
  (let ((headers `(#(#"x-api-key" ,(list_to_binary api-key))))
        (opts (map 'headers headers)))
    (lhc:get url opts)))

;; Usage
(api-key-request "https://api.example.com/data"
                 "sk_live_abc123xyz...")
```

## Async Patterns

### Single Async Request

```lfe
(defun fetch-data-async (url)
  ;; Start async request
  (let ((`#(ok ,req-id) (lhc:get url (map 'mode 'async))))
    ;; Do other work
    (io:format "Request started: ~p~n" (list req-id))
    (io:format "Doing other work...~n")

    ;; Wait for result
    (case (lhc:await req-id 5000)
      (`#(ok ,response)
       `#(ok ,(maps:get 'body response)))
      (`#(error ,error)
       `#(error ,error)))))
```

### Multiple Concurrent Requests

```lfe
(defun fetch-all-users ()
  (let ((user-ids '(1 2 3 4 5))
        (base-url "https://jsonplaceholder.typicode.com/users/"))

    ;; Start all requests
    (let ((requests
           (lists:map
            (lambda (id)
              (let ((url (iolist_to_binary
                         (list base-url (integer_to_list id)))))
                (case (lhc:get url (map 'mode 'async))
                  (`#(ok ,req-id) `#(,id ,req-id))
                  (`#(error ,error) `#(,id error ,error)))))
            user-ids)))

      ;; Collect all results
      (lists:map
       (lambda (req-info)
         (case req-info
           (`#(,id ,req-id)
            (case (lhc:await req-id 5000)
              (`#(ok ,response)
               `#(,id ok ,(jsx:decode (maps:get 'body response))))
              (`#(error ,error)
               `#(,id error ,error))))
           (`#(,id error ,error)
            `#(,id error ,error))))
       requests))))

;; Usage
(fetch-all-users)
;; => [(1 ok #{...}), (2 ok #{...}), ...]
```

### Parallel Requests with Timeout

```lfe
(defun parallel-fetch (urls)
  ;; Start all async requests
  (let ((request-ids
         (lists:map
          (lambda (url)
            (case (lhc:get url (map 'mode 'async 'timeout 5000))
              (`#(ok ,req-id) `#(,url ,req-id))
              (`#(error ,error) `#(,url error ,error))))
          urls)))

    ;; Collect results with individual timeouts
    (lists:map
     (lambda (req-info)
       (case req-info
         (`#(,url ,req-id)
          (case (lhc:await req-id 10000)
            (`#(ok ,response)
             `#(,url ,(maps:get 'status response)))
            (`#(error ,error)
             `#(,url error ,(maps:get 'reason error)))))
         (`#(,url error ,error)
          `#(,url error ,error))))
     request-ids)))
```

## Cookie Management

### Automatic Cookie Handling

```lfe
(defun login-and-fetch-profile ()
  ;; Initialize cookie store
  (let ((store (lhc-cookies:init))
        (opts (map 'backend 'gun 'cookie-store store)))

    ;; Open connection
    (let ((`#(ok ,conn) (lhc:open "https://example.com" opts)))

      ;; Login (sets session cookie)
      (let ((login-data #"{\"username\":\"alice\",\"password\":\"secret\"}")
            (headers '(#(#"content-type" #"application/json"))))
        (lhc-gun:do-request-sync conn 'POST "/api/login"
                                login-data headers (map)))

      ;; Fetch profile (cookie sent automatically)
      (let ((`#(ok ,response) (lhc-gun:do-request-sync conn 'GET
                                                      "/api/profile"
                                                      #"" '() (map))))
        ;; Close connection
        (lhc:close conn)
        `#(ok ,(maps:get 'body response))))))
```

### Inspecting Cookies

```lfe
(defun inspect-cookies (store uri)
  (case (lhc-cookies:query store uri)
    (`#(ok ,cookies ,new-store)
     (lists:foreach
      (lambda (cookie)
        (io:format "Cookie: ~s = ~s~n"
                   (list (maps:get 'name cookie)
                         (maps:get 'value cookie))))
      cookies)
     new-store)
    (`#(error ,error)
     (io:format "Error: ~p~n" (list error))
     store)))
```

### Cookie Persistence Across Connections

```lfe
(defun session-with-cookies ()
  ;; Initialize store
  (let ((store (lhc-cookies:init)))

    ;; First request (get cookie)
    (let ((opts1 (map 'backend 'gun 'cookie-store store)))
      (let ((`#(ok ,conn1) (lhc:open "https://httpbin.org" opts1)))
        (lhc-gun:do-request-sync conn1 'GET "/cookies/set?session=abc123"
                                #"" '() (map))
        (lhc:close conn1)))

    ;; Second request (cookie sent automatically)
    (let ((opts2 (map 'backend 'gun 'cookie-store store)))
      (let ((`#(ok ,conn2) (lhc:open "https://httpbin.org" opts2)))
        (let ((`#(ok ,response) (lhc-gun:do-request-sync conn2 'GET
                                                        "/cookies"
                                                        #"" '() (map))))
          (lhc:close conn2)
          (maps:get 'body response))))))
```

## WebSocket Examples

### Basic WebSocket Echo

```lfe
(defun websocket-echo (message)
  ;; Connect to WebSocket server
  (let ((`#(ok ,conn) (lhc:open "wss://echo.websocket.org"
                               (map 'backend 'gun))))

    ;; Upgrade to WebSocket
    (case (lhc-websocket:upgrade conn "/" (map))
      (`#(ok ,stream-ref)
       ;; Send message
       (lhc-websocket:send-text conn stream-ref message)

       ;; Wait for echo
       (case (lhc-websocket:await-frame conn stream-ref 5000)
         (`#(ok ,frame)
          (let ((data (maps:get 'data frame)))
            (lhc:close conn)
            `#(ok ,data)))
         (`#(error ,error)
          (lhc:close conn)
          `#(error ,error))))

      (`#(error ,error)
       (lhc:close conn)
       `#(error ,error)))))

;; Usage
(websocket-echo #"Hello WebSocket!")
```

### WebSocket Chat Client

```lfe
(defmodule ws-chat
  (export (connect 2) (send-message 2) (receive-loop 2)))

(defun connect (server-url username)
  (case (lhc:open server-url (map 'backend 'gun))
    (`#(ok ,conn)
     (case (lhc-websocket:upgrade conn "/" (map 'mode 'async))
       (`#(ok ,stream-ref)
        ;; Wait for upgrade confirmation
        (receive
          (`#(gun_upgrade ,c ,s ,_ ,_)
           (when (=:= c conn) (=:= s stream-ref))
           ;; Send join message
           (let ((join-msg (jsx:encode
                           (map 'type "join" 'username username))))
             (lhc-websocket:send-text conn stream-ref join-msg))
           `#(ok ,conn ,stream-ref))
          (after 5000
            (lhc:close conn)
            `#(error timeout))))
       (`#(error ,error)
        (lhc:close conn)
        `#(error ,error))))
    (`#(error ,error)
     `#(error ,error))))

(defun send-message (state message)
  (let ((`#(,conn ,stream-ref) state))
    (let ((msg (jsx:encode (map 'type "message" 'text message))))
      (lhc-websocket:send-text conn stream-ref msg))))

(defun receive-loop (state handler-fn)
  (let ((`#(,conn ,stream-ref) state))
    (case (lhc-websocket:await-frame conn stream-ref 60000)
      (`#(ok ,frame)
       (case (maps:get 'type frame)
         ('text
          (let ((data (jsx:decode (maps:get 'data frame))))
            (funcall handler-fn data)
            (receive-loop state handler-fn)))
         ('close
          (io:format "Connection closed~n")
          (lhc:close conn))
         (_
          (receive-loop state handler-fn))))
      (`#(error ,error)
       (io:format "Error: ~p~n" (list error))
       (lhc:close conn)))))
```

### WebSocket with Ping/Pong

```lfe
(defun websocket-with-heartbeat ()
  (let ((`#(ok ,conn) (lhc:open "wss://echo.websocket.org"
                               (map 'backend 'gun))))
    (case (lhc-websocket:upgrade conn "/" (map))
      (`#(ok ,stream-ref)
       ;; Send ping
       (lhc-websocket:ping conn stream-ref)

       ;; Send actual message
       (lhc-websocket:send-text conn stream-ref #"Hello!")

       ;; Receive message (pong handled automatically)
       (case (lhc-websocket:await-frame conn stream-ref 5000)
         (`#(ok ,frame)
          (lhc:close conn)
          `#(ok ,(maps:get 'data frame)))
         (`#(error ,error)
          (lhc:close conn)
          `#(error ,error))))

      (`#(error ,error)
       (lhc:close conn)
       `#(error ,error)))))
```

## Error Handling

### Comprehensive Error Handling

```lfe
(defun safe-request (url)
  (case (lhc:get url (map 'timeout 10000))
    (`#(ok ,response)
     (case (maps:get 'status response)
       (200 `#(ok ,(maps:get 'body response)))
       (404 `#(error not-found))
       (401 `#(error unauthorized))
       (500 `#(error server-error))
       (status `#(error #(unexpected-status ,status)))))

    (`#(error ,error)
     (case (maps:get 'reason error)
       ('connection
        `#(error connection-failed))
       ('timeout
        `#(error request-timeout))
       ('ssl
        `#(error ssl-error))
       (_
        `#(error #(unknown ,(lhc-error:format error))))))))
```

### Retry Logic

```lfe
(defun retry-request (url max-retries)
  (retry-request url max-retries 0))

(defun retry-request (url max-retries attempt)
  (case (lhc:get url)
    (`#(ok ,response) `#(ok ,response))
    (`#(error ,error)
     (if (< attempt max-retries)
       (progn
         (io:format "Retry ~p/~p: ~s~n"
                    (list (+ attempt 1) max-retries
                          (lhc-error:format error)))
         (timer:sleep (* 1000 (+ attempt 1)))  ; Exponential backoff
         (retry-request url max-retries (+ attempt 1)))
       `#(error #(max-retries-exceeded ,error))))))

;; Usage
(retry-request "https://unreliable-api.example.com/data" 3)
```

### Error Recovery

```lfe
(defun fetch-with-fallback (primary-url fallback-url)
  (case (lhc:get primary-url)
    (`#(ok ,response) `#(ok primary ,response))
    (`#(error ,error1)
     (io:format "Primary failed: ~s~n" (list (lhc-error:format error1)))
     (io:format "Trying fallback...~n")
     (case (lhc:get fallback-url)
       (`#(ok ,response) `#(ok fallback ,response))
       (`#(error ,error2)
        `#(error #(both-failed ,error1 ,error2)))))))
```

## Advanced Patterns

### Connection Pooling (Gun)

```lfe
(defmodule api-client
  (export (start 1) (request 2) (stop 1)))

(defun start (base-url)
  ;; Open persistent connection
  (case (lhc:open base-url (map 'backend 'gun))
    (`#(ok ,conn)
     `#(ok ,conn))
    (`#(error ,error)
     `#(error ,error))))

(defun request (conn path)
  ;; Reuse connection for multiple requests
  (lhc-gun:do-request-sync conn 'GET path #"" '() (map)))

(defun stop (conn)
  (lhc:close conn))

;; Usage
(let ((`#(ok ,conn) (api-client:start "https://api.example.com")))
  (api-client:request conn "/users")
  (api-client:request conn "/posts")
  (api-client:request conn "/comments")
  (api-client:stop conn))
```

### Streaming Large Responses

```lfe
(defun stream-to-file (url filename)
  (case (lhc:get url (map 'backend 'gun
                         'mode 'async
                         'stream-to filename))
    (`#(ok ,req-id)
     ;; Response will be streamed to file
     (lhc:await req-id 30000))
    (`#(error ,error)
     `#(error ,error))))
```

### Custom User Agent

```lfe
(defun request-with-user-agent (url)
  (let ((headers '(#(#"user-agent" #"MyApp/1.0 (LHC/2.0)")))
        (opts (map 'headers headers)))
    (lhc:get url opts)))
```

### Rate Limiting

```lfe
(defmodule rate-limiter
  (export (init 1) (request 2)))

(defun init (requests-per-second)
  (map 'interval (div 1000 requests-per-second)
       'last-request (erlang:system_time 'millisecond)))

(defun request (limiter url)
  (let ((now (erlang:system_time 'millisecond))
        (last (maps:get 'last-request limiter))
        (interval (maps:get 'interval limiter))
        (elapsed (- now last)))

    (if (< elapsed interval)
      (timer:sleep (- interval elapsed))
      'ok)

    (let ((result (lhc:get url))
          (new-limiter (maps:put 'last-request
                                (erlang:system_time 'millisecond)
                                limiter)))
      `#(,result ,new-limiter))))

;; Usage
(let ((limiter (rate-limiter:init 10)))  ; 10 requests/second
  (let ((`#(,result1 ,limiter2) (rate-limiter:request limiter url1)))
    (let ((`#(,result2 ,limiter3) (rate-limiter:request limiter2 url2)))
      ;; ... more requests
      'ok)))
```

### Building a REST Client

```lfe
(defmodule rest-client
  (export (get 2) (post 3) (put 3) (delete 2)))

(defun make-url (base-path endpoint)
  (iolist_to_binary (list base-path endpoint)))

(defun default-headers ()
  '(#(#"content-type" #"application/json")
    #(#"accept" #"application/json")))

(defun get (base-path endpoint)
  (let ((url (make-url base-path endpoint))
        (opts (map 'headers (default-headers))))
    (case (lhc:get url opts)
      (`#(ok ,response)
       `#(ok ,(jsx:decode (maps:get 'body response))))
      (`#(error ,error)
       `#(error ,error)))))

(defun post (base-path endpoint data)
  (let ((url (make-url base-path endpoint))
        (body (jsx:encode data))
        (opts (map 'headers (default-headers))))
    (case (lhc:post url body opts)
      (`#(ok ,response)
       `#(ok ,(jsx:decode (maps:get 'body response))))
      (`#(error ,error)
       `#(error ,error)))))

(defun put (base-path endpoint data)
  (let ((url (make-url base-path endpoint))
        (body (jsx:encode data))
        (opts (map 'headers (default-headers))))
    (case (lhc:put url body opts)
      (`#(ok ,response)
       `#(ok ,(jsx:decode (maps:get 'body response))))
      (`#(error ,error)
       `#(error ,error)))))

(defun delete (base-path endpoint)
  (let ((url (make-url base-path endpoint))
        (opts (map 'headers (default-headers))))
    (case (lhc:delete url opts)
      (`#(ok ,response)
       `#(ok ,(maps:get 'status response)))
      (`#(error ,error)
       `#(error ,error)))))

;; Usage
(let ((api-base "https://jsonplaceholder.typicode.com"))
  (rest-client:get api-base "/posts/1")
  (rest-client:post api-base "/posts"
                   (map 'title "New Post" 'body "Content"))
  (rest-client:put api-base "/posts/1"
                  (map 'title "Updated"))
  (rest-client:delete api-base "/posts/1"))
```

## See Also

* [User Guide](user-guide.md) - Complete usage guide
* [API Reference](api-reference.md) - Detailed API documentation
* [Migration Guide](migration-guide.md) - Upgrading from 0.4.x
