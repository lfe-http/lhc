# LHC User Guide

Complete guide to using the Little (LFE) HTTP Client.

## Table of Contents

* [Getting Started](#getting-started)
* [Basic HTTP Requests](#basic-http-requests)
* [Request Options](#request-options)
* [Response Handling](#response-handling)
* [Async Operations](#async-operations)
* [Cookie Management](#cookie-management)
* [WebSocket](#websocket)
* [Error Handling](#error-handling)
* [Backends](#backends)
* [Advanced Usage](#advanced-usage)

## Getting Started

### Installation

Add LHC to your `rebar.config`:

```erlang
{deps, [
  {lhc, "2.0.0"}
]}.
```

Then compile:

```bash
rebar3 compile
```

### Your First Request

```lfe
;; Start the LFE REPL
(lhc:get "https://httpbin.org/get")
;; => {ok, #{status => 200, headers => [...], body => <<"...">>}}
```

## Basic HTTP Requests

### GET

```lfe
;; Simple GET
(lhc:get "https://api.example.com/users")

;; GET with query parameters (in URL)
(lhc:get "https://api.example.com/users?page=1&limit=10")

;; GET with custom headers
(let ((headers '(#(#"authorization" #"Bearer token123"))))
  (lhc:get "https://api.example.com/protected"
           (map 'headers headers)))
```

### POST

```lfe
;; POST with JSON body
(let ((url "https://api.example.com/users")
      (body #"{\"name\": \"Alice\", \"email\": \"alice@example.com\"}")
      (headers '(#(#"content-type" #"application/json")))
      (opts (map 'headers headers)))
  (lhc:post url body opts))

;; POST with form data
(let ((url "https://httpbin.org/post")
      (body #"name=Alice&email=alice@example.com")
      (headers '(#(#"content-type" #"application/x-www-form-urlencoded")))
      (opts (map 'headers headers)))
  (lhc:post url body opts))
```

### PUT

```lfe
;; PUT to update a resource
(let ((url "https://api.example.com/users/123")
      (body #"{\"name\": \"Alice Updated\"}")
      (headers '(#(#"content-type" #"application/json")))
      (opts (map 'headers headers)))
  (lhc:put url body opts))
```

### DELETE

```lfe
;; DELETE a resource
(lhc:delete "https://api.example.com/users/123")

;; DELETE with headers
(let ((headers '(#(#"authorization" #"Bearer token123"))))
  (lhc:delete "https://api.example.com/users/123"
              (map 'headers headers)))
```

### HEAD

```lfe
;; Check if resource exists (no body returned)
(lhc:head "https://api.example.com/users/123")
```

### OPTIONS

```lfe
;; Get allowed methods
(lhc:options "https://api.example.com/users")
```

### PATCH

```lfe
;; Partially update a resource
(let ((url "https://api.example.com/users/123")
      (body #"{\"email\": \"newemail@example.com\"}")
      (headers '(#(#"content-type" #"application/json")))
      (opts (map 'headers headers)))
  (lhc:patch url body opts))
```

## Request Options

All request functions accept an options map as the last parameter:

```lfe
(map 'backend 'gun           ; Backend to use (httpc or gun)
     'mode 'sync             ; Request mode (sync or async)
     'timeout 5000           ; Timeout in milliseconds
     'headers headers        ; Custom headers (list of tuples)
     'cookie-store store)    ; Cookie store (for gun backend)
```

### Specifying Backend

```lfe
;; Use httpc (default, no external dependencies)
(lhc:get url (map 'backend 'httpc))

;; Use gun (recommended for WebSocket and HTTP/2)
(lhc:get url (map 'backend 'gun))
```

### Setting Timeout

```lfe
;; Set 10 second timeout
(lhc:get url (map 'timeout 10000))

;; Different timeout for slow endpoint
(lhc:get "https://slow-api.example.com/data"
         (map 'timeout 30000))
```

### Custom Headers

```lfe
;; Single header
(let ((headers '(#(#"user-agent" #"LHC/2.0"))))
  (lhc:get url (map 'headers headers)))

;; Multiple headers
(let ((headers '(#(#"authorization" #"Bearer token123")
                 #(#"accept" #"application/json")
                 #(#"user-agent" #"LHC/2.0"))))
  (lhc:get url (map 'headers headers)))
```

## Response Handling

### Response Structure

All successful requests return `{ok, response-map}`:

```lfe
(case (lhc:get "https://httpbin.org/get")
  (`#(ok ,response)
   (let ((status (maps:get 'status response))
         (headers (maps:get 'headers response))
         (body (maps:get 'body response)))
     (io:format "Status: ~p~n" (list status))
     (io:format "Body: ~p~n" (list body))))
  (`#(error ,error)
   (io:format "Error: ~p~n" (list error))))
```

### Response Map Fields

* **status** - HTTP status code (integer)
* **headers** - Response headers (list of `{Name, Value}` tuples)
* **body** - Response body (binary)

### Parsing JSON Response

```lfe
;; Assuming you have a JSON library like jsx
(case (lhc:get "https://api.example.com/users")
  (`#(ok ,response)
   (let ((body (maps:get 'body response)))
     (jsx:decode body)))  ; Parse JSON
  (`#(error ,error)
   (io:format "Request failed: ~p~n" (list error))))
```

### Checking Status Code

```lfe
(case (lhc:get url)
  (`#(ok ,response)
   (case (maps:get 'status response)
     (200 (handle-success response))
     (404 (io:format "Not found~n"))
     (500 (io:format "Server error~n"))
     (_ (io:format "Unexpected status~n"))))
  (`#(error ,error)
   (handle-error error)))
```

### Extracting Headers

```lfe
(case (lhc:get url)
  (`#(ok ,response)
   (let ((headers (maps:get 'headers response)))
     ;; Find specific header
     (case (lists:keyfind #"content-type" 1 headers)
       (`#(,_name ,value)
        (io:format "Content-Type: ~p~n" (list value)))
       ('false
        (io:format "No Content-Type header~n")))))
  (`#(error ,error)
   (handle-error error)))
```

## Async Operations

Async mode allows non-blocking HTTP requests:

### Basic Async Request

```lfe
;; Start async request
(let ((`#(ok ,request-id) (lhc:get "https://httpbin.org/delay/5"
                                   (map 'mode 'async))))
  ;; Do other work while request is in progress
  (io:format "Request in progress, ID: ~p~n" (list request-id))

  ;; Wait for result when needed
  (case (lhc:await request-id 10000)
    (`#(ok ,response)
     (io:format "Got response: ~p~n" (list response)))
    (`#(error ,error)
     (io:format "Request failed: ~p~n" (list error)))))
```

### Multiple Concurrent Requests

```lfe
;; Start multiple async requests
(let ((urls '("https://api.example.com/users"
              "https://api.example.com/posts"
              "https://api.example.com/comments")))
  ;; Start all requests
  (let ((request-ids
         (lists:map
          (lambda (url)
            (let ((`#(ok ,req-id) (lhc:get url (map 'mode 'async))))
              `#(,url ,req-id)))
          urls)))

    ;; Collect all results
    (lists:map
     (lambda (url-and-id)
       (let ((`#(,url ,req-id) url-and-id))
         (case (lhc:await req-id 5000)
           (`#(ok ,response)
            (io:format "~s: OK~n" (list url)))
           (`#(error ,error)
            (io:format "~s: Failed~n" (list url))))))
     request-ids)))
```

### Async with Timeout

```lfe
;; Set different timeout for await
(let ((`#(ok ,req-id) (lhc:get url (map 'mode 'async))))
  (case (lhc:await req-id 2000)  ; 2 second timeout
    (`#(ok ,response) 'success)
    (`#(error ,error)
     ;; Could be timeout or other error
     (io:format "Error: ~p~n" (list error)))))
```

## Cookie Management

LHC supports automatic cookie handling with the gun backend.

### Automatic Cookies (Recommended)

```lfe
;; Initialize cookie store
(let ((store (lhc-cookies:init))
      (opts (map 'backend 'gun 'cookie-store store)))

  ;; Open connection with cookie store
  (let ((`#(ok ,conn) (lhc:open "https://example.com" opts)))

    ;; First request sets cookies
    (lhc-gun:do-request-sync conn 'GET "/login?user=alice" #"" '() (map))

    ;; Subsequent requests automatically send cookies
    (lhc-gun:do-request-sync conn 'GET "/dashboard" #"" '() (map))

    ;; Close connection
    (lhc:close conn)))
```

### Cookie Store Operations

```lfe
;; Initialize store
(let ((store (lhc-cookies:init)))

  ;; Store a cookie manually
  (let ((cookie (maps:from_list
                 '(#(name "session")
                   #(value "abc123")
                   #(domain "example.com")
                   #(path "/")))))
    (lhc-cookies:store store "https://example.com" cookie))

  ;; Query cookies for a URI
  (let ((cookies (lhc-cookies:query store "https://example.com/")))
    (io:format "Cookies: ~p~n" (list cookies)))

  ;; List all cookies
  (let ((all-cookies (lhc-cookies:list-all store)))
    (io:format "All cookies: ~p~n" (list all-cookies)))

  ;; Clear all cookies
  (lhc-cookies:clear store))
```

### Cookie Persistence

Cookies persist for the lifetime of the cookie store:

```lfe
(let ((store (lhc-cookies:init)))
  ;; Use the same store across multiple connections
  (let ((`#(ok ,conn1) (lhc:open "https://example.com"
                                (map 'backend 'gun 'cookie-store store))))
    (lhc-gun:do-request-sync conn1 'GET "/set-cookie" #"" '() (map))
    (lhc:close conn1))

  ;; Cookies available in new connection with same store
  (let ((`#(ok ,conn2) (lhc:open "https://example.com"
                                (map 'backend 'gun 'cookie-store store))))
    (lhc-gun:do-request-sync conn2 'GET "/need-cookie" #"" '() (map))
    (lhc:close conn2)))
```

## WebSocket

WebSocket support is available with the gun backend only.

### Basic WebSocket

```lfe
;; Connect to WebSocket server
(let ((`#(ok ,conn) (lhc:open "wss://echo.websocket.org"
                             (map 'backend 'gun))))

  ;; Upgrade to WebSocket
  (let ((`#(ok ,stream-ref) (lhc-websocket:upgrade conn "/" (map))))

    ;; Send text frame
    (lhc-websocket:send-text conn stream-ref #"Hello!")

    ;; Receive response
    (let ((`#(ok ,frame) (lhc-websocket:await-frame conn stream-ref 5000)))
      (io:format "Received: ~p~n" (list (maps:get 'data frame))))

    ;; Close connection
    (lhc:close conn)))
```

### WebSocket Frame Types

```lfe
;; Text frame
(lhc-websocket:send-text conn stream-ref #"Hello World!")

;; Binary frame
(lhc-websocket:send-binary conn stream-ref (binary 1 2 3 4 5))

;; Ping frame
(lhc-websocket:ping conn stream-ref)

;; Close frame
(lhc-websocket:close conn stream-ref
                     (map 'code 1000 'reason #"Normal close"))
```

### Async WebSocket

```lfe
;; Async upgrade
(let ((`#(ok ,conn) (lhc:open "wss://echo.websocket.org"
                             (map 'backend 'gun))))
  (let ((`#(ok ,stream-ref) (lhc-websocket:upgrade conn "/"
                                                   (map)
                                                   (map 'mode 'async))))
    ;; Wait for upgrade confirmation
    (receive
      (`#(gun_upgrade ,c ,s ,protocols ,headers)
       (when (=:= c conn) (=:= s stream-ref))
       (io:format "WebSocket upgraded!~n")
       ;; Now can send/receive frames
       (lhc-websocket:send-text conn stream-ref #"Hello!"))
      (after 5000
        (io:format "Upgrade timeout~n")))))
```

### Receiving WebSocket Frames

```lfe
;; Wait for frame with timeout
(case (lhc-websocket:await-frame conn stream-ref 5000)
  (`#(ok ,frame)
   (let ((type (maps:get 'type frame))
         (data (maps:get 'data frame 'undefined)))
     (case type
       ('text (io:format "Text: ~s~n" (list data)))
       ('binary (io:format "Binary: ~p~n" (list data)))
       ('close (io:format "Connection closed~n"))
       (_ (io:format "Other frame type~n")))))
  (`#(error ,error)
   (io:format "Error: ~p~n" (list error))))
```

## Error Handling

### Error Structure

Errors are returned as `{error, error-map}`:

```lfe
(case (lhc:get "http://invalid-domain-12345.com")
  (`#(ok ,response) ...)
  (`#(error ,error)
   (let ((reason (maps:get 'reason error))
         (message (maps:get 'message error))
         (backend (maps:get 'backend error)))
     (io:format "Error (~p): ~s~n" (list reason message)))))
```

### Error Categories

* **connection** - Network connection failures
* **timeout** - Request timeout
* **protocol** - HTTP protocol errors
* **ssl** - SSL/TLS errors
* **parse** - Response parsing errors
* **not-implemented** - Feature not supported by backend
* **unknown** - Other errors

### Handling Specific Errors

```lfe
(case (lhc:get url)
  (`#(ok ,response)
   (handle-success response))

  (`#(error ,error)
   (case (maps:get 'reason error)
     ('connection
      (io:format "Connection failed: ~s~n"
                 (list (maps:get 'message error))))
     ('timeout
      (io:format "Request timed out~n"))
     ('ssl
      (io:format "SSL error: ~s~n"
                 (list (maps:get 'message error))))
     (_
      (io:format "Unknown error: ~p~n" (list error))))))
```

### Formatting Errors

```lfe
;; Format error for display
(case (lhc:get url)
  (`#(ok ,response) ...)
  (`#(error ,error)
   (io:format "~s~n" (list (lhc-error:format error)))))
;; => "[GUN] CONNECTION: Domain name resolution failed"
```

## Backends

### Choosing a Backend

**Use httpc when:**

* You want zero external dependencies
* You only need simple HTTP/1.1
* You don't need WebSocket
* Performance is not critical

**Use gun when:**

* You need WebSocket support
* You want HTTP/2
* You need better performance
* You want automatic cookie handling

### Backend Comparison

| Feature | httpc | gun |
|---------|-------|-----|
| HTTP/1.1 | ✓ | ✓ |
| HTTP/2 | ✗ | ✓ |
| WebSocket | ✗ | ✓ |
| Cookies | Manual | Automatic |
| Dependencies | None | gun, cowlib |
| Performance | Good | Excellent |

### Backend-Specific Options

```lfe
;; gun-specific options
(lhc:get url (map 'backend 'gun
                  'gun-opts (map 'protocols '(http2 http)
                                 'retry 10)))

;; httpc-specific options
(lhc:get url (map 'backend 'httpc
                  'httpc-opts (map 'relaxed 'true)))
```

## Advanced Usage

### Connection Reuse (gun)

```lfe
;; Open connection
(let ((`#(ok ,conn) (lhc:open "https://api.example.com"
                             (map 'backend 'gun))))

  ;; Make multiple requests on same connection
  (lhc-gun:do-request-sync conn 'GET "/users" #"" '() (map))
  (lhc-gun:do-request-sync conn 'GET "/posts" #"" '() (map))
  (lhc-gun:do-request-sync conn 'GET "/comments" #"" '() (map))

  ;; Close when done
  (lhc:close conn))
```

### SSL/TLS Options

```lfe
;; Custom SSL options
(let ((ssl-opts (map 'verify 'verify_peer
                     'cacerts (public_key:cacerts_get))))
  (lhc:get "https://example.com"
           (map 'gun-opts (map 'transport_opts ssl-opts))))
```

### Streaming Responses

```lfe
;; For large responses, use gun directly for streaming
(let ((`#(ok ,conn) (lhc:open url (map 'backend 'gun))))
  (let ((stream-ref (gun:get conn "/" '())))
    ;; Handle response in chunks
    (receive
      (`#(gun_response ,c ,s ,'nofin ,status ,headers)
       (receive-body c s #""))
      (after 5000
        (io:format "Timeout~n")))))

(defun receive-body (conn stream-ref acc)
  (receive
    (`#(gun_data ,c ,s ,'nofin ,data)
     (receive-body conn stream-ref (binary acc data)))
    (`#(gun_data ,c ,s ,'fin ,data)
     (let ((body (binary acc data)))
       (io:format "Complete body: ~p~n" (list body))))))
```

### Custom User Agent

```lfe
(let ((headers '(#(#"user-agent" #"MyApp/1.0 (LHC/2.0)"))))
  (lhc:get url (map 'headers headers)))
```

## Best Practices

1. **Always handle errors**: Check for `{error, ...}` returns
2. **Set appropriate timeouts**: Default is 5 seconds
3. **Use gun for WebSocket**: httpc doesn't support WebSocket
4. **Reuse connections with gun**: More efficient than opening new connections
5. **Use async for multiple requests**: Improve performance with concurrent requests
6. **Initialize cookie store once**: Reuse across connections
7. **Close connections**: Always close connections when done (especially gun)

## Next Steps

* [API Reference](api-reference.md) - Complete API documentation
* [Examples](examples.md) - More code examples
* [Migration Guide](migration-guide.md) - Upgrading from 0.4.x
