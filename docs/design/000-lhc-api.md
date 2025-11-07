# LHC (Lisp HTTP Client) - Unified API Documentation

## Overview

LHC provides a unified Lisp-friendly API for HTTP client operations, supporting both Erlang's `httpc` and 99s' `gun` as backend implementations. The library automatically dispatches to synchronous or asynchronous implementations based on request options.

## Architecture

### Module Organization

```
lhc/
├── lhc.lfe                    # Public API (dispatches to sync/async)
├── lhc-sync.lfe              # Synchronous operations
├── lhc-async.lfe             # Asynchronous operations
├── lhc-httpc.lfe             # httpc backend implementation
├── lhc-gun.lfe               # gun backend implementation
├── lhc-cookies.lfe           # Cookie handling (wraps gun-cookies)
├── lhc-websocket.lfe         # WebSocket support (gun only)
├── lhc-util.lfe              # Shared utilities
└── lhc-error.lfe             # Error normalization
```

## Core Types

### Common Types

```lisp
;; Backend selection
(deftype backend () '(httpc gun))

;; Operation mode (inferred from options)
(deftype mode () '(sync async))

;; HTTP methods
(deftype method () 
  '(get post put patch delete head options trace connect))

;; Request/Response types
(deftype headers () 
  '(list (tuple binary binary)))

(deftype body () 
  '(union binary string iolist file-path))

(deftype status-code () 
  'integer)

(deftype response ()
  '(tuple ok (map status status-code
                  headers headers
                  body binary)))

(deftype error-response ()
  '(tuple error (map reason atom
                     message binary
                     details any)))

;; URI types
(deftype uri () 'binary)
(deftype url () 'binary)  ;; alias for uri

;; Connection reference (backend-specific)
(deftype conn-ref () 
  '(union pid         ;; gun connection
          profile))   ;; httpc profile (atom)

;; Stream reference (gun-specific, undefined for httpc)
(deftype stream-ref () 
  '(union reference undefined))

;; Request reference (for async operations)
(deftype request-id () 
  '(union reference undefined))
```

### Options Maps

```lisp
;; Top-level request options
(deftype request-opts ()
  '(map backend backend              ;; default: httpc
        mode mode                     ;; default: sync
        timeout timeout              ;; default: 5000 (ms)
        connect-timeout timeout      ;; default: 5000 (ms)
        follow-redirects boolean     ;; default: true
        max-redirects integer        ;; default: 5
        headers headers              ;; default: []
        ;; Async-specific
        receiver pid                 ;; default: (self)
        stream-to stream-destination ;; for streaming responses
        ;; Backend-specific configs
        httpc-opts httpc-opts
        gun-opts gun-opts))

;; httpc backend options
(deftype httpc-opts ()
  '(map profile atom                 ;; default: default
        ssl-opts (list tuple)        ;; SSL options
        autoredirect boolean         ;; default: true
        proxy-auth (tuple string string)
        version http-version         ;; default: 'HTTP/1.1
        relaxed boolean              ;; default: false
        url-encode boolean))         ;; default: true

;; gun backend options  
(deftype gun-opts ()
  '(map protocols (list protocol)    ;; default: [http2, http]
        transport transport          ;; default: tls for 443, tcp otherwise
        tls-opts (list tuple)        ;; TLS options
        tcp-opts (list tuple)        ;; TCP options
        http-opts http-opts
        http2-opts http2-opts
        ws-opts ws-opts
        retry integer                ;; default: 5
        retry-timeout integer        ;; default: 5000 (ms)
        cookie-store cookie-store))  ;; default: undefined

;; gun HTTP/1.1 options
(deftype http-opts ()
  '(map keepalive timeout            ;; default: infinity
        version http-version         ;; default: 'HTTP/1.1
        closing-timeout timeout      ;; default: 15000
        transform-header-name fun))  ;; header name transformer

;; gun HTTP/2 options
(deftype http2-opts ()
  '(map keepalive timeout            ;; default: infinity
        keepalive-tolerance integer  ;; default: disabled
        closing-timeout timeout      ;; default: 15000
        notify-settings-changed boolean  ;; default: false
        ;; Flow control options
        initial-connection-window-size integer
        initial-stream-window-size integer
        max-frame-size-received integer
        max-frame-size-sent integer))

;; gun WebSocket options
(deftype ws-opts ()
  '(map compress boolean             ;; default: false
        closing-timeout timeout      ;; default: 15000
        keepalive timeout            ;; default: infinity
        default-protocol module      ;; default: gun-ws-h
        protocols (list (tuple binary module))
        silence-pings boolean))      ;; default: true

;; Stream destinations for async responses
(deftype stream-destination ()
  '(union (tuple self)               ;; stream to calling process
          (tuple self once)          ;; stream one chunk at a time
          (tuple pid pid)            ;; stream to specific process
          (tuple file string)        ;; stream to file
          none))                     ;; no streaming

;; Cookie store (gun-specific)
(deftype cookie-store ()
  '(tuple module any))               ;; {module, state}

;; Transport types
(deftype transport () '(tcp tls))

;; Protocol types
(deftype protocol () '(http http2 raw socks websocket))

;; HTTP version
(deftype http-version () '(HTTP/1.0 HTTP/1.1))
```

## Public API (`lhc` module)

### Configuration & Lifecycle

#### `(start)`
Start the lhc application (starts both httpc and gun dependencies).

**Returns:** `(tuple 'ok (list atom))` - List of started applications

**Example:**
```lisp
(lhc:start)
;; => {ok, [crypto, asn1, public_key, ssl, inets, gun, lhc]}
```

---

#### `(stop)`
Stop the lhc application.

**Returns:** `'ok`

---

#### `(open uri options)`
Open a connection to a server (explicit connection management).

**Args:**
- `uri`: `uri()` - Server URI (e.g., "https://api.example.com")
- `options`: `request-opts()` - Connection options (requires `backend` key)

**Returns:** 
- `(tuple 'ok conn-ref)` - Connection reference
- `error-response()` - On failure

**Notes:**
- Required for gun backend
- Optional for httpc (connection pool is automatic)
- Gun connections must be explicitly closed with `close/1`

**Example:**
```lisp
;; Gun connection
(let ((opts (map 'backend 'gun
                 'gun-opts (map 'protocols '(http2)))))
  (lhc:open "https://api.example.com" opts))
;; => {ok, <0.234.0>}

;; httpc connection (creates/uses profile)
(let ((opts (map 'backend 'httpc
                 'httpc-opts (map 'profile 'my-api))))
  (lhc:open "https://api.example.com" opts))
;; => {ok, my-api}
```

---

#### `(close conn-ref)`
Close a connection.

**Args:**
- `conn-ref`: `conn-ref()` - Connection reference from `open/2`

**Returns:** `'ok`

**Example:**
```lisp
(lhc:close conn)
;; => ok
```

---

#### `(shutdown conn-ref)`
Gracefully shutdown a connection (gun-specific, allows in-flight requests to complete).

**Args:**
- `conn-ref`: `conn-ref()` - Connection reference

**Returns:** `'ok`

**Notes:**
- For httpc backend, this is equivalent to `close/1`
- For gun, allows pending requests to complete before closing

**Example:**
```lisp
(lhc:shutdown conn)
;; => ok
```

---

### HTTP Request Methods

#### `(get uri options)`
Perform HTTP GET request.

**Args:**
- `uri`: `uri()` - Request URI
- `options`: `request-opts()` - Request options

**Returns:**
- Sync mode: `response()` or `error-response()`
- Async mode: `(tuple 'ok request-id)`

**Example:**
```lisp
;; Synchronous GET
(lhc:get "https://api.example.com/users" 
         (map 'mode 'sync
              'headers '(#("Accept" "application/json"))))
;; => {ok, #{status => 200, headers => [...], body => <<"...">>}}

;; Asynchronous GET
(lhc:get "https://api.example.com/users"
         (map 'mode 'async
              'receiver (self)))
;; => {ok, #Ref<0.1234.0>}
;; Later: receive {http, {#Ref<0.1234.0>, response, ...}}
```

---

#### `(head uri options)`
Perform HTTP HEAD request.

**Args:**
- `uri`: `uri()` - Request URI  
- `options`: `request-opts()` - Request options

**Returns:**
- Sync mode: `response()` (with empty body) or `error-response()`
- Async mode: `(tuple 'ok request-id)`

---

#### `(post uri body options)`
Perform HTTP POST request.

**Args:**
- `uri`: `uri()` - Request URI
- `body`: `body()` - Request body
- `options`: `request-opts()` - Request options (should include Content-Type in headers)

**Returns:**
- Sync mode: `response()` or `error-response()`
- Async mode: `(tuple 'ok request-id)`

**Example:**
```lisp
(lhc:post "https://api.example.com/users"
          (binary "{\"name\": \"Alice\"}")
          (map 'mode 'sync
               'headers '(#("Content-Type" "application/json"))))
;; => {ok, #{status => 201, headers => [...], body => <<"...">>}}
```

---

#### `(put uri body options)`
Perform HTTP PUT request.

**Args:**
- `uri`: `uri()` - Request URI
- `body`: `body()` - Request body
- `options`: `request-opts()` - Request options

**Returns:**
- Sync mode: `response()` or `error-response()`
- Async mode: `(tuple 'ok request-id)`

---

#### `(patch uri body options)`
Perform HTTP PATCH request.

**Args:**
- `uri`: `uri()` - Request URI
- `body`: `body()` - Request body
- `options`: `request-opts()` - Request options

**Returns:**
- Sync mode: `response()` or `error-response()`
- Async mode: `(tuple 'ok request-id)`

---

#### `(delete uri options)`
Perform HTTP DELETE request.

**Args:**
- `uri`: `uri()` - Request URI
- `options`: `request-opts()` - Request options

**Returns:**
- Sync mode: `response()` or `error-response()`
- Async mode: `(tuple 'ok request-id)`

---

#### `(options uri options)`
Perform HTTP OPTIONS request.

**Args:**
- `uri`: `uri()` - Request URI
- `options`: `request-opts()` - Request options

**Returns:**
- Sync mode: `response()` or `error-response()`
- Async mode: `(tuple 'ok request-id)`

---

#### `(request method uri body headers options)`
Generic HTTP request function.

**Args:**
- `method`: `method()` - HTTP method
- `uri`: `uri()` - Request URI
- `body`: `body()` - Request body (can be empty binary for GET/HEAD)
- `headers`: `headers()` - Request headers
- `options`: `request-opts()` - Request options

**Returns:**
- Sync mode: `response()` or `error-response()`
- Async mode: `(tuple 'ok request-id)`

**Example:**
```lisp
(lhc:request 'post 
             "https://api.example.com/data"
             (binary "{\"key\": \"value\"}")
             '(#("Content-Type" "application/json")
               #("Authorization" "Bearer token123"))
             (map 'mode 'sync 'backend 'gun))
```

---

#### `(request-with-conn conn-ref method uri body headers options)`
Make a request using an existing connection.

**Args:**
- `conn-ref`: `conn-ref()` - Connection reference from `open/2`
- `method`: `method()` - HTTP method
- `uri`: `uri()` - Request URI (path only for gun, full URI for httpc)
- `body`: `body()` - Request body
- `headers`: `headers()` - Request headers
- `options`: `request-opts()` - Request options

**Returns:**
- Sync mode: `response()` or `error-response()`
- Async mode: `(tuple 'ok request-id stream-ref)`

**Notes:**
- For gun: URI should be path only (e.g., "/api/users")
- For httpc: Can use full URI or path (profile determines host)

**Example:**
```lisp
(let* (((tuple 'ok conn) (lhc:open "https://api.example.com" opts))
       ((tuple 'ok req-id stream) 
        (lhc:request-with-conn conn 'get "/users" (binary) '() 
                               (map 'mode 'async))))
  ;; ... handle async response ...
  (lhc:close conn))
```

---

### Async Response Handling

#### `(await request-id timeout)`
Wait for an async response (synchronously).

**Args:**
- `request-id`: `request-id()` - Request ID from async request
- `timeout`: `timeout()` - Maximum time to wait (ms)

**Returns:**
- `response()` - Full response
- `error-response()` - On timeout or error

**Notes:**
- Converts async operation to synchronous by blocking
- Useful for testing or controlled async workflows

**Example:**
```lisp
(let (((tuple 'ok req-id) (lhc:get uri (map 'mode 'async))))
  (lhc:await req-id 10000))
;; => {ok, #{status => 200, ...}}
```

---

#### `(await-body request-id timeout)`
Wait specifically for response body (after headers received).

**Args:**
- `request-id`: `request-id()` - Request ID
- `timeout`: `timeout()` - Maximum time to wait

**Returns:**
- `(tuple 'ok binary)` - Response body
- `error-response()` - On timeout or error

---

#### `(cancel request-id)`
Cancel an in-flight async request.

**Args:**
- `request-id`: `request-id()` - Request ID to cancel

**Returns:** `'ok`

**Notes:**
- Best effort - may not prevent response if already sent
- Gun: Sends RST_STREAM for HTTP/2
- httpc: Stops waiting for response

**Example:**
```lisp
(let (((tuple 'ok req-id) (lhc:get uri (map 'mode 'async))))
  ;; Changed our mind
  (lhc:cancel req-id))
```

---

#### `(flush request-id)`
Flush all messages related to a request from the mailbox.

**Args:**
- `request-id`: `request-id()` - Request ID

**Returns:** `'ok`

**Notes:**
- Useful for cleaning up after cancelled requests
- Removes gun_response, gun_data, etc. messages

---

### Streaming

#### `(stream-body conn-ref stream-ref data options)`
Stream request body data (for chunked uploads).

**Args:**
- `conn-ref`: `conn-ref()` - Connection reference
- `stream-ref`: `stream-ref()` - Stream reference (gun only)
- `data`: `(union body 'eof)` - Data chunk or EOF marker
- `options`: `request-opts()` - Options

**Returns:**
- `'ok` - Data sent
- `error-response()` - On failure

**Notes:**
- For chunked transfer encoding
- httpc: Limited support (may require full body upfront)
- gun: Full streaming support

**Example:**
```lisp
;; Initiate request with headers
(let* (((tuple 'ok conn) (lhc:open uri opts))
       ((tuple 'ok req-id stream) 
        (lhc:request-with-conn conn 'post "/upload" 'stream
                               '(#("Content-Type" "application/octet-stream"))
                               (map 'mode 'async))))
  ;; Stream data chunks
  (lhc:stream-body conn stream (binary "chunk1") opts)
  (lhc:stream-body conn stream (binary "chunk2") opts)
  (lhc:stream-body conn stream 'eof opts))
```

---

#### `(update-flow conn-ref stream-ref increment)`
Update flow control window for a stream (gun HTTP/2 only).

**Args:**
- `conn-ref`: `conn-ref()` - Connection reference
- `stream-ref`: `stream-ref()` - Stream reference
- `increment`: `integer()` - Flow control increment

**Returns:** `'ok`

**Notes:**
- Gun HTTP/2 only
- Returns `{error, #{reason => not-implemented, ...}}` for httpc
- Used for manual flow control management

---

### Connection Information

#### `(info conn-ref)`
Get information about a connection.

**Args:**
- `conn-ref`: `conn-ref()` - Connection reference

**Returns:**
- `(tuple 'ok (map))` - Connection info map
- `error-response()` - If connection invalid

**Info Map Keys:**
- `backend`: `backend()` - Which backend (gun/httpc)
- `transport`: `transport()` - tcp or tls
- `protocol`: `protocol()` - Current protocol
- `host`: `binary()` - Server host
- `port`: `integer()` - Server port
- `state`: `atom()` - Connection state (gun-specific)
- Gun-specific: `origin-host`, `origin-port`, `intermediaries`
- httpc-specific: `profile`, `pipeline-length`, `queue-length`

**Example:**
```lisp
(lhc:info conn)
;; => {ok, #{backend => gun,
;;           transport => tls,
;;           protocol => http2,
;;           host => <<"api.example.com">>,
;;           port => 443,
;;           state => connected}}
```

---

#### `(stream-info conn-ref stream-ref)`
Get information about a specific stream (gun only).

**Args:**
- `conn-ref`: `conn-ref()` - Connection reference
- `stream-ref`: `stream-ref()` - Stream reference

**Returns:**
- `(tuple 'ok (map))` - Stream info
- `error-response()` - If stream doesn't exist or httpc backend

**Stream Info Keys:**
- `ref`: `stream-ref()` - Stream reference
- `reply-to`: `pid()` - Process receiving responses
- `state`: `atom()` - Stream state (running, stopping, etc.)
- Gun-specific: `intermediaries`, `tunnel`

---

#### `(ping conn-ref)`
Send a ping to check connection health.

**Args:**
- `conn-ref`: `conn-ref()` - Connection reference

**Returns:**
- `(tuple 'pong round-trip-time)` - RTT in milliseconds
- `error-response()` - If ping fails

**Notes:**
- Gun: Sends HTTP/2 PING frame or HTTP/1.1 empty line
- httpc: Implementation simulates with lightweight request

**Example:**
```lisp
(lhc:ping conn)
;; => {pong, 45}  ; 45ms RTT
```

---

### Cookie Management

#### `(cookie-header uri options)`
Get Cookie header value for a URI.

**Args:**
- `uri`: `uri()` - Target URI
- `options`: `request-opts()` - Options (must include gun-opts with cookie-store)

**Returns:**
- `(tuple 'ok headers)` - Cookie header(s)
- `(tuple 'ok '())` - No cookies for this URI
- `error-response()` - If no cookie store configured

**Notes:**
- Gun only (uses gun-cookies backend)
- httpc manages cookies automatically when enabled

**Example:**
```lisp
(lhc:cookie-header "https://example.com/api" opts)
;; => {ok, [(#("Cookie" "session=abc123; user=alice"))]}
```

---

#### `(store-cookies set-cookie-headers uri options)`
Store cookies from Set-Cookie headers.

**Args:**
- `set-cookie-headers`: `headers()` - Set-Cookie headers from response
- `uri`: `uri()` - URI that sent the cookies
- `options`: `request-opts()` - Options with cookie store

**Returns:**
- `'ok` - Cookies stored
- `error-response()` - If no cookie store configured

**Notes:**
- Typically called automatically by lhc
- Manual use for custom cookie handling

---

#### `(cookies options)`
List all cookies in the store.

**Args:**
- `options`: `request-opts()` - Options with cookie store

**Returns:**
- `(tuple 'ok (list cookie-map))` - List of all cookies
- `error-response()` - If no cookie store

**Cookie Map Keys:**
- `name`: `binary()`
- `value`: `binary()`
- `domain`: `binary()`
- `path`: `binary()`
- `expires`: `(union calendar:datetime 'session)`
- `secure`: `boolean()`
- `http-only`: `boolean()`
- `same-site`: `(union 'strict 'lax 'none 'undefined)`

---

#### `(reset-cookies options)`
Clear all cookies from the store.

**Args:**
- `options`: `request-opts()` - Options with cookie store

**Returns:** `'ok`

**Notes:**
- Gun: Clears gun-cookies store
- httpc: Clears cookies for the specified profile

---

### WebSocket Support (Gun Only)

#### `(ws-upgrade conn-ref path options)`
Upgrade connection to WebSocket.

**Args:**
- `conn-ref`: `conn-ref()` - Connection reference (must be gun)
- `path`: `binary()` - WebSocket endpoint path
- `options`: `request-opts()` - Options with ws-opts

**Returns:**
- Sync: `(tuple 'ok stream-ref)` - Stream for WebSocket
- Async: `(tuple 'ok request-id stream-ref)` - Request and stream refs
- `error-response()` - On upgrade failure or if httpc backend

**Notes:**
- httpc backend returns not-implemented error
- Upgrade confirmation comes via `gun_upgrade` message in async mode

**Example:**
```lisp
(let* (((tuple 'ok conn) (lhc:open "wss://example.com" gun-opts))
       ((tuple 'ok stream) (lhc:ws-upgrade conn "/ws" ws-opts)))
  ;; WebSocket is ready
  (lhc:ws-send conn stream (map 'type 'text 'data "Hello")))
```

---

#### `(ws-send conn-ref stream-ref frame)`
Send WebSocket frame.

**Args:**
- `conn-ref`: `conn-ref()` - Connection reference
- `stream-ref`: `stream-ref()` - WebSocket stream reference
- `frame`: `ws-frame()` - Frame to send

**Frame Types:**
```lisp
(deftype ws-frame ()
  '(union (map type 'text data binary)
          (map type 'binary data binary)
          (map type 'ping)
          (map type 'pong)
          (map type 'close)
          (map type 'close code integer reason binary)))
```

**Returns:**
- `'ok` - Frame sent
- `error-response()` - On failure

**Example:**
```lisp
;; Text frame
(lhc:ws-send conn stream (map 'type 'text 'data "Hello WebSocket"))

;; Binary frame
(lhc:ws-send conn stream (map 'type 'binary 'data (binary 1 2 3 4)))

;; Close with reason
(lhc:ws-send conn stream (map 'type 'close 
                              'code 1000 
                              'reason "Normal closure"))
```

---

### CONNECT Tunnel (Gun Only)

#### `(connect conn-ref destination options)`
Establish HTTP CONNECT tunnel.

**Args:**
- `conn-ref`: `conn-ref()` - Connection reference to proxy
- `destination`: `map()` - Destination server info
- `options`: `request-opts()` - Options

**Destination Map:**
```lisp
#{host => binary(),          ; Required
  port => integer(),         ; Required
  protocols => [protocol()], ; Optional, default [http2, http]
  transport => transport()}  ; Optional, default tcp
```

**Returns:**
- Sync: `(tuple 'ok stream-ref)` - Tunnel stream
- Async: `(tuple 'ok request-id stream-ref)` - Request and stream refs  
- `error-response()` - On failure or if httpc backend

**Notes:**
- Gun only - for proxying requests
- httpc has proxy support but uses different mechanism
- Subsequent requests on tunnel use the stream-ref

**Example:**
```lisp
;; Connect through proxy
(let* (((tuple 'ok proxy-conn) (lhc:open "http://proxy:8080" opts))
       (dest (map 'host "api.example.com" 'port 443 'transport 'tls))
       ((tuple 'ok tunnel) (lhc:connect proxy-conn dest opts)))
  ;; Make requests through tunnel
  (lhc:request-with-conn proxy-conn 'get "/api/data" (binary) '() 
                         (map-update opts 'stream-ref tunnel)))
```

---

### Utility Functions

#### `(set-options profile options)`
Set profile options (httpc only).

**Args:**
- `profile`: `atom()` - Profile name
- `options`: `httpc-opts()` - httpc-specific options

**Returns:**
- `'ok` - Options set
- `error-response()` - On invalid options

**Notes:**
- httpc only - gun configuration is per-connection
- Affects all future requests using this profile

**Example:**
```lisp
(lhc:set-options 'default 
                 (map 'proxy-auth #("user" "pass")
                      'max-sessions 10))
```

---

#### `(get-options profile)`
Get current profile options (httpc only).

**Args:**
- `profile`: `atom()` - Profile name

**Returns:**
- `(tuple 'ok httpc-opts)` - Current options
- `error-response()` - If profile doesn't exist

---

#### `(normalize-headers headers)`
Normalize headers to canonical format.

**Args:**
- `headers`: `(union headers (map binary binary))` - Headers in any format

**Returns:** `headers()` - List of binary tuples

**Notes:**
- Converts various header formats to standard binary tuples
- Lowercases header names (HTTP/2 requirement)
- Useful for preparing headers before requests

**Example:**
```lisp
(lhc:normalize-headers 
  (map "Content-Type" "application/json"
       "accept" "application/json"))
;; => [(#("content-type" "application/json"))
;;     (#("accept" "application/json"))]
```

---

#### `(parse-uri uri)`
Parse URI into components.

**Args:**
- `uri`: `uri()` - URI to parse

**Returns:**
- `(tuple 'ok uri-map)` - Parsed URI components
- `error-response()` - On parse failure

**URI Map Keys:**
- `scheme`: `binary()` - http, https, ws, wss
- `host`: `binary()` - Hostname
- `port`: `(union integer 'undefined)` - Port number
- `path`: `binary()` - Path component
- `query`: `(union binary 'undefined)` - Query string
- `fragment`: `(union binary 'undefined)` - Fragment

**Example:**
```lisp
(lhc:parse-uri "https://api.example.com:8443/v1/users?active=true#section")
;; => {ok, #{scheme => <<"https">>,
;;           host => <<"api.example.com">>,
;;           port => 8443,
;;           path => <<"/v1/users">>,
;;           query => <<"active=true">>,
;;           fragment => <<"section">>}}
```

---

#### `(build-uri uri-map)`
Build URI from components.

**Args:**
- `uri-map`: `map()` - URI components (keys as in parse-uri)

**Returns:**
- `(tuple 'ok uri)` - Built URI
- `error-response()` - If required components missing

**Example:**
```lisp
(lhc:build-uri (map 'scheme "https"
                    'host "api.example.com"
                    'path "/users"))
;; => {ok, <<"https://api.example.com/users">>}
```

---

## Synchronous Operations Module (`lhc-sync`)

This module contains the actual implementations for synchronous operations. The `lhc` module dispatches to these functions when `mode` is `'sync` or when mode is not specified and the request doesn't have async-specific options.

### Functions

All request functions follow the same signature as `lhc` module but are explicitly synchronous:

- `(get uri options)` -> `response()` or `error-response()`
- `(head uri options)` -> `response()` or `error-response()`  
- `(post uri body options)` -> `response()` or `error-response()`
- `(put uri body options)` -> `response()` or `error-response()`
- `(patch uri body options)` -> `response()` or `error-response()`
- `(delete uri options)` -> `response()` or `error-response()`
- `(options uri options)` -> `response()` or `error-response()`
- `(request method uri body headers options)` -> `response()` or `error-response()`
- `(request-with-conn conn method uri body headers options)` -> `response()` or `error-response()`

**Note:** These functions block until the response is complete.

---

## Asynchronous Operations Module (`lhc-async`)

This module contains implementations for asynchronous operations. The `lhc` module dispatches here when `mode` is explicitly `'async` or when async-specific options are present (like `receiver` or `stream-to`).

### Functions

All request functions return immediately with request-id:

- `(get uri options)` -> `(tuple 'ok request-id)`
- `(head uri options)` -> `(tuple 'ok request-id)`
- `(post uri body options)` -> `(tuple 'ok request-id)`
- `(put uri body options)` -> `(tuple 'ok request-id)`
- `(patch uri body options)` -> `(tuple 'ok request-id)`
- `(delete uri options)` -> `(tuple 'ok request-id)`
- `(options uri options)` -> `(tuple 'ok request-id)`
- `(request method uri body headers options)` -> `(tuple 'ok request-id)`
- `(request-with-conn conn method uri body headers options)` -> `(tuple 'ok request-id stream-ref)`

### Async Messages

When using async mode, the receiver process gets these messages:

#### Success Response Flow
```lisp
;; 1. Informational (optional, 1xx status codes)
{http, {RequestId, inform, StatusCode, Headers}}

;; 2. Final response headers
{http, {RequestId, response, StatusCode, Headers}}

;; 3. Response body (if any)
;;    a) Single message for complete body
{http, {RequestId, body, Body}}
;;    b) OR streamed chunks
{http, {RequestId, body-chunk, Chunk}}
{http, {RequestId, body-chunk, Chunk}}
...
{http, {RequestId, body-end}}

;; 4. Trailers (optional, HTTP/2 or chunked encoding)
{http, {RequestId, trailers, Headers}}
```

#### Error Messages
```lisp
;; Connection errors
{http, {RequestId, error, #{reason => Reason, details => Details}}}

;; Connection down (gun)
{gun_down, ConnRef, Protocol, Reason, KilledStreams}

;; Connection up after reconnect (gun)
{gun_up, ConnRef, Protocol}
```

#### WebSocket Messages
```lisp
;; Incoming WebSocket frame
{ws, {StreamRef, #{type => Type, data => Data}}}

;; Where Type is: text | binary | ping | pong | close
```

---

## Backend Implementation Modules

### `lhc-httpc` Module

Wraps Erlang's `httpc` module with LFE conventions.

**Key Functions:**
- `(do-request method uri body headers opts)` - Execute httpc request
- `(create-profile profile opts)` - Set up httpc profile
- `(normalize-response response)` - Convert httpc response to lhc format
- `(normalize-error error)` - Convert httpc error to lhc error format

**httpc-specific Notes:**
- Profiles are managed automatically by module
- Connection pooling is implicit
- HTTP/1.1 only
- Limited WebSocket support (none)

---

### `lhc-gun` Module  

Wraps 99s' `gun` module with LFE conventions.

**Key Functions:**
- `(do-request conn method path body headers opts)` - Execute gun request
- `(open-connection uri opts)` - Open gun connection
- `(await-response conn stream-ref timeout)` - Wait for gun response
- `(normalize-response messages)` - Convert gun messages to lhc format
- `(normalize-error error)` - Convert gun error to lhc error format

**gun-specific Notes:**
- Explicit connection management required
- Supports HTTP/1.1, HTTP/2, WebSocket
- Full async support with message protocol
- Streaming and flow control

---

### `lhc-cookies` Module

Cookie store management (primarily wraps gun-cookies).

**Functions:**
- `(init opts)` -> `cookie-store()` - Initialize cookie store
- `(query store uri)` -> `headers()` - Get cookies for URI
- `(store store cookies uri)` -> `cookie-store()` - Add cookies from URI
- `(list-all store)` -> `(list cookie-map)` - List all cookies
- `(clear store)` -> `cookie-store()` - Clear all cookies
- `(gc store)` -> `cookie-store()` - Garbage collect expired cookies

**Notes:**
- Wraps `gun-cookies-list` by default
- Can be extended with custom backends
- Used automatically when cookie-store is configured

---

### `lhc-websocket` Module

WebSocket-specific functionality (gun only).

**Functions:**
- `(upgrade conn path headers opts)` -> `(tuple 'ok stream-ref)` - Upgrade to WS
- `(send-frame conn stream frame)` -> `'ok` - Send frame
- `(close conn stream reason)` -> `'ok` - Close WebSocket
- `(normalize-frame gun-frame)` -> `ws-frame()` - Convert gun WS frame

---

### `lhc-error` Module

Error normalization across backends.

**Error Categories:**
```lisp
(deftype error-category ()
  '(connection     ; Connection failures
    timeout        ; Request timeout
    protocol       ; Protocol errors
    parse          ; Response parse errors  
    ssl            ; TLS/SSL errors
    auth           ; Authentication failures
    redirect       ; Redirect errors
    not-implemented ; Feature not available
    unknown))      ; Uncategorized
```

**Functions:**
- `(normalize backend error)` -> `error-response()` - Normalize any error
- `(category error)` -> `error-category()` - Categorize error
- `(format error)` -> `binary()` - Human-readable error message

**Error Response Format:**
```lisp
{error, #{reason => ErrorCategory,
          message => <<"Human readable message">>,
          backend => Backend,
          details => OriginalError}}
```

---

### `lhc-util` Module

Shared utility functions.

**Functions:**
- `(ensure-binary data)` -> `binary()` - Convert to binary
- `(lowercase binary)` -> `binary()` - Lowercase binary
- `(header-value headers name)` -> `(union binary 'undefined)` - Find header
- `(merge-headers h1 h2)` -> `headers()` - Merge header lists
- `(timestamp)` -> `integer()` - Current timestamp (ms)
- `(parse-content-type value)` -> `map()` - Parse Content-Type header

---

## Error Handling Examples

### Connection Errors

```lisp
;; Gun connection failure
(lhc:open "https://invalid.example.com" opts)
;; => {error, #{reason => connection,
;;              message => <<"Failed to connect: nxdomain">>,
;;              backend => gun,
;;              details => {error, nxdomain}}}

;; httpc connection failure  
(lhc:get "http://192.0.2.1:9999" opts)
;; => {error, #{reason => connection,
;;              message => <<"Connection refused">>,
;;              backend => httpc,
;;              details => {error, econnrefused}}}
```

### Timeout Errors

```lisp
(lhc:get "https://slow.example.com" (map 'timeout 100))
;; => {error, #{reason => timeout,
;;              message => <<"Request timeout after 100ms">>,
;;              backend => gun,
;;              details => {error, timeout}}}
```

### Not Implemented

```lisp
;; WebSocket on httpc
(lhc:ws-upgrade conn "/ws" (map 'backend 'httpc))
;; => {error, #{reason => not-implemented,
;;              message => <<"WebSocket not supported by httpc backend">>,
;;              backend => httpc}}
```

---

## Usage Examples

### Basic Synchronous GET

```lisp
(defun fetch-user (id)
  (let* ((uri (++ "https://api.example.com/users/" (integer_to_list id)))
         (opts (map 'timeout 5000
                    'headers '(#("Accept" "application/json"))))
         (result (lhc:get uri opts)))
    (case result
      ((tuple 'ok (= (map status 200 body body) response))
       (tuple 'ok (parse-json body)))
      ((tuple 'ok (= (map status status) response))
       (tuple 'error (++ "Unexpected status: " (integer_to_list status))))
      ((tuple 'error error-map)
       (tuple 'error (maps:get 'message error-map))))))
```

### Asynchronous POST with Response Handling

```lisp
(defun create-user-async (user-data)
  (let* ((uri "https://api.example.com/users")
         (body (encode-json user-data))
         (headers '(#("Content-Type" "application/json")))
         (opts (map 'mode 'async
                    'receiver (self)
                    'timeout 10000))
         ((tuple 'ok req-id) (lhc:post uri body (map 'headers headers))))
    (receive
      ((tuple 'http (tuple req-id 'response status headers))
       (receive
         ((tuple 'http (tuple req-id 'body response-body))
          (tuple 'ok status (parse-json response-body)))
         (after 5000
           (tuple 'error "Body timeout"))))
      ((tuple 'http (tuple req-id 'error error-map))
       (tuple 'error error-map))
      (after 10000
        (lhc:cancel req-id)
        (tuple 'error "Request timeout")))))
```

### Using Explicit Connections (Gun)

```lisp
(defun batch-requests (urls)
  (let* ((opts (map 'backend 'gun
                    'gun-opts (map 'protocols '(http2))))
         ((tuple 'ok conn) (lhc:open "https://api.example.com" opts)))
    (try
      (let ((results 
              (lists:map
                (lambda (path)
                  (lhc:request-with-conn conn 'get path (binary) '()
                                        (map 'mode 'sync)))
                urls)))
        results)
      (after
        (lhc:close conn)))))
```

### WebSocket Communication

```lisp
(defun websocket-echo ()
  (let* ((opts (map 'backend 'gun))
         ((tuple 'ok conn) (lhc:open "wss://echo.websocket.org" opts))
         ((tuple 'ok stream) (lhc:ws-upgrade conn "/" opts)))
    ;; Send message
    (lhc:ws-send conn stream (map 'type 'text 'data "Hello WebSocket!"))
    ;; Wait for echo
    (receive
      ((tuple 'ws (tuple stream (= (map type 'text data response) frame)))
       (io:format "Received: ~s~n" (list response))
       (lhc:ws-send conn stream (map 'type 'close))
       (lhc:close conn))
      (after 5000
        (lhc:close conn)
        (tuple 'error "No response")))))
```

### Cookie Store Usage

```lisp
(defun login-with-cookies ()
  (let* ((cookie-store (lhc-cookies:init (map)))
         (opts (map 'backend 'gun
                    'gun-opts (map 'cookie-store cookie-store)))
         ;; Login request
         (login-result 
           (lhc:post "https://example.com/login"
                     (binary "{\"user\":\"alice\",\"pass\":\"secret\"}")
                     (map 'headers '(#("Content-Type" "application/json")))))
         ;; Cookies are automatically stored
         ;; Subsequent requests automatically include cookies
         (profile-result
           (lhc:get "https://example.com/profile" opts)))
    (tuple login-result profile-result)))
```

### Streaming Upload

```lisp
(defun upload-file (file-path)
  (let* ((opts (map 'backend 'gun))
         ((tuple 'ok conn) (lhc:open "https://upload.example.com" opts))
         (headers '(#("Content-Type" "application/octet-stream")))
         ((tuple 'ok req-id stream) 
          (lhc:request-with-conn conn 'post "/upload" 'stream headers
                                 (map 'mode 'async))))
    (try
      (progn
        ;; Stream file in chunks
        (stream-file-chunks conn stream file-path)
        ;; Signal end
        (lhc:stream-body conn stream 'eof opts)
        ;; Wait for response
        (lhc:await req-id 30000))
      (after
        (lhc:close conn)))))

(defun stream-file-chunks (conn stream file-path)
  (let* (((tuple 'ok device) (file:open file-path '(read binary)))
         (chunk-size 65536))
    (try
      (stream-loop conn stream device chunk-size)
      (after
        (file:close device)))))

(defun stream-loop (conn stream device chunk-size)
  (case (file:read device chunk-size)
    ((tuple 'ok chunk)
     (lhc:stream-body conn stream chunk (map))
     (stream-loop conn stream device chunk-size))
    ('eof 'ok)
    ((tuple 'error reason)
     (error (tuple 'file-read-error reason)))))
```

### Through Proxy with CONNECT

```lisp
(defun request-via-proxy ()
  (let* ((proxy-opts (map 'backend 'gun))
         ((tuple 'ok proxy-conn) 
          (lhc:open "http://proxy.example.com:8080" proxy-opts))
         (destination (map 'host "api.secure.com"
                          'port 443
                          'transport 'tls
                          'protocols '(http2)))
         ((tuple 'ok tunnel-stream) 
          (lhc:connect proxy-conn destination proxy-opts))
         ;; Make request through tunnel
         (result 
           (lhc:request-with-conn proxy-conn 'get "/api/data" (binary) '()
                                  (map 'mode 'sync
                                       'stream-ref tunnel-stream))))
    (lhc:close proxy-conn)
    result))
```

---

## Migration from Old lhc

### Old API -> New API Mapping

```lisp
;; Old: Simple GET
(lhc:get "http://example.com")
;; New: Same (defaults to sync mode)
(lhc:get "http://example.com" (map))

;; Old: GET with headers
(lhc:get "http://example.com" '(#("Accept" "application/json")))
;; New: Headers in options map
(lhc:get "http://example.com" (map 'headers '(#("Accept" "application/json"))))

;; Old: POST
(lhc:post "http://example.com" "data" "text/plain")
;; New: Content-Type in headers
(lhc:post "http://example.com" 
          (binary "data")
          (map 'headers '(#("Content-Type" "text/plain"))))

;; Old: Async request (if supported)
(lhc:request 'async 'get "http://example.com")
;; New: Mode in options
(lhc:get "http://example.com" (map 'mode 'async 'receiver (self)))
```

---

## Backend Feature Matrix

| Feature | httpc | gun | Notes |
|---------|-------|-----|-------|
| HTTP/1.1 | ✓ | ✓ | Both fully support |
| HTTP/2 | ✗ | ✓ | httpc HTTP/1.1 only |
| Sync mode | ✓ | ✓ | Both support |
| Async mode | ✓ | ✓ | Both support |
| Connection pooling | ✓ | Manual | httpc automatic, gun explicit |
| Persistent connections | ✓ | ✓ | Both support |
| Pipelining | Limited | ✓ | gun has better support |
| WebSocket | ✗ | ✓ | gun only |
| Server push | ✗ | ✓ | HTTP/2 feature (gun) |
| Flow control | ✗ | ✓ | HTTP/2 feature (gun) |
| Streaming upload | Limited | ✓ | gun has full support |
| Streaming download | ✓ | ✓ | Both support |
| Cookies | ✓ | ✓ | httpc built-in, gun via gun-cookies |
| CONNECT proxy | ✓ | ✓ | Different mechanisms |
| SOCKS proxy | ✗ | ✓ | gun only |
| Unix sockets | ✗ | ✓ | gun only |
| Event handlers | ✗ | ✓ | gun only (gun_event) |

---

## Configuration Examples

### Minimal Configuration (defaults)

```lisp
;; Uses httpc backend, sync mode, default timeouts
(lhc:get "https://api.example.com/users" (map))
```

### Full httpc Configuration

```lisp
(let ((opts (map 'backend 'httpc
                 'mode 'sync
                 'timeout 10000
                 'connect-timeout 5000
                 'follow-redirects 'true
                 'max-redirects 3
                 'headers '(#("User-Agent" "lhc/2.0"))
                 'httpc-opts (map 
                   'profile 'my-api
                   'ssl-opts '(#(verify verify_peer)
                              #(cacerts (public_key:cacerts_get)))
                   'autoredirect 'true
                   'version 'HTTP/1.1
                   'relaxed 'false))))
  (lhc:get "https://api.example.com/data" opts))
```

### Full Gun Configuration

```lisp
(let ((opts (map 'backend 'gun
                 'mode 'async
                 'receiver (self)
                 'timeout 15000
                 'connect-timeout 5000
                 'gun-opts (map
                   'protocols '(http2 http)
                   'transport 'tls
                   'retry 3
                   'retry-timeout 1000
                   'http2-opts (map
                     'keepalive 30000
                     'keepalive-tolerance 3
                     'initial-connection-window-size 1048576)
                   'tls-opts '(#(verify verify_peer)
                              #(cacerts (public_key:cacerts_get))
                              #(server_name_indication "api.example.com"))))))
  (lhc:get "https://api.example.com/data" opts))
```

### WebSocket Configuration

```lisp
(let ((opts (map 'backend 'gun
                 'gun-opts (map
                   'ws-opts (map
                     'compress 'true
                     'keepalive 15000
                     'silence-pings 'false
                     'protocols '(#("chat" my-chat-protocol)))))))
  (let* (((tuple 'ok conn) (lhc:open "wss://chat.example.com" opts))
         ((tuple 'ok stream) (lhc:ws-upgrade conn "/chat" opts)))
    ;; Use WebSocket
    stream))
```

---

## Testing Utilities

These are helper functions useful for testing (could be in a `lhc-test` module):

### `(mock-response status body headers)`
Create a mock response for testing.

### `(with-mock-backend backend-fun test-fun)`
Run tests with a mocked backend.

### `(capture-async-messages timeout)`
Capture all async HTTP messages within timeout.

---

## Performance Considerations

### Connection Reuse

```lisp
;; Good: Reuse connection for multiple requests
(let* (((tuple 'ok conn) (lhc:open uri opts)))
  (try
    (lists:map 
      (lambda (path) (lhc:request-with-conn conn 'get path ...))
      paths)
    (after (lhc:close conn))))

;; Less efficient: New connection per request
(lists:map
  (lambda (path) (lhc:get (++ base-uri path) opts))
  paths)
```

### HTTP/2 Benefits

```lisp
;; With HTTP/2, can send multiple requests in parallel over one connection
(let* ((opts (map 'backend 'gun 
                  'gun-opts (map 'protocols '(http2))))
       ((tuple 'ok conn) (lhc:open uri opts)))
  ;; All requests share one TCP connection
  (let ((req-ids
          (lists:map
            (lambda (path)
              (let (((tuple 'ok req-id stream) 
                     (lhc:request-with-conn conn 'get path (binary) '()
                                           (map 'mode 'async))))
                req-id))
            paths)))
    ;; Collect responses
    (lists:map (lambda (id) (lhc:await id 5000)) req-ids)))
```

### Streaming Large Responses

```lisp
;; Stream to file instead of loading into memory
(lhc:get large-file-url 
         (map 'stream-to (tuple 'file "/tmp/download.bin")
              'mode 'async))
```

---

## Advanced Features

### Custom Retry Logic

```lisp
(defun retry-with-backoff (request-fun max-retries)
  (retry-loop request-fun max-retries 1))

(defun retry-loop (request-fun retries-left backoff)
  (case (request-fun)
    ((tuple 'ok response) 
     (tuple 'ok response))
    ((tuple 'error (= (map reason 'timeout) _))
     (if (> retries-left 0)
       (progn
         (timer:sleep (* backoff 1000))
         (retry-loop request-fun (- retries-left 1) (* backoff 2)))
       (tuple 'error "Max retries exceeded")))
    ((tuple 'error err) 
     (tuple 'error err))))
```

### Rate Limiting

```lisp
(defmodule rate-limiter
  (export (make-limiter 2) (check 1)))

(defun make-limiter (requests-per-second)
  (let ((interval-ms (trunc (/ 1000 requests-per-second))))
    (map 'interval interval-ms 'last-request 0)))

(defun check (limiter)
  (let* ((now (erlang:system_time 'millisecond))
         (last (maps:get 'last-request limiter))
         (interval (maps:get 'interval limiter))
         (elapsed (- now last)))
    (if (< elapsed interval)
      (progn
        (timer:sleep (- interval elapsed))
        (maps:put 'last-request (erlang:system_time 'millisecond) limiter))
      (maps:put 'last-request now limiter))))

;; Usage
(let ((limiter (rate-limiter:make-limiter 10)))  ; 10 req/sec
  (lists:foldl
    (lambda (url acc-limiter)
      (let ((new-limiter (rate-limiter:check acc-limiter)))
        (lhc:get url (map))
        new-limiter))
    limiter
    urls))
```

---

## Common Patterns

### Request with Automatic Retry

```lisp
(defun reliable-get (uri)
  (lhc:get uri (map 'backend 'gun
                    'gun-opts (map 'retry 3 'retry-timeout 2000))))
```

### Concurrent Requests with Timeout

```lisp
(defun parallel-fetch (urls timeout)
  (let* ((parent (self))
         (workers 
           (lists:map
             (lambda (url)
               (spawn (lambda ()
                 (let ((result (lhc:get url (map 'timeout timeout))))
                   (! parent (tuple url result))))))
             urls)))
    (collect-results urls timeout '())))

(defun collect-results
  (['() _ acc] acc)
  ([urls timeout acc]
   (receive
     ((tuple url result)
      (collect-results (lists:delete url urls) timeout 
                      (cons (tuple url result) acc)))
     (after timeout
       ;; Return partial results
       acc))))
```

### Authentication with Token Refresh

```lisp
(defun api-request-with-auth (method uri body token-ref)
  (let* ((token (get-token token-ref))
         (headers `(#("Authorization" ,(++ "Bearer " token))))
         (result (lhc:request method uri body headers (map))))
    (case result
      ((tuple 'ok (= (map status 401) _))
       ;; Token expired, refresh and retry
       (let ((new-token (refresh-token token-ref)))
         (api-request-with-auth method uri body token-ref)))
      (_ result))))
```

---

## Appendix: Message Protocol (Async Mode)

### Complete Message Flow Example

```lisp
;; Make async request
(let (((tuple 'ok req-id) 
       (lhc:get "https://api.example.com/data" (map 'mode 'async))))
  
  ;; Message sequence:
  
  ;; 1. Optional: Connection established (gun only)
  (receive ((tuple 'gun_up conn-ref 'http2) 'ok))
  
  ;; 2. Optional: Informational response
  (receive ((tuple 'http (tuple req-id 'inform 100 headers)) 'continue))
  
  ;; 3. Response status and headers
  (receive 
    ((tuple 'http (tuple req-id 'response status headers))
     (io:format "Status: ~p~n" (list status))))
  
  ;; 4. Response body (may be chunked)
  (receive
    ;; Single body message
    ((tuple 'http (tuple req-id 'body body))
     (io:format "Body: ~p~n" (list body)))
    
    ;; Or chunked (streaming)
    ((tuple 'http (tuple req-id 'body-chunk chunk))
     (io:format "Chunk: ~p~n" (list chunk))
     ;; May receive more body-chunk messages
     (receive ((tuple 'http (tuple req-id 'body-end)) 'done))))
  
  ;; 5. Optional: Trailers
  (receive 
    ((tuple 'http (tuple req-id 'trailers trailer-headers))
     (io:format "Trailers: ~p~n" (list trailer-headers))))
  
  ;; Error scenarios:
  (receive
    ((tuple 'http (tuple req-id 'error error-map))
     (io:format "Error: ~p~n" (list error-map)))
    
    ((tuple 'gun_down conn-ref protocol reason killed-streams)
     (io:format "Connection down: ~p~n" (list reason)))))
```

---

## Appendix: Type Specifications Reference

Complete type specs for use in your own code:

```lisp
;; Import types
(include-lib "lhc/include/lhc-types.lfe")

;; Function with type specs
(defun my-api-call (endpoint)
  "Make API call and return parsed response"
  (let ((result (lhc:get (++ "https://api.example.com" endpoint)
                         (map 'timeout 5000))))
    (case result
      ((tuple 'ok (= (map body body) response))
       (parse-json body))
      ((tuple 'error error)
       (tuple 'error (lhc-error:format error))))))

;; Type: (-> (string) (tuple 'ok map) | (tuple 'error string))
```

---

## End of API Documentation

This unified API provides:
- **Harmonized interface** for both httpc and gun
- **Lisp conventions** throughout (kebab-case, maps over proplists)
- **Mode-based dispatch** (sync/async) via options
- **Explicit connection management** when needed (gun)
- **Automatic connection handling** when desired (httpc)
- **Complete feature coverage** of both backends
- **Normalized errors** across backends
- **WebSocket support** (gun only, with proper not-implemented errors for httpc)
- **Cookie management** (both backends)
- **Streaming** (both upload and download)
- **Flexible configuration** (backend-specific options)

The API is designed to be intuitive for Lisp developers while providing access to all the powerful features of both underlying HTTP client libraries.
