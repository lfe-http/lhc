# LHC Library Reimplementation - Claude Code Instructions

## Project Overview

You are reimplementing the LFE Little HTTP Client (lhc) library to provide a unified, modern API that supports both Erlang's `httpc` and 99s' `gun` HTTP client backends. This is a complete rewrite that preserves backward compatibility where possible while introducing a cleaner, more powerful interface.

## Current State Analysis

### Existing Library (v0.4.0)

- **Current backend**: Only supports Erlang's `httpc` (commented-out support for lhttpc, dlhttpc, hackney, ibrowse)
- **API style**: Simple wrapper functions (get, post, put, delete, etc.)
- **Options**: Uses proplists and records (see `include/lhc-options.lfe`)
- **Return values**: Configurable via options (status, headers, body, all)
- **No async support**: All operations are synchronous
- **No WebSocket support**: HTTP only
- **Limited connection management**: Implicit only

### Current File Structure

```
lhc/
├── include/
│   ├── lhc-options.lfe    (defrecord lhc-opts)
│   └── lhc-records.lfe    (netloc, urldata records)
├── src/
│   ├── lhc.app.src
│   ├── lhc.lfe            (main API)
│   ├── lhc-backend.lfe    (backend dispatch)
│   ├── lhc-util.lfe       (utilities)
│   └── lhc-vsn.lfe        (version info)
└── test/
    ├── unit-lhc-tests.lfe
    └── unit-lhc-util-tests.lfe
```

### Migration Considerations

1. **Preserve utility functions**: Keep `lhc-util:user-agent/0`, `utf8-encode/1,2`, `utf8-decode/1`
2. **Preserve version functions**: Keep `lhc-vsn` module intact
3. **Breaking changes acceptable**: This is a major version bump (0.4.0 → 2.0.0)
4. **Keep existing app metadata**: Preserve license, links, etc. in `.app.src`

## New Architecture (Target Implementation)

Refer to the API documentation in `lhc-api.md` for complete specifications. Key points:

### Module Structure

```
lhc/
├── include/
│   ├── lhc-types.lfe       (NEW: type definitions)
│   └── lhc-records.lfe     (KEEP but may extend)
├── src/
│   ├── lhc.app.src         (UPDATE: add gun dependency)
│   ├── lhc.lfe             (REWRITE: public API with dispatch)
│   ├── lhc-sync.lfe        (NEW: synchronous operations)
│   ├── lhc-async.lfe       (NEW: asynchronous operations)
│   ├── lhc-httpc.lfe       (REWRITE: httpc backend)
│   ├── lhc-gun.lfe         (NEW: gun backend)
│   ├── lhc-cookies.lfe     (NEW: cookie management)
│   ├── lhc-websocket.lfe   (NEW: WebSocket support)
│   ├── lhc-error.lfe       (NEW: error normalization)
│   ├── lhc-util.lfe        (EXTEND: keep existing + add new)
│   └── lhc-vsn.lfe         (KEEP: as-is)
└── test/
    ├── unit-lhc-*.lfe      (EXTEND: comprehensive tests)
    └── integration-*.lfe   (NEW: integration tests)
```

## Implementation Plan

### Phase 1: Foundation (Priority 1)

Implement core infrastructure that everything else depends on.

#### 1.1 Update Dependencies (`rebar.config`)

```erlang
{deps, [
  {lfe, "~> 2.2"},
  {lutil, "~> 0.13"},
  {gun, "~> 2.2"}
]}.
```

#### 1.2 Create Type Definitions (`include/lhc-types.lfe`)

Create all type definitions from the API spec. These should be exportable types that can be used by library users.

```lfe
(defmodule lhc-types
  (export-type all))

;; Define all types from API spec
(deftype backend () '(httpc gun))
(deftype mode () '(sync async))
;; ... etc (see lhc-api.md Types section)
```

#### 1.3 Implement Error Normalization (`src/lhc-error.lfe`)

This module must be implemented first as all other modules depend on it.

**Key Functions:**

```lfe
(defun normalize (backend error) ...)
  ;; -> (tuple 'error error-map)
  ;; error-map: #{reason => category, message => binary,
  ;;              backend => backend, details => original}

(defun category (error) ...)
  ;; -> error-category atom

(defun format (error-map) ...)
  ;; -> human-readable binary string
```

**Error Categories:**

- `connection` - Network/connection failures
- `timeout` - Request timeouts
- `protocol` - HTTP protocol errors
- `parse` - Response parsing errors
- `ssl` - TLS/SSL errors
- `auth` - Authentication failures
- `redirect` - Redirect errors
- `not-implemented` - Feature unavailable in backend
- `unknown` - Uncategorized

**Implementation Notes:**

- For httpc errors, map common tuples like `{error, nxdomain}`, `{error, timeout}`, etc.
- For gun errors, map gun_down, gun_error messages
- Always preserve original error in `details` field
- Use pattern matching for known error patterns
- Default to `unknown` category for unrecognized errors

#### 1.4 Extend Utilities (`src/lhc-util.lfe`)

Keep existing functions, add new ones:

**Preserve from current:**

```lfe
(defun user-agent () ...)         ; Keep as-is
(defun utf8-encode ...)           ; Keep all arities
(defun utf8-decode ...)           ; Keep as-is
(defun proplist? ...)             ; Keep as-is
(defun proplist-kv? ...)          ; Keep as-is
```

**Add new utilities:**

```lfe
(defun ensure-binary (data) ...)
  ;; Convert string/iolist to binary

(defun lowercase (binary) ...)
  ;; Lowercase a binary string

(defun header-value (headers name) ...)
  ;; Find header by name (case-insensitive)
  ;; -> (union binary 'undefined)

(defun merge-headers (h1 h2) ...)
  ;; Merge two header lists, h2 overrides h1

(defun timestamp () ...)
  ;; Current timestamp in milliseconds

(defun parse-content-type (value) ...)
  ;; Parse Content-Type header into map
  ;; -> #{type => binary, subtype => binary, params => map}

(defun normalize-headers (headers) ...)
  ;; Convert various header formats to standard binary tuples
  ;; Accept: list of tuples, map, mixed string/binary/atom keys
  ;; Return: list of {binary(), binary()} tuples

(defun parse-uri (uri) ...)
  ;; Parse URI string into component map
  ;; -> (tuple 'ok uri-map) | error-response

(defun build-uri (uri-map) ...)
  ;; Build URI from component map
  ;; -> (tuple 'ok binary) | error-response
```

**Implementation Notes for URI Functions:**

- Use `uri_string:parse/1` from Erlang stdlib for parsing
- Use `uri_string:recompose/1` for building
- Validate required components (scheme, host for absolute URIs)
- Handle both absolute and relative URIs appropriately

### Phase 2: Backend Implementations (Priority 1)

#### 2.1 httpc Backend (`src/lhc-httpc.lfe`)

Rewrite the existing backend with the new architecture.

**Module Structure:**

```lfe
(defmodule lhc-httpc
  (export
   ;; Core request execution
   (do-request 5)         ; (method uri body headers opts)
   (do-request-sync 5)
   (do-request-async 5)

   ;; Profile management
   (create-profile 2)     ; (profile opts)
   (set-profile-options 2)
   (get-profile-options 1)

   ;; Response normalization
   (normalize-response 1) ; Convert httpc response to lhc format
   (normalize-error 1)    ; Convert httpc error to lhc format

   ;; Options conversion
   (make-httpc-options 1) ; Convert lhc opts to httpc format
   ))
```

**Key Implementation Details:**

1. **Request Execution:**

```lfe
(defun do-request (method uri body headers opts)
  ;; Dispatch to sync or async based on opts
  (case (maps:get 'mode opts 'sync)
    ('sync (do-request-sync method uri body headers opts))
    ('async (do-request-async method uri body headers opts))))

(defun do-request-sync (method uri body headers opts)
  ;; Build httpc request tuple
  (let* ((request (make-request uri body headers))
         (http-opts (make-http-opts opts))
         (httpc-opts (make-httpc-opts opts))
         (httpc-method (method->httpc method)))
    ;; Call httpc:request/4
    (case (httpc:request httpc-method request http-opts httpc-opts)
      (`#(ok ,response)
       (normalize-response response))
      (`#(error ,reason)
       (lhc-error:normalize 'httpc reason)))))

(defun do-request-async (method uri body headers opts)
  ;; Set up receiver, make async request
  (let* ((receiver (maps:get 'receiver opts (self)))
         (request (make-request uri body headers))
         (http-opts (make-http-opts (maps:put 'sync 'false opts)))
         (httpc-opts (make-httpc-opts (maps:put 'receiver receiver opts)))
         (httpc-method (method->httpc method)))
    (case (httpc:request httpc-method request http-opts httpc-opts)
      (`#(ok ,request-id)
       `#(ok ,request-id))
      (`#(error ,reason)
       (lhc-error:normalize 'httpc reason)))))
```

2. **Options Conversion:**

```lfe
(defun make-http-opts (lhc-opts)
  ;; Extract httpc HTTP options from lhc opts
  (let* ((httpc-opts (maps:get 'httpc-opts lhc-opts #{}))
         (timeout (maps:get 'timeout lhc-opts 5000))
         (connect-timeout (maps:get 'connect-timeout lhc-opts timeout))
         (ssl-opts (maps:get 'ssl-opts httpc-opts '()))
         (autoredirect (maps:get 'autoredirect httpc-opts
                                  (maps:get 'follow-redirects lhc-opts 'true))))
    ;; Build httpc http_options() list
    `(#(timeout ,timeout)
      #(connect_timeout ,connect-timeout)
      #(ssl ,ssl-opts)
      #(autoredirect ,autoredirect))))

(defun make-httpc-opts (lhc-opts)
  ;; Extract httpc options() from lhc opts
  (let* ((httpc-opts (maps:get 'httpc-opts lhc-opts #{}))
         (stream-to (maps:get 'stream-to lhc-opts 'none)))
    ;; Build options list
    (let ((base-opts '(#(body_format binary))))
      (case stream-to
        ('none base-opts)
        (`#(file ,path)
         (cons `#(stream ,path) base-opts))
        (`#(self)
         (cons `#(sync false) base-opts))
        (_ base-opts)))))
```

3. **Response Normalization:**

```lfe
(defun normalize-response
  ;; Full response
  ((`#(#(,_version ,status ,_reason) ,headers ,body))
   `#(ok ,(maps:from_list
            `(#(status ,status)
              #(headers ,(normalize-headers headers))
              #(body ,body)))))

  ;; Async response messages need to be accumulated
  ;; This is handled in lhc-async module

  (other)
   (lhc-error:normalize 'httpc `#(unknown-response ,other))))

(defun normalize-headers (headers)
  ;; Convert httpc headers to standard format
  ;; httpc returns strings, we want binaries
  (lists:map
    (lambda (`#(,name ,value))
      `#(,(lhc-util:ensure-binary (string:lowercase name))
         ,(lhc-util:ensure-binary value)))
    headers))
```

4. **Method Conversion:**

```lfe
(defun method->httpc
  (('GET) 'get)
  (('HEAD) 'head)
  (('POST) 'post)
  (('PUT) 'put)
  (('DELETE) 'delete)
  (('TRACE) 'trace)
  (('OPTIONS) 'options)
  (('PATCH) 'patch)   ; Note: httpc doesn't support PATCH
  (('CONNECT) 'connect)) ; Note: httpc CONNECT support is limited
```

5. **Profile Management:**

```lfe
(defun create-profile (profile opts)
  ;; Set up an httpc profile with options
  (httpc:set_options (make-profile-opts opts) profile))

(defun make-profile-opts (lhc-opts)
  ;; Convert lhc options to httpc profile options
  ;; max_sessions, max_keep_alive_length, keep_alive_timeout, etc.
  ...)
```

**Error Cases to Handle:**

- PATCH method not supported → return not-implemented error
- WebSocket upgrade → return not-implemented error
- Flow control operations → return not-implemented error
- Advanced HTTP/2 features → return not-implemented error

#### 2.2 gun Backend (`src/lhc-gun.lfe`)

Implement the gun backend from scratch.

**Module Structure:**

```lfe
(defmodule lhc-gun
  (export
   ;; Connection management
   (open-connection 2)    ; (uri opts)
   (close-connection 1)   ; (conn-ref)
   (shutdown-connection 1)

   ;; Request execution
   (do-request 6)         ; (conn method path body headers opts)
   (do-request-sync 6)
   (do-request-async 6)

   ;; Response handling
   (await-response 3)     ; (conn stream-ref timeout)
   (collect-response 3)   ; Collect async messages into response

   ;; Streaming
   (stream-body 4)        ; (conn stream-ref data opts)
   (update-flow 3)        ; (conn stream-ref increment)

   ;; Info
   (connection-info 1)    ; (conn-ref)
   (stream-info 2)        ; (conn-ref stream-ref)
   (ping 1)               ; (conn-ref)

   ;; Normalization
   (normalize-response 1)
   (normalize-error 1)
   (make-gun-options 1)
   ))
```

**Key Implementation Details:**

1. **Connection Opening:**

```lfe
(defun open-connection (uri opts)
  (let* ((uri-map (lhc-util:parse-uri uri))
         (host (binary_to_list (maps:get 'host uri-map)))
         (port (get-port uri-map))
         (gun-opts (make-gun-options opts)))
    (case (gun:open host port gun-opts)
      (`#(ok ,conn-pid)
       ;; Wait for connection
       (case (gun:await_up conn-pid (maps:get 'connect-timeout opts 5000))
         (`#(ok ,protocol)
          `#(ok ,conn-pid))
         (`#(error ,reason)
          (gun:close conn-pid)
          (lhc-error:normalize 'gun reason))))
      (`#(error ,reason)
       (lhc-error:normalize 'gun reason)))))

(defun get-port (uri-map)
  (case (maps:get 'port uri-map 'undefined)
    ('undefined
     (case (maps:get 'scheme uri-map)
       (<<"https">>) 443)
       (<<"wss">>) 443)
       (_ 80)))
    (port port)))
```

2. **Synchronous Requests:**

```lfe
(defun do-request-sync (conn method path body headers opts)
  (let* ((stream-ref (gun:request conn method path headers body))
         (timeout (maps:get 'timeout opts 5000)))
    (await-response conn stream-ref timeout)))

(defun await-response (conn stream-ref timeout)
  ;; Use gun:await/2,3 to collect response
  (case (gun:await conn stream-ref timeout)
    (`#(response fin ,status ,headers)
     ;; No body
     `#(ok ,(maps:from_list
              `(#(status ,status)
                #(headers ,(normalize-headers headers))
                #(body ,(binary))))))

    (`#(response nofin ,status ,headers)
     ;; Body follows
     (case (gun:await_body conn stream-ref timeout)
       (`#(ok ,body)
        `#(ok ,(maps:from_list
                 `(#(status ,status)
                   #(headers ,(normalize-headers headers))
                   #(body ,body)))))
       (`#(error ,reason)
        (lhc-error:normalize 'gun reason))))

    (`#(error ,reason)
     (lhc-error:normalize 'gun reason))))
```

3. **Asynchronous Requests:**

```lfe
(defun do-request-async (conn method path body headers opts)
  (let* ((stream-ref (gun:request conn method path headers body))
         (request-id (make_ref)))
    ;; Store mapping of request-id to stream-ref for later
    ;; The lhc-async module will handle message collection
    `#(ok ,request-id ,stream-ref)))
```

4. **Options Conversion:**

```lfe
(defun make-gun-options (lhc-opts)
  (let* ((gun-opts (maps:get 'gun-opts lhc-opts #{}))
         (transport (get-transport lhc-opts))
         (protocols (maps:get 'protocols gun-opts
                              (get-default-protocols transport)))
         (retry (maps:get 'retry gun-opts 5))
         (retry-timeout (maps:get 'retry-timeout gun-opts 5000)))
    (maps:merge
      (maps:from_list
        `(#(transport ,transport)
          #(protocols ,protocols)
          #(retry ,retry)
          #(retry_timeout ,retry-timeout)))
      (get-protocol-opts gun-opts))))

(defun get-default-protocols
  (('tcp) '(http))
  (('tls) '(http2 http)))

(defun get-protocol-opts (gun-opts)
  ;; Extract http_opts, http2_opts, ws_opts and convert
  (let ((http-opts (maps:get 'http-opts gun-opts #{}))
        (http2-opts (maps:get 'http2-opts gun-opts #{}))
        (ws-opts (maps:get 'ws-opts gun-opts #{})))
    ;; Convert to gun format
    (maps:from_list
      (lists:flatten
        `(,(if (map_size http-opts)
             `((#(http_opts ,(make-gun-http-opts http-opts))))
             '())
          ,(if (map_size http2-opts)
             `((#(http2_opts ,(make-gun-http2-opts http2-opts))))
             '())
          ,(if (map_size ws-opts)
             `((#(ws_opts ,(make-gun-ws-opts ws-opts))))
             '()))))))
```

5. **Message Handling:**
Gun sends messages to the connection owner. The async module needs to handle:

- `{gun_response, ConnPid, StreamRef, IsFin, Status, Headers}`
- `{gun_data, ConnPid, StreamRef, IsFin, Data}`
- `{gun_trailers, ConnPid, StreamRef, Trailers}`
- `{gun_error, ConnPid, StreamRef, Reason}`
- `{gun_error, ConnPid, Reason}`
- `{gun_up, ConnPid, Protocol}`
- `{gun_down, ConnPid, Protocol, Reason, KilledStreams}`

**Critical Notes:**

- Gun connections are processes that MUST be closed
- Always use gun:await_up after gun:open
- Handle reconnection via gun_up/gun_down messages
- Flow control is important for large responses
- Cookie store must be initialized if using cookies

### Phase 3: Core API Layers (Priority 1)

#### 3.1 Synchronous Operations (`src/lhc-sync.lfe`)

Implement all sync HTTP methods.

**Module Structure:**

```lfe
(defmodule lhc-sync
  (export
   (get 2)
   (head 2)
   (post 3)
   (put 3)
   (patch 3)
   (delete 2)
   (options 2)
   (request 5)
   (request-with-conn 6)
   ))
```

**Implementation Pattern:**
Each function:

1. Validates options
2. Determines backend from options
3. Prepares request parameters
4. Calls appropriate backend module
5. Returns normalized response or error

```lfe
(defun get (uri opts)
  (request 'GET uri (binary) '() opts))

(defun request (method uri body headers opts)
  ;; Merge headers into opts
  (let* ((merged-opts (maps:put 'headers
                                (lhc-util:merge-headers
                                  headers
                                  (maps:get 'headers opts '()))
                                opts))
         (backend (maps:get 'backend merged-opts 'httpc)))
    (case backend
      ('httpc
       (lhc-httpc:do-request-sync method uri body
                                  (maps:get 'headers merged-opts '())
                                  merged-opts))
      ('gun
       ;; For gun without explicit connection, open one temporarily
       (case (lhc-gun:open-connection uri merged-opts)
         (`#(ok ,conn)
          (let ((result (request-with-conn conn method uri body headers merged-opts)))
            (lhc-gun:close-connection conn)
            result))
         (error error)))
      (_
       (lhc-error:normalize 'lhc `#(unknown-backend ,backend))))))

(defun request-with-conn (conn method uri body headers opts)
  ;; Assumes connection is already open
  (let* ((backend (maps:get 'backend opts 'httpc))
         (path (extract-path uri)))
    (case backend
      ('gun
       (lhc-gun:do-request-sync conn method path body headers opts))
      ('httpc
       ;; httpc doesn't really use explicit connections the same way
       ;; but we can use the profile
       (lhc-httpc:do-request-sync method uri body headers
                                  (maps:put 'profile conn opts)))
      (_
       (lhc-error:normalize 'lhc `#(unknown-backend ,backend))))))
```

#### 3.2 Asynchronous Operations (`src/lhc-async.lfe`)

Implement all async HTTP methods with message handling.

**Module Structure:**

```lfe
(defmodule lhc-async
  (export
   (get 2)
   (head 2)
   (post 3)
   (put 3)
   (patch 3)
   (delete 2)
   (options 2)
   (request 5)
   (request-with-conn 6)

   ;; Message handling
   (await 2)            ; (request-id timeout)
   (await-body 2)       ; (request-id timeout)
   (cancel 1)           ; (request-id)
   (flush 1)            ; (request-id)
   ))
```

**Implementation Notes:**

1. **Async Requests Return Immediately:**

```lfe
(defun get (uri opts)
  (request 'GET uri (binary) '() opts))

(defun request (method uri body headers opts)
  (let* ((merged-opts (maps:put 'mode 'async
                                (maps:put 'headers
                                          (lhc-util:merge-headers headers
                                                                  (maps:get 'headers opts '()))
                                          opts)))
         (backend (maps:get 'backend merged-opts 'httpc)))
    (case backend
      ('httpc
       (lhc-httpc:do-request-async method uri body
                                   (maps:get 'headers merged-opts '())
                                   merged-opts))
      ('gun
       (case (lhc-gun:open-connection uri merged-opts)
         (`#(ok ,conn)
          ;; Store connection for later cleanup
          (let ((`#(ok ,req-id ,stream)
                 (lhc-gun:do-request-async conn method (extract-path uri)
                                          body headers merged-opts)))
            ;; TODO: Register connection/stream for cleanup
            `#(ok ,req-id)))
         (error error)))
      (_
       (lhc-error:normalize 'lhc `#(unknown-backend ,backend))))))
```

2. **Await Implementation:**

```lfe
(defun await (request-id timeout)
  ;; Collect messages and build response
  (receive
    ;; httpc async response
    (`#(http #(,request-id ,result))
     (lhc-httpc:normalize-response result))

    ;; gun response start
    (`#(gun_response ,_conn ,stream-ref ,_fin ,status ,headers)
     (when (=:= stream-ref (get-stream-ref request-id)))
     ;; Continue collecting body
     (await-gun-body request-id status headers timeout))

    ;; Errors
    (`#(http #(,request-id error ,reason))
     (lhc-error:normalize 'httpc reason))

    (`#(gun_error ,_conn ,stream-ref ,reason)
     (when (=:= stream-ref (get-stream-ref request-id)))
     (lhc-error:normalize 'gun reason))

    (after timeout
      (lhc-error:normalize 'lhc
                          `#(timeout ,(binary "Request timeout after "
                                              (integer_to_binary timeout)
                                              "ms"))))))

(defun await-gun-body (request-id status headers timeout)
  ;; Collect data messages
  (await-gun-body request-id status headers timeout (binary)))

(defun await-gun-body (request-id status headers timeout acc)
  (receive
    (`#(gun_data ,_conn ,stream-ref fin ,data)
     (when (=:= stream-ref (get-stream-ref request-id)))
     ;; Complete
     `#(ok ,(maps:from_list
              `(#(status ,status)
                #(headers ,(lhc-gun:normalize-headers headers))
                #(body ,(binary acc data))))))

    (`#(gun_data ,_conn ,stream-ref nofin ,data)
     (when (=:= stream-ref (get-stream-ref request-id)))
     ;; More data coming
     (await-gun-body request-id status headers timeout (binary acc data)))

    (`#(gun_trailers ,_conn ,stream-ref ,trailers)
     (when (=:= stream-ref (get-stream-ref request-id)))
     ;; Complete with trailers
     `#(ok ,(maps:from_list
              `(#(status ,status)
                #(headers ,(++ (lhc-gun:normalize-headers headers)
                              (lhc-gun:normalize-headers trailers)))
                #(body ,acc)))))

    (after timeout
      (lhc-error:normalize 'lhc `#(timeout "Body timeout")))))
```

3. **Cancel Implementation:**

```lfe
(defun cancel (request-id)
  (let ((backend (get-backend-for-request request-id)))
    (case backend
      ('httpc
       (httpc:cancel_request request-id))
      ('gun
       (let* ((conn (get-conn-for-request request-id))
              (stream (get-stream-ref request-id)))
         (gun:cancel conn stream)))
      (_ 'ok))
    'ok))
```

**Critical Implementation Details:**

- Need to maintain a registry mapping request-id to (backend, conn, stream-ref)
- Consider using ETS table or process dictionary for this
- Clean up registry entries after await completes or timeout
- Handle gun_down messages gracefully
- Implement proper message flushing

#### 3.3 Public API Dispatcher (`src/lhc.lfe`)

Rewrite main module to dispatch based on options.

**Module Structure:**

```lfe
(defmodule lhc
  (export
   ;; Lifecycle
   (start 0) (start 1)
   (stop 0)

   ;; Connection management
   (open 2)
   (close 1)
   (shutdown 1)

   ;; HTTP methods - auto-dispatch sync/async
   (get 2)
   (head 2)
   (post 3)
   (put 3)
   (patch 3)
   (delete 2)
   (options 2)
   (request 5)
   (request-with-conn 6)

   ;; Async handling
   (await 2)
   (await-body 2)
   (cancel 1)
   (flush 1)

   ;; Streaming
   (stream-body 4)
   (update-flow 3)

   ;; Connection info
   (info 1)
   (stream-info 2)
   (ping 1)

   ;; Cookies
   (cookie-header 2)
   (store-cookies 3)
   (cookies 1)
   (reset-cookies 1)

   ;; WebSocket
   (ws-upgrade 3)
   (ws-send 3)

   ;; CONNECT
   (connect 3)

   ;; Config
   (set-options 2)
   (get-options 1)

   ;; Utilities
   (normalize-headers 1)
   (parse-uri 1)
   (build-uri 1)

   ;; Metadata
   (version 0)
   (versions 0)
   ))
```

**Dispatch Logic:**

```lfe
(defun get (uri opts)
  (dispatch-request 'get (list uri opts)))

(defun post (uri body opts)
  (dispatch-request 'post (list uri body opts)))

;; ... similar for other methods

(defun dispatch-request (method args)
  ;; Determine if sync or async based on options
  (let* ((opts (extract-opts args))
         (mode (determine-mode opts)))
    (case mode
      ('sync
       (apply 'lhc-sync method args))
      ('async
       (apply 'lhc-async method args)))))

(defun determine-mode (opts)
  ;; Explicit mode takes precedence
  (case (maps:get 'mode opts 'undefined)
    ('sync) 'sync)
    ('async) 'async)
    ('undefined
     ;; Infer from options
     (if (has-async-options? opts)
       'async
       'sync))))

(defun has-async-options? (opts)
  ;; Check for async-specific options
  (or (maps:is_key 'receiver opts)
      (maps:is_key 'stream-to opts)
      (=:= (maps:get 'mode opts 'sync) 'async)))
```

**Connection Management:**

```lfe
(defun open (uri opts)
  (let ((backend (maps:get 'backend opts 'httpc)))
    (case backend
      ('httpc
       ;; For httpc, "opening" means setting up profile
       (let ((profile (maps:get 'profile
                                (maps:get 'httpc-opts opts #{})
                                'default)))
         (lhc-httpc:create-profile profile opts)
         `#(ok ,profile)))
      ('gun
       (lhc-gun:open-connection uri opts))
      (_
       (lhc-error:normalize 'lhc `#(unknown-backend ,backend))))))

(defun close (conn-ref)
  ;; Determine backend from conn-ref type
  (cond
    ((is_pid conn-ref)
     ;; It's a gun connection
     (lhc-gun:close-connection conn-ref))
    ((is_atom conn-ref)
     ;; It's an httpc profile - no explicit close needed
     'ok)
    ('true
     'ok)))
```

### Phase 4: Additional Features (Priority 2)

#### 4.1 Cookie Management (`src/lhc-cookies.lfe`)

Wrap gun-cookies for unified interface.

**Module Structure:**

```lfe
(defmodule lhc-cookies
  (export
   (init 1)              ; Initialize cookie store
   (query 2)             ; Get cookies for URI
   (store 3)             ; Store cookies from response
   (list-all 1)          ; List all cookies
   (clear 1)             ; Clear all cookies
   (gc 1)                ; Garbage collect expired
   ))
```

**Implementation:**

```lfe
(defun init (opts)
  ;; Initialize gun_cookies_list store
  (gun_cookies_list:init opts))

(defun query (store uri)
  ;; Get cookies for URI
  (case (gun_cookies:query store (lhc-util:parse-uri uri))
    (`#(ok ,cookies ,new-store)
     `#(ok ,cookies ,new-store))
    (error error)))

(defun store (store cookies uri)
  ;; Store Set-Cookie headers
  (gun_cookies:store store cookies uri))

;; ... etc
```

**Integration with Requests:**

- If cookie-store is configured in opts, automatically:
  1. Query store before request, add Cookie header
  2. After response, extract Set-Cookie headers and store
- This should be transparent in lhc-httpc and lhc-gun modules

#### 4.2 WebSocket Support (`src/lhc-websocket.lfe`)

Gun-only WebSocket functionality.

**Module Structure:**

```lfe
(defmodule lhc-websocket
  (export
   (upgrade 3)           ; Upgrade connection to WebSocket
   (send-frame 3)        ; Send WebSocket frame
   (close 3)             ; Close WebSocket
   (normalize-frame 1)   ; Normalize gun frame to lhc format
   ))
```

**Implementation:**

```lfe
(defun upgrade (conn path opts)
  (let* ((ws-opts (maps:get 'ws-opts (maps:get 'gun-opts opts #{}) #{}))
         (headers (maps:get 'headers opts '()))
         (gun-ws-opts (make-gun-ws-opts ws-opts)))
    (let ((stream-ref (gun:ws_upgrade conn path headers gun-ws-opts)))
      ;; In async mode, caller waits for gun_upgrade message
      ;; In sync mode, wait for it here
      (case (maps:get 'mode opts 'sync)
        ('sync
         (receive
           (`#(gun_upgrade ,_conn ,stream-ref ,_protocols ,_headers)
            `#(ok ,stream-ref))
           (`#(gun_error ,_conn ,stream-ref ,reason)
            (lhc-error:normalize 'gun reason))
           (after 5000
             (lhc-error:normalize 'gun `#(ws-upgrade-timeout)))))
        ('async
         `#(ok ,stream-ref))))))

(defun send-frame (conn stream frame-map)
  ;; Convert lhc frame format to gun format
  (let ((gun-frame (lhc-to-gun-frame frame-map)))
    (gun:ws_send conn stream gun-frame)
    'ok))

(defun lhc-to-gun-frame
  ((#M(type text data data))
   `#(text ,data))
  ((#M(type binary data data))
   `#(binary ,data))
  ((#M(type ping))
   'ping)
  ((#M(type pong))
   'pong)
  ((#M(type close))
   'close)
  ((#M(type close code code reason reason))
   `#(close ,code ,reason)))

(defun normalize-frame
  ((`#(text ,data))
   #M(type text data data))
  ((`#(binary ,data))
   #M(type binary data data))
  (('ping)
   #M(type ping))
  (('pong)
   #M(type pong))
  (('close)
   #M(type close))
  ((`#(close ,code ,reason))
   #M(type close code code reason reason)))
```

### Phase 5: Testing (Priority 1)

Create comprehensive test suite.

#### 5.1 Unit Tests

Test each module in isolation with mocked dependencies.

**Test Files:**

- `test/unit-lhc-error-tests.lfe` - Error normalization
- `test/unit-lhc-util-tests.lfe` - Extend existing with new functions
- `test/unit-lhc-httpc-tests.lfe` - httpc backend (with mocked httpc)
- `test/unit-lhc-gun-tests.lfe` - gun backend (with mocked gun)
- `test/unit-lhc-sync-tests.lfe` - Sync operations
- `test/unit-lhc-async-tests.lfe` - Async operations
- `test/unit-lhc-cookies-tests.lfe` - Cookie management
- `test/unit-lhc-websocket-tests.lfe` - WebSocket operations

**Testing Strategy:**

1. Use `meck` or similar for mocking Erlang modules
2. Test error paths extensively
3. Test option parsing and conversion
4. Test response normalization
5. Test message handling for async

**Example Test Structure:**

```lfe
(defmodule unit-lhc-error-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest normalize-httpc-timeout
  (let ((result (lhc-error:normalize 'httpc `#(error timeout))))
    (is-equal 'error (element 1 result))
    (let ((error-map (element 2 result)))
      (is-equal 'timeout (maps:get 'reason error-map))
      (is-equal 'httpc (maps:get 'backend error-map))
      (is (maps:is_key 'message error-map))
      (is (maps:is_key 'details error-map)))))

(deftest normalize-gun-nxdomain
  (let ((result (lhc-error:normalize 'gun `#(error nxdomain))))
    (is-equal 'error (element 1 result))
    (let ((error-map (element 2 result)))
      (is-equal 'connection (maps:get 'reason error-map)))))

;; ... more tests
```

#### 5.2 Integration Tests

Test with real HTTP servers.

**Test Files:**

- `test/integration-httpc-tests.lfe` - httpc against real servers
- `test/integration-gun-tests.lfe` - gun against real servers
- `test/integration-websocket-tests.lfe` - WebSocket against ws://echo.websocket.org

**Testing Strategy:**

1. Use httpbin.org or similar for HTTP testing
2. Use echo.websocket.org for WebSocket testing
3. Test both sync and async modes
4. Test cookies, redirects, errors
5. Test HTTP/1.1 and HTTP/2 (gun only)

**Example:**

```lfe
(defmodule integration-gun-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest gun-http2-get
  (let* ((opts (map 'backend 'gun
                    'gun-opts (map 'protocols '(http2))))
         ((tuple 'ok response) (lhc:get "https://http2.golang.org/reqinfo" opts)))
    (is-equal 200 (maps:get 'status response))
    (is (is_binary (maps:get 'body response)))))

(deftest gun-websocket-echo
  (let* ((opts (map 'backend 'gun))
         ((tuple 'ok conn) (lhc:open "wss://echo.websocket.org" opts))
         ((tuple 'ok stream) (lhc:ws-upgrade conn "/" opts)))
    (lhc:ws-send conn stream (map 'type 'text 'data "Hello WebSocket!"))
    (receive
      ((tuple 'ws (tuple stream (= (map type 'text data response) _)))
       (is-equal "Hello WebSocket!" (binary_to_list response))
       (lhc:close conn))
      (after 5000
        (lhc:close conn)
        (error "WebSocket timeout")))))
```

### Phase 6: Documentation (Priority 2)

#### 6.1 Module Documentation

Add comprehensive docstrings to all public functions.

**Format:**

```lfe
(defun get (uri opts)
  "Perform HTTP GET request.

  Args:
    uri (binary): Request URI
    opts (map): Request options (see lhc-types)

  Returns:
    Sync mode: {ok, response-map} or {error, error-map}
    Async mode: {ok, request-id}

  Examples:
    ;; Synchronous GET
    (lhc:get \"https://api.example.com/users\"
             (map 'mode 'sync))

    ;; Asynchronous GET
    (lhc:get \"https://api.example.com/users\"
             (map 'mode 'async 'receiver (self)))

  See also: lhc:request/5, lhc-sync:get/2, lhc-async:get/2"
  ...)
```

#### 6.2 User Guide

Create comprehensive user documentation.

**Files to Create:**

- `docs/getting-started.md` - Installation, basic usage
- `docs/api-reference.md` - Generated from API spec
- `docs/backends.md` - Backend comparison, when to use which
- `docs/async.md` - Guide to async operations
- `docs/websockets.md` - WebSocket guide
- `docs/cookies.md` - Cookie management guide
- `docs/migration.md` - Migrating from 0.4.x to 2.0.0
- `docs/examples.md` - Comprehensive examples

#### 6.3 Migration Guide

Help users upgrade from old API.

**Content:**

- Breaking changes list
- API mapping (old → new)
- Common migration patterns
- Deprecation warnings (if any functions kept for compat)

### Phase 7: Polish (Priority 3)

#### 7.1 Backward Compatibility Layer (Optional)

Consider providing `lhc-compat` module for easier migration:

```lfe
(defmodule lhc-compat
  "Compatibility layer for lhc 0.4.x API"
  (export all))

(defun get
  ;; Old signature: (get url)
  ((url) (when (is_list url))
   (lhc:get (list_to_binary url) (map)))
  ;; Old signature: (get url headers)
  ((url headers) (when (is_list url) (is_list headers))
   (lhc:get (list_to_binary url)
            (map 'headers (convert-headers headers)))))

;; ... etc
```

#### 7.2 Performance Optimization

- Profile common operations
- Optimize header normalization (it's done frequently)
- Consider ETS for request registry instead of process dictionary
- Pool gun connections if frequently opening/closing

#### 7.3 Telemetry/Observability

Consider adding telemetry events for:

- Request start/end
- Connection open/close
- Errors
- Latency metrics

## Implementation Guidelines

### Code Style

1. **Naming Conventions:**
   - Use kebab-case for functions: `do-request`, `make-options`
   - Use descriptive names: avoid abbreviations except common ones (uri, opts, conn)
   - Private functions: prefix with internal name if needed, but export all is fine for LFE

2. **Pattern Matching:**
   - Use pattern matching extensively in function heads
   - Prefer pattern matching over conditionals when possible
   - Use guards for type checks

3. **Maps vs Proplists:**
   - Use maps for all new code (opts, responses, etc.)
   - Convert proplists to maps when received
   - Use `#M()` map syntax for pattern matching

4. **Error Handling:**
   - Always return `{ok, Result}` or `{error, ErrorMap}`
   - Never throw exceptions from public API
   - Use lhc-error:normalize for all errors
   - Preserve original error in details field

5. **Documentation:**
   - Docstring for every public function
   - Include type information
   - Provide examples
   - Cross-reference related functions

### Testing Requirements

1. **Coverage:**
   - Aim for >80% code coverage
   - Test all error paths
   - Test edge cases (empty body, no headers, etc.)

2. **Test Organization:**
   - One test module per source module
   - Group related tests with descriptive names
   - Use setup/teardown for common fixtures

3. **Integration Tests:**
   - Should be runnable offline (mock external services if needed)
   - Use configurable endpoints (env vars)
   - Clean up connections properly

### Dependencies Management

Current dependencies:

- `lfe` 2.1.1 (LFE compiler)
- `lutil` 0.13.2 (LFE utilities)

Add:

- `gun` 2.2.0 (HTTP/2, HTTP/1.1, WebSocket client)

Keep:

- `inets` (built-in, for httpc)
- `ssl` (built-in, for TLS)

### Build Configuration

Update `rebar.config`:

```erlang
{erl_opts, [
  debug_info,
  {no_auto_import, [{get,1}]}  % Keep existing
]}.

{deps, [
  {lfe, "2.1.1"},
  {lutil, "0.13.2"},
  {gun, "2.2.0"}   % NEW
]}.

{plugins, [
  {rebar3_lfe, "0.4.4"}
]}.

{provider_hooks, [
  {pre, [{compile, {lfe, compile}}]}
]}.

{profiles, [
  {test, [
    {deps, [
      {proper, "1.4.0"},
      {ltest, "0.13.4"},
      {meck, "0.9.2"}    % NEW - for mocking
    ]},
    {eunit_opts, [verbose]},
    {erl_opts, [{src_dirs, ["src", "test"]}]}
  ]}
]}.
```

Update `.app.src`:

```erlang
{application, lhc, [
  {description, "LFE Little HTTP Client"},
  {vsn, "2.0.0"},   % MAJOR VERSION BUMP
  {modules, [
    lhc,
    'lhc-sync',
    'lhc-async',
    'lhc-httpc',
    'lhc-gun',
    'lhc-cookies',
    'lhc-websocket',
    'lhc-error',
    'lhc-util',
    'lhc-vsn'
  ]},
  {registered, []},
  {applications, [
    kernel,
    stdlib,
    inets,
    ssl,
    gun      % NEW
  ]},
  {included_applications, []},
  {env, [
    {default_backend, httpc}  % NEW - configurable default
  ]},
  {licenses, ["Apache-2"]},
  {links, [
    {"GitHub", "https://github.com/lfe-http/lhc"},
    {"Gitlab", "https://gitlab.com/lfex/lhc"},
    {"Hex", "https://hex.pm/packages/lhc"}
  ]}
]}.
```

## Validation Checklist

Before considering the implementation complete, verify:

### Functional Requirements

- [ ] All HTTP methods work (GET, POST, PUT, PATCH, DELETE, HEAD, OPTIONS)
- [ ] Both sync and async modes work correctly
- [ ] Both httpc and gun backends work
- [ ] WebSocket support works (gun only, httpc returns not-implemented)
- [ ] Cookie management works
- [ ] Error normalization works for all error types
- [ ] Connection management (open/close) works
- [ ] Streaming (request and response) works
- [ ] Flow control works (gun only)
- [ ] CONNECT tunneling works (gun only)

### Non-Functional Requirements

- [ ] Code follows LFE/Erlang style conventions
- [ ] All public functions have docstrings
- [ ] Test coverage >80%
- [ ] All tests pass
- [ ] No compiler warnings
- [ ] Documentation is complete and accurate
- [ ] Examples in docs actually work
- [ ] Migration guide is helpful

### Integration Requirements

- [ ] Works with latest LFE (2.1.1+)
- [ ] Works with Erlang 24-27
- [ ] Works with gun 2.2.0
- [ ] Works with httpc (built-in to Erlang)
- [ ] rebar3 integration works
- [ ] Hex package metadata is correct

## Troubleshooting Common Issues

### Issue: gun connection doesn't establish

**Solution:** Make sure to call `gun:await_up/2` after `gun:open/2`. The connection is asynchronous.

### Issue: httpc returns error on PATCH

**Solution:** This is expected. httpc doesn't support PATCH. Return a not-implemented error.

### Issue: Messages not received in async mode

**Solution:** Check that receiver process is correct. Gun messages go to connection owner by default.

### Issue: Memory leak with gun connections

**Solution:** Always close gun connections. They are processes that need explicit cleanup.

### Issue: Headers not working

**Solution:** Remember to lowercase header names for HTTP/2. Use lhc-util:normalize-headers.

### Issue: Cookie store not persisting

**Solution:** gun_cookies_list is in-memory only. For persistence, need custom backend.

## Timeline Estimate

Assuming one developer working full-time:

- **Phase 1** (Foundation): 2-3 days
- **Phase 2** (Backends): 4-5 days
- **Phase 3** (API Layers): 3-4 days
- **Phase 4** (Features): 2-3 days
- **Phase 5** (Testing): 3-4 days
- **Phase 6** (Documentation): 2-3 days
- **Phase 7** (Polish): 1-2 days

**Total: ~3-4 weeks**

Phases 1-3 are critical path. Phases 4-7 can be parallelized or done incrementally.

## Success Criteria

The implementation is successful when:

1. **All functional requirements met** - Every feature in the API spec works
2. **Tests pass** - >80% coverage, all tests green
3. **Documentation complete** - Users can learn and use the library
4. **Examples work** - All code examples in docs execute successfully
5. **Backward compatibility considered** - Migration path is clear
6. **Performance acceptable** - No obvious performance issues
7. **Ready for release** - Can be published to Hex without issues

## Next Steps After Implementation

1. **Alpha release** - Get feedback from early adopters
2. **Beta release** - Wider testing, fix issues
3. **1.0 release** - Stable, production-ready
4. **Ongoing** - Maintain compatibility with gun/httpc updates
5. **Future** - Consider additional backends (mint, etc.)

---

## Quick Reference: Module Dependencies

```
lhc (public API)
├── lhc-sync
│   ├── lhc-httpc
│   │   ├── lhc-error
│   │   └── lhc-util
│   └── lhc-gun
│       ├── lhc-error
│       └── lhc-util
├── lhc-async
│   ├── lhc-httpc
│   └── lhc-gun
├── lhc-websocket
│   └── lhc-gun
├── lhc-cookies
│   └── gun_cookies (external)
├── lhc-error
└── lhc-util

lhc-vsn (independent, metadata only)
```

Build order:

1. lhc-util, lhc-error (no dependencies on other lhc modules)
2. lhc-httpc, lhc-gun (depend on util and error)
3. lhc-sync, lhc-async (depend on backends)
4. lhc-cookies, lhc-websocket (depend on gun)
5. lhc (depends on everything)

---

**This completes the implementation instructions. Refer to `lhc-api.md` for detailed API specifications while implementing each module. Good luck!**
