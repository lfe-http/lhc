# LHC API Reference

Complete API reference for LHC 2.0.

## Table of Contents

* [lhc - Main API](#lhc---main-api)
* [lhc-async - Async Operations](#lhc-async---async-operations)
* [lhc-cookies - Cookie Management](#lhc-cookies---cookie-management)
* [lhc-websocket - WebSocket Support](#lhc-websocket---websocket-support)
* [lhc-error - Error Handling](#lhc-error---error-handling)
* [Common Types](#common-types)

## lhc - Main API

Main public API for LHC. Provides a unified interface for HTTP requests that automatically dispatches to synchronous or asynchronous operations based on options.

### Lifecycle Functions

#### `(lhc:start)`

Start the LHC application with default backend.

* **Returns:** `{ok, [started-apps]}` or `{error, reason}`

#### `(lhc:start backend)`

Start the LHC application with specified backend.

* **Parameters:**
  * `backend` - Backend to use (`'httpc` or `'gun`)
* **Returns:** `{ok, [started-apps]}` or `{error, reason}`

#### `(lhc:stop)`

Stop the LHC application.

* **Returns:** `ok` or `{error, reason}`

### Connection Management

#### `(lhc:open uri opts)`

Open a connection to a server.

* **Parameters:**
  * `uri` - Server URI (string or binary)
  * `opts` - Options map
* **Returns:** `{ok, conn-ref}` or `{error, error-map}`
* **Note:** For httpc: Creates/configures a profile. For gun: Opens a TCP/TLS connection.

#### `(lhc:close conn)`

Close a connection.

* **Parameters:**
  * `conn` - Connection reference (gun pid or httpc profile)
* **Returns:** `ok`

#### `(lhc:shutdown conn)`

Shutdown a connection immediately (gun only).

* **Parameters:**
  * `conn` - Connection reference
* **Returns:** `ok`

### HTTP Methods

All HTTP methods auto-dispatch between sync and async based on options:
* Explicit: `(map 'mode 'sync)` or `(map 'mode 'async)`
* Implicit: async if `'receiver` or `'stream-to` is set

#### `(lhc:get uri opts)`

Perform an HTTP GET request.

* **Parameters:**
  * `uri` - Request URI (string or binary)
  * `opts` - Options map
* **Returns:**
  * Sync: `{ok, response-map}` or `{error, error-map}`
  * Async: `{ok, request-id}`

#### `(lhc:head uri opts)`

Perform an HTTP HEAD request.

* **Parameters:**
  * `uri` - Request URI
  * `opts` - Options map
* **Returns:** Same as GET

#### `(lhc:post uri body opts)`

Perform an HTTP POST request.

* **Parameters:**
  * `uri` - Request URI
  * `body` - Request body (binary or iolist)
  * `opts` - Options map
* **Returns:** Same as GET

#### `(lhc:put uri body opts)`

Perform an HTTP PUT request.

* **Parameters:**
  * `uri` - Request URI
  * `body` - Request body (binary or iolist)
  * `opts` - Options map
* **Returns:** Same as GET

#### `(lhc:patch uri body opts)`

Perform an HTTP PATCH request.

* **Parameters:**
  * `uri` - Request URI
  * `body` - Request body (binary or iolist)
  * `opts` - Options map
* **Returns:** Same as GET
* **Note:** Requires gun backend. httpc does not support PATCH.

#### `(lhc:delete uri opts)`

Perform an HTTP DELETE request.

* **Parameters:**
  * `uri` - Request URI
  * `opts` - Options map
* **Returns:** Same as GET

#### `(lhc:options uri opts)`

Perform an HTTP OPTIONS request.

* **Parameters:**
  * `uri` - Request URI
  * `opts` - Options map
* **Returns:** Same as GET

#### `(lhc:trace uri opts)`

Perform an HTTP TRACE request.

* **Parameters:**
  * `uri` - Request URI
  * `opts` - Options map
* **Returns:** Same as GET

### Generic Request

#### `(lhc:request method uri body headers opts)`

Perform an HTTP request with full control.

* **Parameters:**
  * `method` - HTTP method atom (`'GET`, `'POST`, etc.)
  * `uri` - Request URI
  * `body` - Request body (binary or iolist)
  * `headers` - Request headers (list or map)
  * `opts` - Options map
* **Returns:** Same as GET

#### `(lhc:request-with-conn conn method uri body headers opts)`

Perform an HTTP request with an existing connection.

* **Parameters:**
  * `conn` - Connection reference
  * `method` - HTTP method atom
  * `uri` - Request URI or path
  * `body` - Request body
  * `headers` - Request headers
  * `opts` - Options map
* **Returns:** Same as GET

### Async Utilities

#### `(lhc:await request-id timeout)`

Wait for an async response.

* **Parameters:**
  * `request-id` - Request identifier from async request
  * `timeout` - Timeout in milliseconds
* **Returns:** `{ok, response-map}` or `{error, error-map}`

#### `(lhc:await request-id timeout auto-close)`

Wait for an async response with auto-close option.

* **Parameters:**
  * `request-id` - Request identifier
  * `timeout` - Timeout in milliseconds
  * `auto-close` - Auto-close gun connection after response (boolean)
* **Returns:** `{ok, response-map}` or `{error, error-map}`

#### `(lhc:cancel request-id)`

Cancel an async request.

* **Parameters:**
  * `request-id` - Request identifier
* **Returns:** `ok`

#### `(lhc:flush request-id)`

Flush messages for an async request.

* **Parameters:**
  * `request-id` - Request identifier
* **Returns:** `ok`

### Utility Functions

#### `(lhc:normalize-headers headers)`

Normalize headers to standard binary tuple format.

* **Parameters:**
  * `headers` - List of tuples or map
* **Returns:** List of `{binary, binary}` tuples with lowercase names

#### `(lhc:parse-uri uri)`

Parse a URI string into component map.

* **Parameters:**
  * `uri` - URI (binary or string)
* **Returns:** `{ok, uri-map}` or `{error, error-map}`

#### `(lhc:build-uri uri-map)`

Build a URI string from component map.

* **Parameters:**
  * `uri-map` - Map with keys: `'scheme`, `'host`, `'port`, `'path`, `'query`, `'fragment`
* **Returns:** `{ok, binary}` or `{error, error-map}`

#### `(lhc:version)`

Get LHC version string.

* **Returns:** Version string

#### `(lhc:versions)`

Get version information for LHC and dependencies.

* **Returns:** Map of component versions

## lhc-async - Async Operations

Asynchronous HTTP operations. All request functions return immediately with `{ok, request-id}`.

### Message Formats

* **httpc:** `{http, {request-id, result}}`
* **gun:**
  * `{gun_response, conn, stream-ref, fin, status, headers}`
  * `{gun_data, conn, stream-ref, fin, data}`
  * `{gun_error, conn, stream-ref, reason}`

### HTTP Methods

All async methods have the same pattern:

#### `(lhc-async:get uri opts)`

Perform an asynchronous HTTP GET request.

* **Parameters:**
  * `uri` - Request URI
  * `opts` - Options map (should include `'receiver` or defaults to `(self)`)
* **Returns:** `{ok, request-id}`

#### `(lhc-async:head uri opts)`

Asynchronous HTTP HEAD request.

* **Returns:** `{ok, request-id}`

#### `(lhc-async:post uri body opts)`

Asynchronous HTTP POST request.

* **Returns:** `{ok, request-id}`

#### `(lhc-async:put uri body opts)`

Asynchronous HTTP PUT request.

* **Returns:** `{ok, request-id}`

#### `(lhc-async:patch uri body opts)`

Asynchronous HTTP PATCH request.

* **Returns:** `{ok, request-id}`

#### `(lhc-async:delete uri opts)`

Asynchronous HTTP DELETE request.

* **Returns:** `{ok, request-id}`

#### `(lhc-async:options uri opts)`

Asynchronous HTTP OPTIONS request.

* **Returns:** `{ok, request-id}`

#### `(lhc-async:trace uri opts)`

Asynchronous HTTP TRACE request.

* **Returns:** `{ok, request-id}`

### Generic Request

#### `(lhc-async:request method uri body headers opts)`

Perform an asynchronous HTTP request.

* **Parameters:**
  * `method` - HTTP method atom
  * `uri` - Request URI
  * `body` - Request body
  * `headers` - Request headers
  * `opts` - Options map
* **Returns:** `{ok, request-id}` or `{error, error-map}`

#### `(lhc-async:request-with-conn conn method uri body headers opts)`

Asynchronous request with existing connection.

* **Returns:** `{ok, request-id}` or `{error, error-map}`

### Response Handling

#### `(lhc-async:await request-id timeout)`

Wait for and collect an async HTTP response.

* **Parameters:**
  * `request-id` - Request identifier
  * `timeout` - Timeout in milliseconds
* **Returns:** `{ok, response-map}` or `{error, error-map}`
* **Note:** Blocks until response is received or timeout occurs.

#### `(lhc-async:await request-id timeout auto-close)`

Wait for async response with auto-close option.

* **Parameters:**
  * `auto-close` - Close gun connection after receiving response (boolean)
* **Returns:** `{ok, response-map}` or `{error, error-map}`

#### `(lhc-async:cancel request-id)`

Cancel an async request.

* **Parameters:**
  * `request-id` - Request identifier
* **Returns:** `ok`
* **Note:** For httpc: calls `httpc:cancel_request`. For gun: calls `gun:cancel` on the stream.

#### `(lhc-async:flush request-id)`

Flush any remaining messages for a request.

* **Parameters:**
  * `request-id` - Request identifier
* **Returns:** `ok`

## lhc-cookies - Cookie Management

Cookie store management. When using the gun backend, cookies are handled AUTOMATICALLY by Gun.

### Cookie Store Types

* **gun_cookies_list** - In-memory cookie store (default)
* Custom implementations possible (for persistence, sharing, etc.)

### Cookie Store Management

#### `(lhc-cookies:init)`

Initialize a cookie store with default options.

* **Returns:** Cookie store (opaque term)

#### `(lhc-cookies:init opts)`

Initialize a cookie store with options.

* **Parameters:**
  * `opts` - Map with keys:
    * `'type` - Store type (default: `'gun_cookies_list`)
    * `'max-cookies` - Maximum cookies to store
    * `'max-cookies-per-domain` - Max cookies per domain
* **Returns:** Cookie store

### Cookie Operations

#### `(lhc-cookies:query store uri)`

Query cookies for a URI.

* **Parameters:**
  * `store` - Cookie store
  * `uri` - URI (binary or string)
* **Returns:** `{ok, cookie-list, new-store}` or `{error, error-map}`
* **Note:** new-store may differ if expired cookies were removed.

#### `(lhc-cookies:store store set-cookies uri)`

Store cookies from Set-Cookie headers.

* **Parameters:**
  * `store` - Cookie store
  * `set-cookies` - List of Set-Cookie header values (binaries)
  * `uri` - URI where cookies were set
* **Returns:** `{ok, new-store}` or `{error, error-map}`

#### `(lhc-cookies:list-all store)`

List all cookies in the store.

* **Parameters:**
  * `store` - Cookie store
* **Returns:** `{ok, cookie-list}`
* **Note:** Each cookie is a map with keys: `'name`, `'value`, `'domain`, `'path`, `'max-age`, `'secure`, `'http-only`, `'same-site`

#### `(lhc-cookies:clear store)`

Clear all cookies from the store.

* **Parameters:**
  * `store` - Cookie store
* **Returns:** `{ok, new-store}`

#### `(lhc-cookies:gc store)`

Garbage collect expired cookies.

* **Parameters:**
  * `store` - Cookie store
* **Returns:** `{ok, new-store}`

#### `(lhc-cookies:gc store time)`

Garbage collect cookies expired before given time.

* **Parameters:**
  * `store` - Cookie store
  * `time` - Erlang datetime tuple `{{Y,M,D},{H,M,S}}`
* **Returns:** `{ok, new-store}`

### Request/Response Integration

#### `(lhc-cookies:add-cookie-header headers uri store)`

Add Cookie header to request headers.

* **Parameters:**
  * `headers` - Request headers (list of tuples)
  * `uri` - Request URI
  * `store` - Cookie store
* **Returns:** `{ok, new-headers, new-store}` or `{error, error-map}`
* **Note:** Queries cookie store for cookies matching URI and adds Cookie header.

#### `(lhc-cookies:extract-set-cookies headers)`

Extract Set-Cookie headers from response.

* **Parameters:**
  * `headers` - Response headers (list of tuples)
* **Returns:** List of Set-Cookie header values (binaries)
* **Note:** Multiple Set-Cookie headers returned as separate list items.

#### `(lhc-cookies:update-store-from-response store response uri)`

Update cookie store with Set-Cookie headers from response.

* **Parameters:**
  * `store` - Cookie store
  * `response` - Response map with `'headers` key
  * `uri` - Request URI
* **Returns:** `{ok, new-store}` or `{error, error-map}`
* **Note:** Automatically extracts Set-Cookie headers and stores them.

## lhc-websocket - WebSocket Support

WebSocket functionality (gun backend only). httpc does not support WebSocket connections.

### WebSocket Lifecycle

1. Open gun connection to WebSocket endpoint
2. Upgrade connection to WebSocket protocol
3. Send and receive frames
4. Close connection

### Frame Types

* **text** - Text data frame
* **binary** - Binary data frame
* **ping** - Ping control frame
* **pong** - Pong control frame
* **close** - Close connection frame

### WebSocket Lifecycle

#### `(lhc-websocket:upgrade conn path opts)`

Upgrade a gun connection to WebSocket.

* **Parameters:**
  * `conn` - Gun connection pid
  * `path` - WebSocket path (e.g., `"/"`)
  * `opts` - Options map
* **Returns:**
  * Sync mode: `{ok, stream-ref}` or `{error, error-map}`
  * Async mode: `{ok, stream-ref}`
* **Note:** stream-ref identifies this WebSocket stream.

#### `(lhc-websocket:upgrade conn path headers opts)`

Upgrade with custom headers.

* **Parameters:**
  * `conn` - Gun connection pid
  * `path` - WebSocket path
  * `headers` - Additional headers for upgrade request
  * `opts` - Map with keys:
    * `'mode` - `'sync` or `'async` (default: `'sync`)
    * `'protocols` - List of WebSocket subprotocols
    * `'extensions` - WebSocket extensions
    * `'timeout` - Upgrade timeout in ms (sync mode only)
* **Returns:** Same as upgrade/3
* **Note:** Sync mode waits for `gun_upgrade` message. Async mode returns immediately.

#### `(lhc-websocket:close conn stream-ref opts)`

Close a WebSocket connection.

* **Parameters:**
  * `conn` - Gun connection pid
  * `stream-ref` - WebSocket stream reference
  * `opts` - Map with keys:
    * `'code` - Close code (default: 1000 normal closure)
    * `'reason` - Close reason string
* **Returns:** `ok`

### Frame Operations

#### `(lhc-websocket:send conn stream-ref frame)`

Send a WebSocket frame.

* **Parameters:**
  * `conn` - Gun connection pid
  * `stream-ref` - WebSocket stream reference
  * `frame` - Frame map with keys:
    * `'type` - `'text`, `'binary`, `'ping`, `'pong`, or `'close`
    * `'data` - Frame data (for text/binary)
    * `'code` - Close code (for close frames)
    * `'reason` - Close reason (for close frames)
* **Returns:** `ok`

#### `(lhc-websocket:send-text conn stream-ref text)`

Send a text frame.

* **Parameters:**
  * `text` - Text data (binary or string)
* **Returns:** `ok`

#### `(lhc-websocket:send-binary conn stream-ref data)`

Send a binary frame.

* **Parameters:**
  * `data` - Binary data
* **Returns:** `ok`

#### `(lhc-websocket:ping conn stream-ref)`

Send a ping frame.

* **Returns:** `ok`

#### `(lhc-websocket:pong conn stream-ref)`

Send a pong frame.

* **Returns:** `ok`

### Frame Handling

#### `(lhc-websocket:await-frame conn stream-ref timeout)`

Wait for a WebSocket frame.

* **Parameters:**
  * `conn` - Gun connection pid
  * `stream-ref` - WebSocket stream reference
  * `timeout` - Timeout in milliseconds
* **Returns:** `{ok, frame-map}` or `{error, error-map}`
* **Note:** frame-map contains:
  * `'type` - Frame type atom
  * `'data` - Frame data (for text/binary)
  * `'code` - Close code (for close frames)
  * `'reason` - Close reason (for close frames)

#### `(lhc-websocket:normalize-frame frame)`

Normalize gun WebSocket frame to LHC format.

* **Parameters:**
  * `frame` - Gun WebSocket frame
* **Returns:** Frame map with standardized keys

### Frame Construction

#### `(lhc-websocket:text-frame text)`

Create a text frame map.

* **Parameters:**
  * `text` - Text data (binary or string)
* **Returns:** Frame map

#### `(lhc-websocket:binary-frame data)`

Create a binary frame map.

* **Parameters:**
  * `data` - Binary data
* **Returns:** Frame map

#### `(lhc-websocket:close-frame)`

Create a close frame map with default code.

* **Returns:** Frame map

#### `(lhc-websocket:close-frame code reason)`

Create a close frame map with code and reason.

* **Parameters:**
  * `code` - Close code integer
  * `reason` - Close reason (binary or string)
* **Returns:** Frame map

#### `(lhc-websocket:ping-frame)`

Create a ping frame map.

* **Returns:** Frame map

#### `(lhc-websocket:pong-frame)`

Create a pong frame map.

* **Returns:** Frame map

## lhc-error - Error Handling

Error normalization and formatting across backends (httpc and gun).

### Error Categories

* **connection** - Network connection failures
* **timeout** - Request timeout
* **protocol** - HTTP protocol errors
* **parse** - Response parsing errors
* **ssl** - SSL/TLS errors
* **auth** - Authentication errors
* **redirect** - Redirect errors
* **not-implemented** - Feature not supported by backend
* **unknown** - Other errors

### Error Map Structure

All normalized errors: `{error, error-map}` where error-map contains:

* `'reason` - Error category atom
* `'message` - Human-readable binary string
* `'backend` - Backend that generated the error
* `'details` - Original error

### Functions

#### `(lhc-error:normalize backend error)`

Normalize an error from a backend.

* **Parameters:**
  * `backend` - Backend atom (`'httpc`, `'gun`, or `'lhc`)
  * `error` - Raw error from backend
* **Returns:** `{error, error-map}`

#### `(lhc-error:category error-map)`

Extract the error category from an error map.

* **Parameters:**
  * `error-map` - Error map or error tuple `{error, error-map}`
* **Returns:** Error category atom

#### `(lhc-error:format error-map)`

Format an error map into human-readable string.

* **Parameters:**
  * `error-map` - Error map or error tuple
* **Returns:** Formatted binary string
* **Example:** `"[GUN] CONNECTION: Domain name resolution failed"`

#### `(lhc-error:make-error reason-category message backend)`

Create an error map.

* **Parameters:**
  * `reason-category` - Error category atom
  * `message` - Human-readable string/binary
  * `backend` - Backend atom
* **Returns:** Error map
* **Note:** details = reason

#### `(lhc-error:make-error reason-category message backend details)`

Create an error map with details.

* **Parameters:**
  * `details` - Original error details
* **Returns:** Error map

## Common Types

### Options Map

Request options (all optional):

```lfe
(map 'backend 'gun           ; Backend ('httpc or 'gun)
     'mode 'sync             ; Request mode ('sync or 'async)
     'timeout 5000           ; Timeout in milliseconds
     'headers headers        ; Custom headers (list of tuples)
     'cookie-store store     ; Cookie store (gun backend)
     'gun-opts gun-opts      ; Gun-specific options
     'httpc-opts httpc-opts) ; httpc-specific options
```

### Response Map

Successful response:

```lfe
#{status => 200,                           ; HTTP status code (integer)
  headers => [(#"content-type" #"..."), ...], ; Response headers
  body => #"..."}                          ; Response body (binary)
```

### Error Map

Error response:

```lfe
#{reason => 'connection,        ; Error category
  message => #"Connection...",  ; Human-readable message
  backend => 'gun,              ; Backend that generated error
  details => ...}               ; Original error
```

### URI Map

Parsed URI:

```lfe
#{scheme => #"https",
  host => #"example.com",
  port => 443,
  path => #"/path",
  query => #"key=value",
  fragment => #"section"}
```

### Cookie Map

Cookie structure:

```lfe
#{name => #"session",
  value => #"abc123",
  domain => #"example.com",
  path => #"/",
  max-age => 3600,
  secure => true,
  http-only => true,
  same-site => 'lax}
```

### Frame Map

WebSocket frame:

```lfe
#{type => 'text,      ; or 'binary, 'ping, 'pong, 'close
  data => #"..."}     ; For text/binary frames

#{type => 'close,
  code => 1000,
  reason => #"Normal close"}
```
