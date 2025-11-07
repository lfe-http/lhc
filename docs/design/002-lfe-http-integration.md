# LHC HTTP Client Conversion Layer Implementation

## Project Overview

Implement conversion utilities between the `lfe-http/http` library's request/response structures and two underlying HTTP client libraries: Erlang's `inets httpc` and 99's `gun`. This will enable the `lhc` wrapper library to use a unified `lfe-http/http` interface while supporting both backend implementations.

## Architecture

```
User Code (LFE)
      ↓
lfe-http/http (request/response maps)
      ↓
lhc (wrapper library)
      ↓ (conversion layer - THIS IMPLEMENTATION)
inets httpc  |  gun
      ↓           ↓
   Network    Network
```

## Current State Analysis

### lfe-http/http Request Structure
From `src/http.request.lfe`:
```erlang
#{method => binary(),           % e.g., #"GET"
  url => binary(),              % full URL
  body => binary(),             % request body
  headers => map(),             % normalized headers map (lowercase keys)
  path-segments => [binary()],  % URL path split into segments
  query-parsed => map()}        % parsed query parameters
```

Key characteristics:
- Method is uppercase binary (e.g., `#"GET"`, `#"POST"`)
- Headers are in a map with binary keys (lowercase)
- Body is binary
- URL is the full URL including scheme, host, port, path, query

### lfe-http/http Response Structure
From `src/http.response.lfe`:
```erlang
#{status => integer(),          % e.g., 200
  body => binary(),             % response body
  headers => map()}             % response headers map
```

Key characteristics:
- Status is an integer (200, 404, etc.)
- Headers are in a map with binary keys
- Body is binary

### inets httpc Request Format
From `lib/inets/src/http_client/httpc.erl`:
```erlang
% Simple GET
httpc:request(Url)

% Full request
httpc:request(Method, Request, HTTPOptions, Options)
where:
  Method = head | get | put | patch | post | trace | options | delete
  Request = {Url, Headers} 
          | {Url, Headers, ContentType, Body}
  Url = string()
  Headers = [{Field, Value}]  % list of tuples, Field and Value are strings or binaries
  ContentType = string()
  Body = string() | binary() | {Fun, Acc} | {chunkify, Fun, Acc}
  HTTPOptions = [HTTPOption]
  Options = [Option]
```

Important HTTPOptions:
- `{timeout, timeout()}` - request timeout (default: infinity)
- `{connect_timeout, timeout()}` - connection timeout
- `{ssl, [ssl:tls_option()]}` - SSL/TLS options
- `{autoredirect, boolean()}` - auto-follow redirects (default: true)
- `{version, HttpVersion}` - HTTP version string

Important Options:
- `{sync, boolean()}` - synchronous/asynchronous (default: true)
- `{stream, StreamTo}` - streaming mode
- `{body_format, string | binary}` - response body format (default: string)
- `{full_result, boolean()}` - full or simple result (default: true)
- `{headers_as_is, boolean()}` - preserve header case (default: false)

### inets httpc Response Format
From `lib/inets/src/http_client/httpc_response.erl`:
```erlang
% Full result (when full_result = true, the default)
{ok, {{HttpVersion, StatusCode, ReasonPhrase}, Headers, Body}}
where:
  HttpVersion = string()  % "HTTP/1.1"
  StatusCode = integer()
  ReasonPhrase = string()
  Headers = [{Field, Value}]  % list of tuples
  Body = string() | binary()

% Simple result (when full_result = false)
{ok, {StatusCode, Body}}

% Stream result
{ok, saved_to_file}

% Async request ID
{ok, RequestId}

% Error
{error, Reason}
```

### gun Request API
From `src/gun.erl`:
```erlang
% Open connection
{ok, ConnPid} = gun:open(Host, Port)
{ok, ConnPid} = gun:open(Host, Port, Opts)
where:
  Host = string() | inet:ip_address()
  Port = integer()
  Opts = gun:opts()

% Make requests (returns StreamRef)
StreamRef = gun:get(ConnPid, Path)
StreamRef = gun:get(ConnPid, Path, Headers)
StreamRef = gun:get(ConnPid, Path, Headers, ReqOpts)

StreamRef = gun:post(ConnPid, Path, Headers)
StreamRef = gun:post(ConnPid, Path, Headers, Body)
StreamRef = gun:post(ConnPid, Path, Headers, Body, ReqOpts)

StreamRef = gun:request(ConnPid, Method, Path, Headers, Body)
StreamRef = gun:request(ConnPid, Method, Path, Headers, Body, ReqOpts)

where:
  Path = iodata()  % path with query string
  Headers = [{binary(), iodata()}]  % list of tuples
  Body = iodata()
  Method = binary()  % <<"GET">>, <<"POST">>, etc.
```

### gun Response Handling
From `src/gun.erl` and `src/gun_http.erl`:

Gun uses an asynchronous message-passing model. Responses arrive as Erlang messages:

```erlang
% Response headers
{gun_response, ConnPid, StreamRef, fin, StatusCode, Headers}
{gun_response, ConnPid, StreamRef, nofin, StatusCode, Headers}

% Response body data
{gun_data, ConnPid, StreamRef, fin, Data}
{gun_data, ConnPid, StreamRef, nofin, Data}

% Trailers (rare)
{gun_trailers, ConnPid, StreamRef, Trailers}

% Errors
{gun_error, ConnPid, StreamRef, Reason}
{gun_error, ConnPid, Reason}  % connection-level error

% Connection state
{gun_up, ConnPid, Protocol}
{gun_down, ConnPid, Protocol, Reason, KilledStreams}

where:
  StatusCode = integer()
  Headers = [{binary(), binary()}]  % list of tuples
  Data = binary()
```

## Implementation Requirements

### Phase 1: Create Conversion Module Structure

Create `src/lhc.converter.lfe` with the following module structure:

```lisp
(defmodule lhc.converter
  (export
   ;; httpc conversions
   (http-request->httpc-request 1)
   (http-request->httpc-request 2)  ;; with options
   (httpc-response->http-response 1)
   
   ;; gun conversions
   (http-request->gun-request 1)
   (http-request->gun-request 2)  ;; with connection pid
   (gun-response->http-response 1)
   (gun-response->http-response 2)  ;; accumulating body chunks
   
   ;; Helper functions
   (extract-host-port 1)
   (extract-path-query 1)
   (headers-map->list 1)
   (headers-list->map 1)
   (method-binary->atom 1)
   (method-atom->binary 1)))
```

### Phase 2: Implement httpc Conversions

#### 2.1: `http-request->httpc-request/1`

Convert an lfe-http/http request map to httpc format.

**Input:**
```lisp
#{method #"GET"
  url #"https://api.example.com/users?active=true"
  headers #m(#"content-type" #"application/json"
             #"authorization" #"Bearer token123")
  body #""
  path-segments [#"users"]
  query-parsed #m(#"active" #"true")}
```

**Output:**
```erlang
{get, 
 {"https://api.example.com/users?active=true",
  [{"content-type", "application/json"},
   {"authorization", "Bearer token123"}]},
 [],  % HTTPOptions (empty for default)
 [{body_format, binary}]}  % Options (always want binary bodies)
```

**Implementation steps:**
1. Convert method binary to atom: `#"GET"` → `get` (lowercase)
2. Convert URL binary to string: `binary_to_list(Url)`
3. Convert headers map to list of tuples:
   - Extract all key-value pairs from map
   - Convert keys and values to strings/binaries as needed
   - Ensure keys are lowercase strings (httpc expects this)
4. Determine if body is needed:
   - Methods with body: `post`, `put`, `patch`, `delete`
   - Methods without body: `get`, `head`, `options`, `trace`
5. If body needed and present:
   - Extract content-type from headers (or default to `"application/octet-stream"`)
   - Convert body binary to binary (already is binary)
   - Return: `{Url, Headers, ContentType, Body}`
6. If no body needed or empty:
   - Return: `{Url, Headers}`
7. Set default options:
   - Always include `{body_format, binary}` to ensure we get binary responses
   - HTTPOptions should be empty list initially

**Edge cases to handle:**
- Empty headers map → `[]`
- Missing content-type for body requests → default to `"application/octet-stream"`
- Binary method needs uppercase-to-lowercase conversion
- URL must be a string, not binary

#### 2.2: `http-request->httpc-request/2`

Same as above but accepts a second argument for additional httpc options.

**Input:**
```lisp
(http-request->httpc-request 
  request-map
  #m(timeout 5000
     ssl (#m(verify verify_peer
             cacerts ,(public_key:cacerts_get)))))
```

**Implementation:**
1. Call `http-request->httpc-request/1` to get base tuple
2. Merge provided options into HTTPOptions
3. Common options to support:
   - `timeout` → `{timeout, Value}`
   - `connect_timeout` → `{connect_timeout, Value}`
   - `ssl` → `{ssl, SslOpts}`
   - `autoredirect` → `{autoredirect, Bool}`
4. Return tuple with merged options

#### 2.3: `httpc-response->http-response/1`

Convert httpc response to lfe-http/http format.

**Input (full result):**
```erlang
{ok, {{"HTTP/1.1", 200, "OK"},
      [{"content-type", "application/json"},
       {"content-length", "42"}],
      <<"{\"status\":\"ok\"}">>}}
```

**Output:**
```lisp
#m(status 200
   headers #m(#"content-type" #"application/json"
              #"content-length" #"42")
   body #"{\"status\":\"ok\"}")
```

**Implementation steps:**
1. Pattern match on response format:
   - `{ok, {{_, StatusCode, _}, HeadersList, Body}}` → full result
   - `{ok, {StatusCode, Body}}` → simple result
   - `{ok, saved_to_file}` → handle specially (may need to read file)
   - `{error, Reason}` → convert to error map
2. Extract status code (integer)
3. Convert headers list to map:
   - Convert each `{Field, Value}` to binary keys
   - Normalize keys to lowercase
   - Build map with all headers
4. Ensure body is binary (may need conversion from string)
5. Return map with three keys: `status`, `headers`, `body`

**Edge cases:**
- Headers may have string or binary values
- Body may be string (if `body_format` not set) → convert to binary
- Handle `saved_to_file` case (body might be on disk)
- Error responses: `{error, Reason}` → `#m(error Reason)`

### Phase 3: Implement gun Conversions

#### 3.1: `http-request->gun-request/1`

This is more complex because gun requires:
1. An open connection (pid)
2. Separate host/port extraction
3. Path with query string

This function should return a tuple with connection info and request parts:

**Input:**
```lisp
#{method #"POST"
  url #"https://api.example.com:443/users?active=true"
  headers #m(#"content-type" #"application/json")
  body #"{\"name\":\"Alice\"}"
  path-segments [#"users"]
  query-parsed #m(#"active" #"true")}
```

**Output:**
```lisp
#m(scheme https
   host "api.example.com"
   port 443
   method #"POST"
   path "/users?active=true"
   headers [(#"content-type" #"application/json")]
   body #"{\"name\":\"Alice\"}")
```

**Implementation steps:**
1. Parse URL to extract:
   - Scheme (http or https)
   - Host (string or IP address)
   - Port (integer, default 80 for http, 443 for https)
2. Build path string:
   - Join path-segments with "/"
   - Append "?" + query string if query-parsed is not empty
   - Ensure leading "/"
3. Convert headers map to list of tuples with binary keys and values
4. Keep method as binary (gun expects binary)
5. Return map with all components

**Helper function needed:** `extract-host-port/1`
```lisp
(defun extract-host-port (url)
  "Parse URL and extract scheme, host, port"
  (let* ((parsed (uri_string:parse (binary_to_list url)))
         (scheme (maps:get 'scheme parsed))
         (host (maps:get 'host parsed))
         (port (maps:get 'port parsed 
                 (default-port scheme))))
    #m(scheme (list_to_atom scheme)
       host host
       port port)))

(defun default-port
  (["http"] 80)
  (["https"] 443))
```

**Helper function needed:** `extract-path-query/1`
```lisp
(defun extract-path-query (url-map)
  "Build path with query string from parsed URL or request map"
  (let* ((path-segs (maps:get 'path-segments url-map '()))
         (query-map (maps:get 'query-parsed url-map #m()))
         (path (build-path path-segs))
         (query (build-query query-map)))
    (if (== query #"")
        path
        (binary (path binary) #"?" (query binary)))))
```

#### 3.2: `http-request->gun-request/2`

Same as above but with an open connection pid.

**Input:**
```lisp
(http-request->gun-request request-map conn-pid)
```

**Output:**
```lisp
#m(conn-pid pid
   method #"POST"
   path "/users?active=true"
   headers [(#"content-type" #"application/json")]
   body #"{\"name\":\"Alice\"}")
```

This variant assumes connection is already open and just returns request parts ready for `gun:request/5`.

#### 3.3: `gun-response->http-response/1` and `/2`

Gun responses arrive as messages and may be split across multiple messages. This converter needs to handle:
1. Initial response with headers
2. Accumulating body chunks
3. Final response

**Pattern 1: Single message with complete response**
```erlang
{gun_response, ConnPid, StreamRef, fin, 200,
 [{<<"content-type">>, <<"application/json">>}]}
```

**Pattern 2: Multi-message response**
```erlang
{gun_response, ConnPid, StreamRef, nofin, 200,
 [{<<"content-type">>, <<"application/json">>}]}
% ... later ...
{gun_data, ConnPid, StreamRef, nofin, <<"first chunk">>}
{gun_data, ConnPid, StreamRef, fin, <<"last chunk">>}
```

**Implementation for `/1` (single message):**
```lisp
(defun gun-response->http-response
  ([`#(gun_response ,_ ,_ fin ,status ,headers)]
   ;; Complete response with no body
   #m(status status
      headers (headers-list->map headers)
      body #""))
  
  ([`#(gun_response ,_ ,_ nofin ,status ,headers)]
   ;; Incomplete response, need to accumulate body
   #m(status status
      headers (headers-list->map headers)
      body incomplete
      chunks []))
  
  ([`#(gun_data ,_ ,_ fin ,data)]
   ;; Final body chunk (use with accumulated state)
   data)
  
  ([`#(gun_error ,_ ,_ ,reason)]
   #m(error reason)))
```

**Implementation for `/2` (accumulating chunks):**
```lisp
(defun gun-response->http-response
  ;; Initial response message
  ([`#(gun_response ,_ ,_ ,fin-flag ,status ,headers) 'init]
   (if (== fin-flag 'fin)
       #m(status status
          headers (headers-list->map headers)
          body #"")
       #m(status status
          headers (headers-list->map headers)
          chunks []
          complete? false)))
  
  ;; Accumulate body chunk
  ([`#(gun_data ,_ ,_ ,fin-flag ,data) acc-map]
   (let* ((chunks (maps:get 'chunks acc-map))
          (new-chunks (++ chunks `(,data))))
     (if (== fin-flag 'fin)
         (mset acc-map 
               'body (iolist_to_binary new-chunks)
               'complete? true)
         (mset acc-map 
               'chunks new-chunks))))
  
  ;; Handle trailers (rare)
  ([`#(gun_trailers ,_ ,_ ,trailers) acc-map]
   (let* ((existing-headers (maps:get 'headers acc-map))
          (trailer-headers (headers-list->map trailers))
          (merged-headers (maps:merge existing-headers trailer-headers)))
     (mset acc-map 'headers merged-headers))))
```

**Usage pattern:**
```lisp
;; Receive initial response
(let ((resp1 (receive 
               (`#(gun_response ,_ ,_ ,_ ,_ ,_) = msg msg)
               (after 5000 'timeout))))
  (let ((state (gun-response->http-response resp1 'init)))
    (if (maps:get 'complete? state true)
        state  ;; Done, return response
        ;; Keep receiving body chunks
        (accumulate-body state))))

(defun accumulate-body (state)
  (receive
    (`#(gun_data ,_ ,_ ,_ ,_) = msg
     (let ((new-state (gun-response->http-response msg state)))
       (if (maps:get 'complete? new-state)
           (maps:remove 'chunks 
             (maps:remove 'complete? new-state))
           (accumulate-body new-state))))
    (after 5000 
      #m(error timeout))))
```

### Phase 4: Helper Functions

#### 4.1: `headers-map->list/1`

Convert headers map to list of tuples for httpc/gun.

```lisp
(defun headers-map->list (headers-map)
  "Convert headers map to list of tuples"
  (maps:fold
    (lambda (key val acc)
      (cons (tuple (ensure-binary key)
                   (ensure-binary val))
            acc))
    '()
    headers-map))
```

#### 4.2: `headers-list->map/1`

Convert headers list to map for lfe-http/http.

```lisp
(defun headers-list->map (headers-list)
  "Convert headers list to normalized map"
  (lists:foldl
    (lambda (header acc)
      (let ((`#(,key ,val) header))
        (maps:put 
          (http.util:binary-downcase (ensure-binary key))
          (ensure-binary val)
          acc)))
    #m()
    headers-list))
```

#### 4.3: `method-binary->atom/1`

```lisp
(defun method-binary->atom (method-bin)
  "Convert binary method to lowercase atom"
  (list_to_atom 
    (string:lowercase 
      (binary_to_list method-bin))))
```

#### 4.4: `method-atom->binary/1`

```lisp
(defun method-atom->binary (method-atom)
  "Convert atom method to uppercase binary"
  (list_to_binary 
    (string:uppercase 
      (atom_to_list method-atom))))
```

### Phase 5: Testing Strategy

Create `test/lhc.converter-tests.lfe`:

```lisp
(defmodule lhc.converter-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

;;; httpc conversion tests

(deftest http-request->httpc-request-get
  (let* ((req #m(method #"GET"
                 url #"https://example.com/api/users"
                 headers #m(#"accept" #"application/json")
                 body #""
                 path-segments [#"api" #"users"]
                 query-parsed #m()))
         (`#(,method #(,url ,headers) ,http-opts ,opts)
          (lhc.converter:http-request->httpc-request req)))
    (is-equal 'get method)
    (is-equal "https://example.com/api/users" url)
    (is-equal '(#("accept" "application/json")) headers)
    (is-equal '() http-opts)
    (is (lists:member '#(body_format binary) opts))))

(deftest http-request->httpc-request-post-with-body
  (let* ((req #m(method #"POST"
                 url #"https://example.com/api/users"
                 headers #m(#"content-type" #"application/json")
                 body #"{\"name\":\"test\"}"
                 path-segments [#"api" #"users"]
                 query-parsed #m()))
         (`#(,method #(,url ,headers ,content-type ,body) ,_ ,_)
          (lhc.converter:http-request->httpc-request req)))
    (is-equal 'post method)
    (is-equal "application/json" content-type)
    (is-equal #"{\"name\":\"test\"}" body)))

(deftest httpc-response->http-response-full
  (let* ((httpc-resp '#(ok #(#("HTTP/1.1" 200 "OK")
                             (#("content-type" "application/json"))
                             #"{\"status\":\"ok\"}"))
         (http-resp (lhc.converter:httpc-response->http-response httpc-resp)))
    (is-equal 200 (maps:get 'status http-resp))
    (is-equal #"application/json" 
              (maps:get #"content-type" (maps:get 'headers http-resp)))
    (is-equal #"{\"status\":\"ok\"}" (maps:get 'body http-resp))))

;;; gun conversion tests

(deftest http-request->gun-request-basic
  (let* ((req #m(method #"GET"
                 url #"https://api.example.com/users?active=true"
                 headers #m(#"accept" #"application/json")
                 body #""
                 path-segments [#"users"]
                 query-parsed #m(#"active" #"true")))
         (gun-req (lhc.converter:http-request->gun-request req)))
    (is-equal 'https (maps:get 'scheme gun-req))
    (is-equal "api.example.com" (maps:get 'host gun-req))
    (is-equal 443 (maps:get 'port gun-req))
    (is-equal #"GET" (maps:get 'method gun-req))
    (is-equal "/users?active=true" (maps:get 'path gun-req))))

(deftest gun-response->http-response-complete
  (let* ((gun-msg '#(gun_response pid ref fin 200
                     (#(#"content-type" #"text/plain"))))
         (http-resp (lhc.converter:gun-response->http-response gun-msg)))
    (is-equal 200 (maps:get 'status http-resp))
    (is-equal #"" (maps:get 'body http-resp))
    (is-equal #"text/plain"
              (maps:get #"content-type" (maps:get 'headers http-resp)))))

(deftest gun-response->http-response-chunked
  (let* ((msg1 '#(gun_response pid ref nofin 200
                  (#(#"content-type" #"text/plain"))))
         (state1 (lhc.converter:gun-response->http-response msg1 'init))
         (msg2 '#(gun_data pid ref nofin #"first "))
         (state2 (lhc.converter:gun-response->http-response msg2 state1))
         (msg3 '#(gun_data pid ref fin #"chunk"))
         (final (lhc.converter:gun-response->http-response msg3 state2)))
    (is-equal 200 (maps:get 'status final))
    (is-equal #"first chunk" (maps:get 'body final))))

;;; Helper function tests

(deftest headers-map->list-conversion
  (let* ((headers-map #m(#"content-type" #"application/json"
                         #"accept" #"text/html"))
         (headers-list (lhc.converter:headers-map->list headers-map)))
    (is-equal 2 (length headers-list))
    (is (lists:member '#(#"content-type" #"application/json") headers-list))
    (is (lists:member '#(#"accept" #"text/html") headers-list))))

(deftest headers-list->map-conversion
  (let* ((headers-list '(#(#"Content-Type" #"application/json")
                         #(#"Accept" #"text/html")))
         (headers-map (lhc.converter:headers-list->map headers-list)))
    (is-equal #"application/json" (maps:get #"content-type" headers-map))
    (is-equal #"text/html" (maps:get #"accept" headers-map))))

(deftest method-conversions
  (is-equal 'get (lhc.converter:method-binary->atom #"GET"))
  (is-equal 'post (lhc.converter:method-binary->atom #"POST"))
  (is-equal #"GET" (lhc.converter:method-atom->binary 'get))
  (is-equal #"POST" (lhc.converter:method-atom->binary 'post)))
```

### Phase 6: Integration with lhc

After converters are working, update the main `lhc` module to use them:

```lisp
(defmodule lhc
  (export
   (request 1) (request 2)
   (get 1) (get 2)
   (post 2) (post 3)
   ;; ... other methods
   ))

;;; Using httpc backend
(defun request
  ;; Basic request with defaults
  ([req] (when (is_map req))
   (request req #m(backend httpc)))
  
  ;; Request with options
  ([req opts] (when (is_map req) (is_map opts))
   (let* ((backend (maps:get 'backend opts 'httpc))
          (result (case backend
                    ('httpc (request-httpc req opts))
                    ('gun (request-gun req opts)))))
     result)))

(defun request-httpc (req opts)
  "Make request using httpc backend"
  (let* ((httpc-req (lhc.converter:http-request->httpc-request req opts))
         (`#(,method ,request ,http-opts ,req-opts) httpc-req)
         (result (httpc:request method request http-opts req-opts)))
    (case result
      (`#(ok ,response)
       (lhc.converter:httpc-response->http-response `#(ok ,response)))
      (`#(error ,reason)
       #m(error reason)))))

(defun request-gun (req opts)
  "Make request using gun backend"
  (let* ((gun-req (lhc.converter:http-request->gun-request req))
         (scheme (maps:get 'scheme gun-req))
         (host (maps:get 'host gun-req))
         (port (maps:get 'port gun-req))
         (gun-opts (build-gun-opts scheme opts))
         (`#(ok ,conn-pid) (gun:open host port gun-opts))
         (method (maps:get 'method gun-req))
         (path (maps:get 'path gun-req))
         (headers (maps:get 'headers gun-req))
         (body (maps:get 'body gun-req))
         (stream-ref (gun:request conn-pid method path headers body))
         (timeout (maps:get 'timeout opts 5000)))
    ;; Receive response
    (receive
      (`#(gun_response ,_ ,_ ,fin-flag ,status ,resp-headers)
       (let ((init-resp (lhc.converter:gun-response->http-response
                          `#(gun_response ,conn-pid ,stream-ref 
                            ,fin-flag ,status ,resp-headers)
                          'init)))
         (if (== fin-flag 'fin)
             (progn
               (gun:close conn-pid)
               init-resp)
             ;; Continue receiving body
             (receive-gun-body conn-pid stream-ref init-resp timeout))))
      (after timeout
        (gun:close conn-pid)
        #m(error timeout)))))

(defun receive-gun-body (conn-pid stream-ref acc timeout)
  "Accumulate gun body chunks"
  (receive
    (`#(gun_data ,_ ,_ ,fin-flag ,data)
     (let ((new-acc (lhc.converter:gun-response->http-response
                      `#(gun_data ,conn-pid ,stream-ref ,fin-flag ,data)
                      acc)))
       (if (== fin-flag 'fin)
           (progn
             (gun:close conn-pid)
             (maps:remove 'chunks (maps:remove 'complete? new-acc)))
           (receive-gun-body conn-pid stream-ref new-acc timeout))))
    (after timeout
      (gun:close conn-pid)
      #m(error timeout))))

(defun build-gun-opts (scheme user-opts)
  "Build gun connection options"
  (let ((base-opts (case scheme
                     ('https #m(transport tls))
                     ('http #m()))))
    (maps:merge base-opts user-opts)))
```

## Dependencies on lfe-http/http Updates

### PAUSE POINT 1: Check if these features exist in lfe-http/http

Before implementing converters, verify that `lfe-http/http` supports:

1. **URL parsing into components** - Does `http.request:new/1` properly parse:
   - Scheme (http/https)
   - Host
   - Port
   - Path
   - Query parameters

2. **Header normalization** - Are headers consistently:
   - Stored in maps with binary keys
   - Normalized to lowercase keys

3. **Query parameter handling** - Can we:
   - Parse query string to map
   - Reconstruct query string from map

### PAUSE POINT 2: If missing, add these to lfe-http/http

If the above features are missing or incomplete, pause and add them to `lfe-http/http`:

#### Add to `src/http.util.lfe`:

```lisp
(defun parse-url (url)
  "Parse URL into components map"
  (let* ((parsed (uri_string:parse (ensure-list url)))
         (scheme (maps:get 'scheme parsed "http"))
         (host (maps:get 'host parsed))
         (port (maps:get 'port parsed (default-port scheme)))
         (path (maps:get 'path parsed "/"))
         (query (maps:get 'query parsed "")))
    #m(scheme (list_to_atom scheme)
       host host
       port port
       path path
       query query)))

(defun default-port
  (["http"] 80)
  (["https"] 443)
  ([_] 80))

(defun build-url (components)
  "Build URL from components map"
  (let* ((scheme (atom_to_list (maps:get 'scheme components 'http)))
         (host (maps:get 'host components))
         (port (maps:get 'port components))
         (path (maps:get 'path components "/"))
         (query (maps:get 'query components ""))
         (port-str (if (== port (default-port scheme))
                       ""
                       (++ ":" (integer_to_list port))))
         (query-str (if (== query "")
                        ""
                        (++ "?" query))))
    (list_to_binary
      (++ scheme "://" host port-str path query-str))))
```

#### Update `src/http.request.lfe`:

Ensure `new/1` stores URL components:

```lisp
(defun new
  ((url) (when (is_binary url))
   (let* ((parsed (http.util:parse-url url))
          ;; ... existing code ...
          (scheme (maps:get 'scheme parsed))
          (host (maps:get 'host parsed))
          (port (maps:get 'port parsed)))
     ;; Add to request map:
     `#m(;; ... existing fields ...
         scheme ,scheme
         host ,host
         port ,port
         ;; ... existing fields ...
         ))))
```

This will make conversions much easier since we can directly access host/port/scheme without re-parsing.

### PAUSE POINT 3: Verify content-type extraction

Ensure `http.request.lfe` has a helper to extract content-type from headers:

```lisp
(defun get-content-type (headers)
  "Extract content-type from headers, with default"
  (maps:get #"content-type" headers #"application/octet-stream"))
```

## Implementation Checklist

- [ ] Phase 1: Create `src/lhc.converter.lfe` module structure
- [ ] Phase 2: Implement httpc conversions
  - [ ] `http-request->httpc-request/1`
  - [ ] `http-request->httpc-request/2`
  - [ ] `httpc-response->http-response/1`
- [ ] Phase 3: Implement gun conversions
  - [ ] `http-request->gun-request/1`
  - [ ] `http-request->gun-request/2`
  - [ ] `gun-response->http-response/1`
  - [ ] `gun-response->http-response/2`
- [ ] Phase 4: Implement helper functions
  - [ ] `headers-map->list/1`
  - [ ] `headers-list->map/1`
  - [ ] `method-binary->atom/1`
  - [ ] `method-atom->binary/1`
  - [ ] `extract-host-port/1`
  - [ ] `extract-path-query/1`
- [ ] Phase 5: Create comprehensive tests
- [ ] Phase 6: Integrate with main lhc module
- [ ] Verify all tests pass
- [ ] Test with real HTTP endpoints
- [ ] Document usage examples

## Expected Challenges

1. **gun's asynchronous nature**: The receive loops need careful timeout handling
2. **Header normalization**: Both backends have slightly different header formats
3. **Body encoding**: Ensure binary vs string handling is consistent
4. **Connection management**: gun requires explicit connection open/close
5. **Error handling**: Different error formats from each backend need normalization
6. **SSL/TLS options**: These differ significantly between httpc and gun

## Success Criteria

1. All unit tests pass
2. Can make successful requests with both httpc and gun backends
3. Request/response conversions are bidirectional and lossless
4. Headers are properly normalized in both directions
5. Error cases are handled gracefully
6. Code is well-documented with usage examples

## Additional Resources

- [inets httpc documentation](https://www.erlang.org/doc/man/httpc.html)
- [gun documentation](https://ninenines.eu/docs/en/gun/2.0/guide/)
- [LFE documentation](https://lfe.io/)
- URI parsing: `uri_string` module in Erlang/OTP
