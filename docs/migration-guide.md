# Migration Guide: 0.4.x → 2.0.0

Guide for upgrading from LHC 0.4.x to 2.0.0.

## Table of Contents

* [Overview](#overview)
* [Breaking Changes](#breaking-changes)
* [New Features](#new-features)
* [API Changes](#api-changes)
* [Migration Steps](#migration-steps)
* [Code Examples](#code-examples)

## Overview

LHC 2.0 is a complete rewrite with significant improvements:

* **New backends**: Replaced lhttpc with httpc and gun
* **WebSocket support**: Full WebSocket client implementation
* **Improved async**: Better async request handling
* **Automatic cookies**: Gun backend handles cookies automatically
* **Modern API**: Maps instead of proplists, consistent error handling
* **Better testing**: Comprehensive integration test suite

## Breaking Changes

### 1. Backend Change

**Old (0.4.x):**
```lfe
;; Used lhttpc backend (required lhttpc dependency)
(lhc:get "http://example.com")
```

**New (2.0.0):**
```lfe
;; Uses httpc by default (built-in, no dependencies)
(lhc:get "http://example.com")

;; Or use gun backend
(lhc:get "http://example.com" (map 'backend 'gun))
```

**Action Required:**
* Remove lhttpc from your dependencies
* Choose between httpc (default) or gun backend
* Add gun to dependencies if using gun backend

### 2. Options Format

**Old (0.4.x):**
```lfe
;; Options were proplists
(lhc:get url '(#(timeout 5000) #(headers ,headers)))
```

**New (2.0.0):**
```lfe
;; Options are now maps
(lhc:get url (map 'timeout 5000 'headers headers))
```

**Action Required:**
* Convert all option proplists to maps
* Use `(map 'key1 value1 'key2 value2)` instead of `'(#(key1 value1) #(key2 value2))`

### 3. Response Format

**Old (0.4.x):**
```lfe
;; Response structure varied
{ok, {{StatusCode, ReasonPhrase}, Headers, Body}}
```

**New (2.0.0):**
```lfe
;; Consistent response map
{ok, #{status => StatusCode,
       headers => Headers,
       body => Body}}
```

**Action Required:**
* Update response pattern matching
* Access fields using `maps:get`
* Status code is now just an integer (no reason phrase)

### 4. Header Format

**Old (0.4.x):**
```lfe
;; Headers as strings
'(("Content-Type" "application/json")
  ("Authorization" "Bearer token"))
```

**New (2.0.0):**
```lfe
;; Headers as binaries
'(#(#"content-type" #"application/json")
  #(#"authorization" #"Bearer token"))
```

**Action Required:**
* Convert header names and values to binaries
* Use `#"string"` syntax instead of `"string"`

### 5. Error Handling

**Old (0.4.x):**
```lfe
;; Various error formats
{error, Reason}
```

**New (2.0.0):**
```lfe
;; Normalized error maps
{error, #{reason => Category,
          message => "Human readable message",
          backend => Backend,
          details => OriginalError}}
```

**Action Required:**
* Update error handling to expect error maps
* Use error categories for error type checking
* Access error message for display

### 6. Async API

**Old (0.4.x):**
```lfe
;; Async API was experimental
(lhc:request-async url method options)
;; No await function
```

**New (2.0.0):**
```lfe
;; Improved async API
(let ((`#(ok ,req-id) (lhc:get url (map 'mode 'async))))
  (lhc:await req-id timeout))
```

**Action Required:**
* Use `'mode 'async` option instead of separate async functions
* Use `lhc:await` to collect results
* Request ID is now opaque (don't rely on internal structure)

## New Features

### WebSocket Support

**Not available in 0.4.x**

**New in 2.0.0:**
```lfe
(let ((`#(ok ,conn) (lhc:open "wss://echo.websocket.org"
                             (map 'backend 'gun))))
  (let ((`#(ok ,ws) (lhc-websocket:upgrade conn "/" (map))))
    (lhc-websocket:send-text conn ws #"Hello!")
    (lhc-websocket:await-frame conn ws 5000)))
```

### Automatic Cookie Management

**Old (0.4.x):**
```lfe
;; Manual cookie handling required
```

**New (2.0.0):**
```lfe
;; Automatic with gun backend
(let ((store (lhc-cookies:init)))
  (lhc:open url (map 'backend 'gun 'cookie-store store)))
;; Cookies automatically saved and sent
```

### HTTP/2 Support

**Not available in 0.4.x**

**New in 2.0.0:**
```lfe
;; HTTP/2 automatic with gun backend
(lhc:get url (map 'backend 'gun))
```

## API Changes

### Function Signatures

| Function | 0.4.x | 2.0.0 |
|----------|-------|-------|
| get | `(lhc:get url)` | `(lhc:get url)` ✓ Same |
| get with opts | `(lhc:get url opts-proplist)` | `(lhc:get url opts-map)` |
| post | `(lhc:post url body opts)` | `(lhc:post url body opts)` ✓ Same |
| Response | `{ok, {Status, Headers, Body}}` | `{ok, #{...}}` |
| Error | `{error, Reason}` | `{error, #{...}}` |

### Removed Functions

These functions from 0.4.x are no longer available:

* `lhc:request-async/*` - Use `'mode 'async` option instead
* Backend-specific functions - Now automatic via options

### New Functions

New in 2.0.0:

* `lhc:await/2` - Await async request result
* `lhc:open/2` - Open persistent connection
* `lhc:close/1` - Close connection
* `lhc-websocket:*` - Full WebSocket API
* `lhc-cookies:*` - Cookie management functions
* `lhc-error:format/1` - Format error for display

## Migration Steps

### Step 1: Update Dependencies

**Old rebar.config:**
```erlang
{deps, [
  {lhc, "0.4.0"},
  {lhttpc, "1.6.2"}  % Remove this
]}.
```

**New rebar.config:**
```erlang
{deps, [
  {lhc, "2.0.0"}
  % lhttpc removed
  % gun added automatically if needed
]}.
```

### Step 2: Update Basic Requests

**Old:**
```lfe
(case (lhc:get url)
  (`#(ok #(#(,status ,_reason) ,headers ,body))
   (io:format "Status: ~p~n" (list status)))
  (`#(error ,reason)
   (io:format "Error: ~p~n" (list reason))))
```

**New:**
```lfe
(case (lhc:get url)
  (`#(ok ,response)
   (let ((status (maps:get 'status response)))
     (io:format "Status: ~p~n" (list status))))
  (`#(error ,error)
   (let ((message (maps:get 'message error)))
     (io:format "Error: ~s~n" (list message)))))
```

### Step 3: Update Options

**Old:**
```lfe
(let ((opts '(#(timeout 10000)
              #(headers (("Content-Type" "application/json"))))))
  (lhc:post url body opts))
```

**New:**
```lfe
(let ((headers '(#(#"content-type" #"application/json")))
      (opts (map 'timeout 10000 'headers headers)))
  (lhc:post url body opts))
```

### Step 4: Update Headers

**Old:**
```lfe
(let ((headers '(("Authorization" "Bearer token123")
                 ("Accept" "application/json"))))
  ...)
```

**New:**
```lfe
(let ((headers '(#(#"authorization" #"Bearer token123")
                 #(#"accept" #"application/json"))))
  ...)
```

### Step 5: Update Async Code

**Old:**
```lfe
(let ((async-ref (lhc:request-async url 'get '())))
  ;; Wait and check manually
  ...)
```

**New:**
```lfe
(let ((`#(ok ,req-id) (lhc:get url (map 'mode 'async))))
  (case (lhc:await req-id 5000)
    (`#(ok ,response) ...)
    (`#(error ,error) ...)))
```

## Code Examples

### Example 1: Simple GET

**Before (0.4.x):**
```lfe
(defun fetch-user (id)
  (let ((url (++ "https://api.example.com/users/" (integer_to_list id))))
    (case (lhc:get url)
      (`#(ok #(#(200 "OK") ,_headers ,body))
       `#(ok ,body))
      (`#(ok #(#(404 "Not Found") ,_ ,_))
       `#(error not-found))
      (`#(error ,reason)
       `#(error ,reason)))))
```

**After (2.0.0):**
```lfe
(defun fetch-user (id)
  (let ((url (++ "https://api.example.com/users/" (integer_to_list id))))
    (case (lhc:get url)
      (`#(ok ,response)
       (case (maps:get 'status response)
         (200 `#(ok ,(maps:get 'body response)))
         (404 `#(error not-found))
         (_ `#(error ,(maps:get 'status response)))))
      (`#(error ,error)
       `#(error ,(maps:get 'reason error))))))
```

### Example 2: POST with Headers

**Before (0.4.x):**
```lfe
(defun create-user (name email)
  (let ((url "https://api.example.com/users")
        (body (jsx:encode (map 'name name 'email email)))
        (opts '(#(headers (("Content-Type" "application/json")
                          ("Authorization" "Bearer token123"))))))
    (lhc:post url body opts)))
```

**After (2.0.0):**
```lfe
(defun create-user (name email)
  (let ((url "https://api.example.com/users")
        (body (jsx:encode (map 'name name 'email email)))
        (headers '(#(#"content-type" #"application/json")
                   #(#"authorization" #"Bearer token123")))
        (opts (map 'headers headers)))
    (lhc:post url body opts)))
```

### Example 3: Multiple Concurrent Requests

**Before (0.4.x):**
```lfe
;; Difficult with 0.4.x async API
(defun fetch-all (urls)
  ;; Sequential requests only
  (lists:map (lambda (url) (lhc:get url)) urls))
```

**After (2.0.0):**
```lfe
(defun fetch-all (urls)
  ;; Concurrent requests
  (let ((request-ids
         (lists:map
          (lambda (url)
            (let ((`#(ok ,req-id) (lhc:get url (map 'mode 'async))))
              `#(,url ,req-id)))
          urls)))
    (lists:map
     (lambda (url-and-id)
       (let ((`#(,url ,req-id) url-and-id))
         (case (lhc:await req-id 5000)
           (`#(ok ,response) `#(,url ok ,(maps:get 'status response)))
           (`#(error ,error) `#(,url error ,(maps:get 'reason error))))))
     request-ids)))
```

### Example 4: Error Handling

**Before (0.4.x):**
```lfe
(case (lhc:get url)
  (`#(ok ,response) (handle-success response))
  (`#(error 'timeout) (io:format "Request timed out~n"))
  (`#(error 'econnrefused) (io:format "Connection refused~n"))
  (`#(error ,other) (io:format "Unknown error: ~p~n" (list other))))
```

**After (2.0.0):**
```lfe
(case (lhc:get url)
  (`#(ok ,response) (handle-success response))
  (`#(error ,error)
   (case (maps:get 'reason error)
     ('timeout (io:format "~s~n" (list (maps:get 'message error))))
     ('connection (io:format "~s~n" (list (maps:get 'message error))))
     (_ (io:format "~s~n" (list (lhc-error:format error)))))))
```

## Testing Your Migration

After migrating, test your code thoroughly:

```bash
# Compile
rebar3 compile

# Run your tests
rebar3 eunit
rebar3 ct

# Test manually in REPL
rebar3 lfe repl
```

Example test in REPL:

```lfe
;; Test basic GET
(lhc:get "https://httpbin.org/get")

;; Test POST
(lhc:post "https://httpbin.org/post" #"{\"test\": \"data\"}"
          (map 'headers '(#(#"content-type" #"application/json"))))

;; Test async
(let ((`#(ok ,req-id) (lhc:get "https://httpbin.org/delay/2"
                               (map 'mode 'async))))
  (lhc:await req-id 5000))
```

## Common Migration Issues

### Issue 1: Headers as Strings

**Problem:**
```lfe
;; This won't work
(map 'headers '(("content-type" "application/json")))
```

**Solution:**
```lfe
;; Use binaries
(map 'headers '(#(#"content-type" #"application/json")))
```

### Issue 2: Response Pattern Matching

**Problem:**
```lfe
;; Old pattern won't match
(`#(ok #(#(,status ,reason) ,headers ,body)) ...)
```

**Solution:**
```lfe
;; Use map
(`#(ok ,response)
 (let ((status (maps:get 'status response))
       (body (maps:get 'body response)))
   ...))
```

### Issue 3: Proplist Options

**Problem:**
```lfe
;; Old style
(lhc:get url '(#(timeout 5000)))
```

**Solution:**
```lfe
;; New style
(lhc:get url (map 'timeout 5000))
```

## Need Help?

If you encounter issues during migration:

1. Check the [User Guide](user-guide.md) for usage examples
2. Review the [API Reference](api-reference.md) for function signatures
3. Look at [Examples](examples.md) for common patterns
4. [Open an issue](https://github.com/lfex/lhc/issues) on GitHub

## Benefits of Upgrading

* **Better performance** with gun backend
* **WebSocket support** for real-time applications
* **Automatic cookies** simplify session management
* **HTTP/2** support for modern APIs
* **Better error messages** for easier debugging
* **Comprehensive tests** ensure reliability
* **Active development** and bug fixes
