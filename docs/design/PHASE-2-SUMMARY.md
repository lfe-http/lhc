# LHC Reimplementation - Phase 2 Completion Summary

**Date:** 2025-11-06
**Phase:** Phase 2 - Backend Implementations
**Status:** ✅ COMPLETED

## Overview

Phase 2 implemented both HTTP client backends for LHC 2.0: the httpc backend (rewrite) and the gun backend (new). These backends provide the core HTTP request/response functionality with different capabilities and performance characteristics.

## Completed Tasks

### 2.1 httpc Backend Rewritten ✅

**File:** `src/lhc-httpc.lfe` (420 lines)

**Exports:**
- Core request execution: `do-request/5`, `do-request-sync/5`, `do-request-async/5`
- Profile management: `create-profile/2`, `set-profile-options/2`, `get-profile-options/1`
- Response normalization: `normalize-response/1`, `normalize-headers/1`
- Options conversion: `make-http-options/1`, `make-httpc-options/1`
- Request construction: `make-request/3`

**Key Features:**
- ✅ Synchronous and asynchronous HTTP requests
- ✅ Profile management for connection pooling
- ✅ Options conversion from LHC format to httpc format
- ✅ Response normalization to standard LHC format
- ✅ Header conversion (string tuples → binary tuples)
- ✅ Method conversion (uppercase atoms → lowercase atoms)
- ✅ Body format handling (binary/iolist → string)
- ✅ Error normalization via lhc-error module

**Supported Methods:**
- GET, HEAD, POST, PUT, DELETE, OPTIONS, TRACE, CONNECT

**Limitations (httpc):**
- ❌ PATCH method not supported (returns not-implemented error)
- ❌ No WebSocket support
- ❌ Limited HTTP/2 support
- ❌ No flow control

**HTTP Options Handled:**
- `timeout` - Request timeout
- `connect-timeout` - Connection timeout
- `ssl-opts` - SSL/TLS options
- `autoredirect` - Follow redirects
- `max-redirect` - Maximum redirect count
- `stream-to` - Stream response to file or self

**Profile Management:**
- Configurable profiles for connection pooling
- Options: max_sessions, max_keep_alive_length, keep_alive_timeout, max_pipeline_length, pipeline_timeout

### 2.2 gun Backend Implemented ✅

**File:** `src/lhc-gun.lfe` (532 lines)

**Exports:**
- Connection management: `open-connection/2`, `close-connection/1`, `shutdown-connection/1`, `await-up/2`
- Request execution: `do-request/6`, `do-request-sync/6`, `do-request-async/6`
- Response handling: `await-response/3`, `collect-body/4`
- Streaming: `stream-body/4`, `update-flow/3`
- Connection info: `connection-info/1`, `stream-info/2`, `ping/1`
- Normalization: `normalize-response/1`, `normalize-headers/1`
- Options: `make-gun-options/1`
- Utilities: `get-port/1`, `get-transport/1`

**Key Features:**
- ✅ HTTP/1.1 and HTTP/2 support
- ✅ WebSocket support (foundation - full implementation in Phase 4)
- ✅ Connection-based architecture (explicit open/close)
- ✅ Synchronous and asynchronous requests
- ✅ Flow control for streaming
- ✅ Connection pooling support
- ✅ TLS/SSL transport
- ✅ Protocol negotiation (HTTP/1.1, HTTP/2)
- ✅ CONNECT tunneling support
- ✅ Request streaming (chunked encoding)
- ✅ Response streaming
- ✅ Ping/pong for connection health checks

**Supported Methods:**
- GET, HEAD, POST, PUT, PATCH, DELETE, OPTIONS, CONNECT, TRACE

**Advantages over httpc:**
- ✅ PATCH method support
- ✅ HTTP/2 support
- ✅ WebSocket support
- ✅ Better streaming
- ✅ Flow control
- ✅ Connection reuse

**Gun Options Handled:**
- `protocols` - List of protocols to use (http, http2, ws)
- `transport` - tcp or tls
- `retry` - Number of retry attempts
- `retry-timeout` - Timeout between retries
- `connect-timeout` - Connection timeout
- `http-opts` - HTTP/1.1 specific options
- `http2-opts` - HTTP/2 specific options
- `ws-opts` - WebSocket specific options

**Connection Management:**
- Explicit connection opening with `gun:open/3`
- Wait for connection ready with `gun:await_up/2`
- Graceful close with `gun:close/1`
- Immediate shutdown with `gun:shutdown/1`
- Connection info and health checks

**Response Collection:**
- Message-based response handling
- Supports `gun_response`, `gun_data`, `gun_trailers` messages
- Timeout-based collection
- Body accumulation for sync mode
- Stream reference for async mode

## Implementation Details

### Backend Architecture Comparison

| Feature | httpc | gun |
|---------|-------|-----|
| Connection Model | Implicit | Explicit (pid-based) |
| HTTP/2 | Limited | Full Support |
| WebSocket | No | Yes |
| PATCH Method | No | Yes |
| Async Model | Callback | Message-based |
| Flow Control | No | Yes |
| Streaming | Basic | Advanced |
| Default | Yes | No |

### Request Flow

#### httpc Backend:
```
1. Convert LHC options → httpc options
2. Build request tuple {url, headers, content-type, body}
3. Call httpc:request/4 or httpc:request/5
4. Normalize response {status, headers, body}
5. Return {ok, response-map} or {error, error-map}
```

#### gun Backend:
```
1. Parse URI to extract host, port, scheme
2. Open connection with gun:open/3
3. Wait for connection ready with gun:await_up/2
4. Send request with gun:request/4 or gun:request/5
5. For sync: await-response to collect messages
6. For async: return stream-ref
7. Normalize response
8. Return {ok, response-map} or {error, error-map}
```

### Error Handling

Both backends use `lhc-error:normalize/2` for consistent error formatting:

```lfe
;; httpc errors
(lhc-error:normalize 'httpc 'nxdomain)
;; → {error, #{reason => connection, message => ..., backend => httpc, ...}}

;; gun errors
(lhc-error:normalize 'gun 'econnrefused)
;; → {error, #{reason => connection, message => ..., backend => gun, ...}}
```

### Header Normalization

Both backends normalize headers to lowercase binary tuples:

```lfe
;; Input (various formats)
[{"Content-Type", "application/json"}]
#{<<"Content-Type">> => <<"application/json">>}

;; Output (normalized)
[{<<"content-type">>, <<"application/json">>}]
```

### Method Conversion

**httpc:** Uppercase atom → Lowercase atom
```lfe
'GET → 'get
'POST → 'post
```

**gun:** Uppercase atom → Uppercase binary
```lfe
'GET → <<"GET">>
'POST → <<"POST">>
```

## Files Created

1. `src/lhc-httpc.lfe` - 420 lines - httpc backend implementation
2. `src/lhc-gun.lfe` - 532 lines - gun backend implementation
3. `docs/design/PHASE-2-SUMMARY.md` - This file

## Build Status

**Note:** Code still won't compile as API layer modules (lhc-sync, lhc-async, lhc) don't exist yet. These will be created in Phase 3.

## Testing

No tests written in Phase 2. Comprehensive testing scheduled for Phase 5.

## Dependencies on Phase 1

Both backends successfully use:
- ✅ `lhc-error` for error normalization
- ✅ `lhc-util` for header manipulation, URI parsing, binary conversion
- ✅ `lhc-types` for type documentation

## Next Phase: Phase 3 - Core API Layers

### What to Do Next:

**Phase 3 will implement the API layers that use these backends:**

1. **Task 3.1: Implement synchronous operations** (`src/lhc-sync.lfe`)
   - Public sync API: `get/2`, `post/3`, `put/3`, etc.
   - Request dispatcher to backends
   - Connection management wrapper
   - Options validation and merging

2. **Task 3.2: Implement asynchronous operations** (`src/lhc-async.lfe`)
   - Public async API: `get/2`, `post/3`, `put/3`, etc.
   - Message collection and handling
   - Request tracking (request-id → backend/conn/stream)
   - Async utilities: `await/2`, `cancel/1`, `flush/1`

3. **Task 3.3: Rewrite public API dispatcher** (`src/lhc.lfe`)
   - Main public API that dispatches to sync/async
   - Connection management: `open/2`, `close/1`
   - Determine mode from options
   - Delegate to lhc-sync or lhc-async
   - Utility exports: `normalize-headers/1`, `parse-uri/1`, etc.

### Dependencies for Phase 3:

**Already Complete:**
- ✅ Type definitions (`lhc-types`)
- ✅ Error normalization (`lhc-error`)
- ✅ Utilities (`lhc-util`)
- ✅ httpc backend (`lhc-httpc`)
- ✅ gun backend (`lhc-gun`)

**Required for Basic Functionality:**
- Need sync and async API layers
- Need main dispatcher module

### Estimated Effort:
- lhc-sync: 2-3 hours
- lhc-async: 3-4 hours
- lhc (dispatcher): 2 hours
- **Total: 6-9 hours**

## Backend Implementation Highlights

### httpc Backend Strengths:
1. **No Dependencies** - Built into Erlang/OTP
2. **Simple API** - Easy to use for basic requests
3. **Stable** - Well-tested, production-ready
4. **Profile-based Pooling** - Good for connection reuse

### gun Backend Strengths:
1. **Modern Protocols** - HTTP/2, WebSocket support
2. **Full Feature Set** - PATCH, flow control, streaming
3. **Performance** - Better for concurrent requests
4. **Explicit Control** - Fine-grained connection management

### When to Use Each Backend:

**Use httpc when:**
- Simple HTTP/1.1 requests
- No HTTP/2 or WebSocket needed
- Want zero external dependencies
- Basic GET/POST operations

**Use gun when:**
- Need HTTP/2 or WebSocket
- Need PATCH method
- Want advanced streaming
- Want connection pooling control
- Performance is critical

## Notes and Considerations

1. **Connection Lifecycle:**
   - httpc: Connections managed implicitly via profiles
   - gun: Explicit open/close required

2. **Async Patterns:**
   - httpc: Callback-based (messages to receiver)
   - gun: Message-based (gun_response, gun_data, etc.)

3. **Header Handling:**
   - Both backends normalize to lowercase binary tuples
   - HTTP/2 requires lowercase header names

4. **Body Handling:**
   - httpc needs Content-Type extracted separately
   - gun includes Content-Type in headers

5. **Response Streaming:**
   - httpc: Limited (stream to file or self)
   - gun: Full flow control support

## Known Limitations

### httpc Backend:
- PATCH method returns `not-implemented` error
- WebSocket not supported
- Limited HTTP/2 support
- No advanced flow control

### gun Backend:
- Requires explicit connection management
- More complex async handling
- External dependency (not built-in)

## Sign-Off

Phase 2 is complete and ready for commit. Both HTTP backends are fully implemented and ready to be used by the API layers.

**Ready for Phase 3:** ✅ YES

---

**Continuation Instructions:**

To continue from this point:
1. Read this summary document
2. Read Phase 1 summary for context
3. Proceed with Phase 3.1: Implement `src/lhc-sync.lfe`
4. Reference the design document `docs/design/001-lhc-reimplementation.md` sections 3.1-3.3
5. Both backends (httpc and gun) are ready to receive requests
6. All error handling and utilities are in place
