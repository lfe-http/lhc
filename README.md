# LHC - Little (LFE) HTTP Client

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe badge]][lfe]
[![Erlang Versions][erlang badge]][version]
[![Tags][github tags badge]][github tags]

*A modern, full-featured HTTP client for LFE with support for multiple backends*

[![LHC project logo][logo]][logo-large]

## Contents

* [Introduction](#introduction-)
* [Features](#features-)
* [Installation](#installation-)
* [Quick Start](#quick-start-)
* [Documentation](#documentation-)
* [Backends](#backends-)
* [Testing](#testing-)
* [Migration](#migration-)
* [License](#license-)

## Introduction [&#x219F;](#contents)

LHC (Little HTTP Client) is a comprehensive HTTP client library for LFE (Lisp Flavoured Erlang) that provides a simple, consistent API for making HTTP requests. Version 2.0 is a complete rewrite featuring:

- **Multiple Backends**: Choose between `httpc` (built-in) or `gun` (high-performance)
- **Async Operations**: Non-blocking HTTP requests with easy result collection
- **WebSocket Support**: Full WebSocket client implementation (gun backend only)
- **Cookie Management**: Automatic cookie handling with persistent stores
- **Modern API**: Clean, consistent interface across all features
- **Comprehensive Testing**: 27 integration tests ensuring reliability

## Features [&#x219F;](#contents)

### HTTP Methods
- GET, POST, PUT, DELETE, HEAD, OPTIONS, PATCH
- Custom headers and query parameters
- Request/response body handling
- Timeout configuration

### Advanced Features
- **Synchronous & Asynchronous** requests
- **Automatic cookie management** with persistent stores
- **WebSocket support** (text, binary, ping/pong, close frames)
- **Connection pooling** (gun backend)
- **HTTP/2 support** (gun backend)
- **SSL/TLS** with custom certificate validation
- **Error normalization** across backends

### Multiple Backends
- **httpc**: Erlang's built-in HTTP client (no dependencies)
- **gun**: High-performance HTTP/1.1, HTTP/2, and WebSocket client

## Installation [&#x219F;](#contents)

Add LHC to your `rebar.config`:

```erlang
{deps, [
  {lhc, "2.0.0"}
]}.
```

Then fetch dependencies:

```bash
rebar3 compile
```

### Dependencies

LHC requires:
- **LFE** 2.1.1 or later
- **Erlang/OTP** 24 or later

Optional backends:
- **gun** 2.0.1+ (included automatically if using gun backend)

## Quick Start [&#x219F;](#contents)

### Simple GET Request

```lfe
;; Using httpc backend (default)
(lhc:get "https://httpbin.org/get")
;; => {ok, #{status => 200, headers => [...], body => <<"...">>}}

;; Using gun backend
(lhc:get "https://httpbin.org/get" (map 'backend 'gun))
```

### POST with JSON

```lfe
(let ((url "https://httpbin.org/post")
      (body #"{\"name\": \"value\"}")
      (headers '(#(#"content-type" #"application/json")))
      (opts (map 'headers headers)))
  (lhc:post url body opts))
```

### Async Requests

```lfe
;; Start async request
(let ((`#(ok ,req-id) (lhc:get "https://httpbin.org/delay/2"
                               (map 'mode 'async))))
  ;; Do other work...

  ;; Collect result when ready
  (lhc:await req-id 5000))
;; => {ok, #{status => 200, ...}}
```

### Cookies

```lfe
;; Cookies are handled automatically with gun backend
(let ((store (lhc-cookies:init))
      (opts (map 'backend 'gun 'cookie-store store)))
  ;; Open connection with cookie store
  (let ((`#(ok ,conn) (lhc:open "https://example.com" opts)))
    ;; Cookies automatically saved and sent on subsequent requests
    (lhc-gun:do-request-sync conn 'GET "/set-cookie" #"" '() (map))
    (lhc-gun:do-request-sync conn 'GET "/need-cookie" #"" '() (map))))
```

### WebSocket

```lfe
;; Connect and upgrade to WebSocket
(let ((`#(ok ,conn) (lhc:open "wss://echo.websocket.org"
                              (map 'backend 'gun))))
  ;; Upgrade to WebSocket
  (let ((`#(ok ,ws) (lhc-websocket:upgrade conn "/" (map))))
    ;; Send text frame
    (lhc-websocket:send-text conn ws #"Hello WebSocket!")

    ;; Receive echo
    (let ((`#(ok ,frame) (lhc-websocket:await-frame conn ws 5000)))
      (io:format "Received: ~p~n" (list (maps:get 'data frame))))

    ;; Close
    (lhc:close conn)))
```

## Documentation [&#x219F;](#contents)

Comprehensive documentation is available in the `docs/` directory:

- **[User Guide](docs/user-guide.md)** - Complete usage guide with examples
- **[API Reference](docs/api-reference.md)** - Detailed API documentation
- **[Examples](docs/examples.md)** - Practical code examples and patterns
- **[Migration Guide](docs/migration-guide.md)** - Upgrading from 0.4.x to 2.0.0

### Core Modules

- **`lhc`** - Main API (get, post, put, delete, etc.)
- **`lhc-async`** - Async request handling
- **`lhc-cookies`** - Cookie store management
- **`lhc-websocket`** - WebSocket operations
- **`lhc-error`** - Error normalization and formatting

### Backend Modules

- **`lhc-httpc`** - httpc backend implementation
- **`lhc-gun`** - gun backend implementation
- **`lhc-backend`** - Backend dispatcher

## Backends [&#x219F;](#contents)

LHC supports two HTTP client backends:

### httpc (Default)

Erlang's built-in HTTP client. Best for:
- Simple HTTP requests
- No external dependencies
- Legacy compatibility

**Limitations:**
- No HTTP/2 support
- No WebSocket support
- Limited connection pooling

### gun (Recommended)

Modern, high-performance HTTP client. Best for:
- WebSocket applications
- HTTP/2 support
- High-performance scenarios
- Advanced connection management

**Features:**
- HTTP/1.1, HTTP/2, WebSocket
- Connection pooling
- Automatic cookie handling
- Superior performance

**Specify backend:**

```lfe
(lhc:get url (map 'backend 'gun))
```

## Testing [&#x219F;](#contents)

LHC includes comprehensive integration tests:

```bash
# Run all tests
rebar3 ct

# Run specific suite
rebar3 ct --suite=lhc_http_SUITE

# Run with coverage
rebar3 do ct, cover
```

### Test Coverage

- **27 integration tests** across 4 test suites
- **HTTP operations** (GET, POST, PUT, DELETE, headers, status codes)
- **Async operations** (concurrent requests, timeouts)
- **Cookie management** (persistence, deletion)
- **WebSocket** (upgrade, frames, ping/pong, close)

All tests run against a local Cowboy-based echo server (no external dependencies).

## Migration [&#x219F;](#contents)

Upgrading from 0.4.x? See the **[Migration Guide](docs/migration-guide.md)** for:

- Breaking changes
- New features
- API differences
- Code migration examples

### Quick Migration Summary

**Old (0.4.x):**
```lfe
(lhc:get "http://example.com")  ; Used lhttpc backend
```

**New (2.0.0):**
```lfe
(lhc:get "http://example.com")  ; Uses httpc by default
(lhc:get "http://example.com" (map 'backend 'gun))  ; Or gun
```

Major changes:
- Backend switched from `lhttpc` to `httpc`/`gun`
- Cookie API redesigned (automatic with gun)
- WebSocket support added
- Async API improved
- Options now use maps instead of proplists

## Development [&#x219F;](#contents)

### Project Structure

```
lhc/
├── src/              # Source code
│   ├── lhc.lfe       # Main API
│   ├── lhc-async.lfe
│   ├── lhc-cookies.lfe
│   ├── lhc-websocket.lfe
│   └── ...
├── test/             # Integration tests
│   ├── lhc_http_SUITE.lfe
│   ├── lhc_async_SUITE.lfe
│   ├── lhc_cookies_SUITE.lfe
│   └── lhc_websocket_SUITE.lfe
└── docs/             # Documentation
```

### Building

```bash
# Compile
rebar3 compile

# Run tests
rebar3 ct

# Clean
rebar3 clean
```

## Contributing [&#x219F;](#contents)

Contributions welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## License [&#x219F;](#contents)

Apache Version 2 License

Copyright (c) 2015 BilloSystems, Ltd. Co.

Copyright (c) 2015-2025 Duncan McGreggor <oubiwann@gmail.com>

## Links [&#x219F;](#contents)

- **[GitHub Repository](https://github.com/lfex/lhc)**
- **[Issue Tracker](https://github.com/lfex/lhc/issues)**
- **[LFE Language](https://github.com/rvirding/lfe)**
- **[Gun HTTP Client](https://github.com/ninenines/gun)**

---

**Made with ❤️ for the LFE community**

<!-- Named page links below: /-->

[logo]: priv/images/lhc-small.jpg
[logo-large]: priv/images/lhc.jpg
[org]: https://github.com/lfex
[github]: https://github.com/lfex/lhc
[gitlab]: https://gitlab.com/lfex/lhc
[gh-actions-badge]: https://github.com/lfex/lhc/actions/workflows/cicd.yml/badge.svg
[gh-actions]: https://github.com/lfex/lhc/actions
[lfe]: https://github.com/rvirding/lfe
[lfe badge]: https://img.shields.io/badge/lfe-2.1.1-blue.svg
[erlang badge]: https://img.shields.io/badge/erlang-24%20to%2027-blue.svg
[version]: https://github.com/lfex/lhc/blob/master/.github/workflows/cicd.yml
[github tags]: https://github.com/lfex/lhc/tags
[github tags badge]: https://img.shields.io/github/tag/lfex/lhc.svg
[github downloads]: https://img.shields.io/github/downloads/lfex/lhc/total.svg
[hex badge]: https://img.shields.io/hexpm/v/lhc.svg?maxAge=2592000
[hex package]: https://hex.pm/packages/lhc
[hex downloads]: https://img.shields.io/hexpm/dt/lhc.svg
