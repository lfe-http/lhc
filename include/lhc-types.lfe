(defmodule lhc-types
  "Type definitions for LHC library.

  This module contains all type definitions used throughout the lhc library.
  These types can be exported and used by library consumers for type checking
  and documentation purposes."
  (export-type all))

;; Core Types
(deftype backend () '(httpc gun))
(deftype mode () '(sync async))
(deftype http-method () '(GET HEAD POST PUT PATCH DELETE OPTIONS CONNECT TRACE))

;; HTTP Version
(deftype http-version ()
  '(http/1.0 http/1.1 http/2))

;; Status Codes
(deftype status-code () '(integer))

;; Headers
(deftype header-name () '(binary))
(deftype header-value () '(binary))
(deftype header () `#(,header-name ,header-value))
(deftype headers () `(list ,header))

;; Body
(deftype body () '(union binary iolist))

;; URI Components
(deftype uri () '(binary))
(deftype scheme () '(union binary atom))
(deftype host () '(binary))
(deftype port-number () '(integer))
(deftype path () '(binary))
(deftype query () '(binary))
(deftype fragment () '(binary))

;; URI Map
(deftype uri-map ()
  `(map #(scheme ,scheme)
        #(host ,host)
        #(port ,port-number)
        #(path ,path)
        #(query ,query)
        #(fragment ,fragment)))

;; Request Options
(deftype timeout () '(union integer infinity))
(deftype connect-timeout () '(union integer infinity))

;; Cookie Types
(deftype cookie-store () '(term))  ; gun_cookies_list opaque type
(deftype cookie-name () '(binary))
(deftype cookie-value () '(binary))
(deftype cookie ()
  `(map #(name ,cookie-name)
        #(value ,cookie-value)
        #(domain ,binary)
        #(path ,binary)
        #(expires ,integer)
        #(secure ,boolean)
        #(http-only ,boolean)))

;; WebSocket Types
(deftype ws-frame-type () '(text binary ping pong close))
(deftype ws-close-code () '(integer))
(deftype ws-frame ()
  `(map #(type ,ws-frame-type)
        #(data ,binary)
        #(code ,ws-close-code)
        #(reason ,binary)))

;; Connection Types
(deftype connection-ref () '(union pid atom))  ; gun: pid, httpc: profile atom
(deftype stream-ref () '(term))  ; gun stream reference
(deftype request-id () '(union term reference))

;; Response Types
(deftype response ()
  `(map #(status ,status-code)
        #(headers ,headers)
        #(body ,body)
        #(version ,http-version)))

;; Error Categories
(deftype error-category ()
  '(connection timeout protocol parse ssl auth redirect not-implemented unknown))

;; Error Map
(deftype error-map ()
  `(map #(reason ,error-category)
        #(message ,binary)
        #(backend ,backend)
        #(details ,term)))

;; Result Types
(deftype ok-response () `#(ok ,response))
(deftype error-response () `#(error ,error-map))
(deftype result () `(union ,ok-response ,error-response))

(deftype async-result () `#(ok ,request-id))

;; Flow Control
(deftype flow () '(integer))  ; Number of data messages to receive

;; Stream Options
(deftype stream-destination ()
  '(union #(file binary) self none))

;; Protocol Options
(deftype protocol () '(http http2 ws))
(deftype protocols () `(list ,protocol))

;; TLS/SSL Options
(deftype ssl-opts () '(list))  ; Erlang ssl options proplist

;; Backend-Specific Options
(deftype httpc-opts ()
  `(map #(profile ,atom)
        #(ssl-opts ,ssl-opts)
        #(autoredirect ,boolean)
        #(stream-to ,stream-destination)))

(deftype gun-opts ()
  `(map #(protocols ,protocols)
        #(retry ,integer)
        #(retry-timeout ,timeout)
        #(transport ,atom)
        #(http-opts ,map)
        #(http2-opts ,map)
        #(ws-opts ,map)))

;; Main Options Map
(deftype options ()
  `(map #(backend ,backend)
        #(mode ,mode)
        #(headers ,headers)
        #(timeout ,timeout)
        #(connect-timeout ,connect-timeout)
        #(follow-redirects ,boolean)
        #(max-redirects ,integer)
        #(cookie-store ,cookie-store)
        #(stream-to ,stream-destination)
        #(flow ,flow)
        #(receiver ,pid)
        #(httpc-opts ,httpc-opts)
        #(gun-opts ,gun-opts)
        #(ssl-opts ,ssl-opts)))

;; CONNECT Tunnel Types
(deftype tunnel-protocol () '(union tcp tls))
(deftype tunnel-info ()
  `(map #(origin-host ,host)
        #(origin-port ,port-number)
        #(protocol ,tunnel-protocol)))
