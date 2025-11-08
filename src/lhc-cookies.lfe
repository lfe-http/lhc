(defmodule lhc-cookies
  "Cookie management for LHC.

  This module provides cookie store initialization and management utilities.
  When using the gun backend, cookies are handled AUTOMATICALLY by Gun when
  you configure a cookie store on your connection.

  AUTOMATIC Cookie Handling (Recommended - Gun Backend):

    Gun's cookie system automatically extracts Set-Cookie headers from responses
    and adds Cookie headers to subsequent requests. Just pass a cookie store
    when opening a connection:

    ;; Initialize cookie store
    (let ((store (lhc-cookies:init)))
      ;; Open connection WITH cookie store
      (let ((`#(ok ,conn) (lhc:open \"https://example.com\"
                                    (map 'backend 'gun
                                         'cookie-store store))))
        ;; Make requests - cookies handled automatically!
        (lhc-gun:do-request-sync conn 'GET \"/login\" #\"\" '() (map))
        (lhc-gun:do-request-sync conn 'GET \"/profile\" #\"\" '() (map))
        ;; Cookies from /login are automatically sent to /profile
        (lhc:close conn)))

  MANUAL Cookie Management (Advanced Use Cases):

    Use the query/store/list-all functions for:
    - Debugging cookies
    - Inspecting cookie store contents
    - Manual cookie injection from external sources
    - Custom cookie workflows

  Cookie Store Types:
  - gun_cookies_list: In-memory cookie store (default)
  - Custom implementations possible (for persistence, sharing, etc.)"
  (export
   ;; Cookie store management
   (init 0)
   (init 1)

   ;; Cookie operations
   (query 2)
   (store 3)
   (list-all 1)
   (clear 1)
   (gc 1)
   (gc 2)

   ;; Integration with requests
   (add-cookie-header 3)
   (extract-set-cookies 1)
   (update-store-from-response 3)))

;;; ===================================================================
;;; Cookie Store Management
;;; ===================================================================

(defun init ()
  "Initialize a cookie store with default options.

  Returns:
    Cookie store (opaque term)"
  (init (map)))

(defun init
  "Initialize a cookie store with options.

  Args:
    opts: Options map
      - type: Store type (default: gun_cookies_list)
      - max-cookies: Maximum cookies to store
      - max-cookies-per-domain: Max cookies per domain

  Returns:
    Cookie store (opaque term)

  Example:
    (let ((store (lhc-cookies:init (map 'max-cookies 1000))))
      ...)"
  ((opts)
   (let* ((store-type (maps:get 'type opts 'gun_cookies_list))
          (init-opts (build-store-opts opts)))
     (case store-type
       ('gun_cookies_list
        (gun_cookies_list:init init-opts))
       (_
        ;; For custom store types, call their init
        (erlang:apply store-type 'init `(,init-opts)))))))

(defun build-store-opts
  "Build options for cookie store initialization."
  ((opts)
   (maps:from_list
    (lists:filtermap
     (lambda (opt-spec)
       (case opt-spec
         (`#(,key ,erlang-key)
          (case (maps:get key opts 'undefined)
            ('undefined 'false)
            (value `#(true #(,erlang-key ,value)))))))
     `(#(max-cookies max_cookies)
       #(max-cookies-per-domain max_cookies_per_domain))))))

;;; ===================================================================
;;; Cookie Operations
;;; ===================================================================

(defun query
  "Query cookies for a URI.

  Args:
    store: Cookie store
    uri: URI (binary or string)

  Returns:
    {ok, cookie-list, new-store} or {error, error-map}

  The new-store may differ if expired cookies were removed."
  ((store uri)
   (case (lhc-util:parse-uri uri)
     (`#(ok ,uri-map)
      (let ((uri-info (uri-map-to-gun-format uri-map)))
        (case (gun_cookies:query store uri-info)
          (`#(ok ,cookies ,new-store)
           `#(ok ,cookies ,new-store))
          (`#(error ,reason)
           (lhc-error:normalize 'lhc `#(cookie-error ,reason))))))
     (error error))))

(defun store
  "Store cookies from Set-Cookie headers.

  Args:
    store: Cookie store
    set-cookies: List of Set-Cookie header values (binaries)
    uri: URI where cookies were set

  Returns:
    {ok, new-store} or {error, error-map}

  Example:
    (lhc-cookies:store store
                       '(#\"session=abc; Path=/; HttpOnly\")
                       \"https://example.com\")"
  ((store set-cookies uri)
   (case (lhc-util:parse-uri uri)
     (`#(ok ,uri-map)
      (let ((uri-info (uri-map-to-gun-format uri-map)))
        (case (gun_cookies:store store set-cookies uri-info)
          (`#(ok ,new-store)
           `#(ok ,new-store))
          (`#(error ,reason)
           (lhc-error:normalize 'lhc `#(cookie-error ,reason))))))
     (error error))))

(defun list-all
  "List all cookies in the store.

  Args:
    store: Cookie store

  Returns:
    {ok, cookie-list}

  Each cookie is a map with keys:
    - name, value, domain, path, max-age, secure, http-only, same-site"
  ((store)
   (case (gun_cookies:list store)
     (cookies `#(ok ,cookies)))))

(defun clear
  "Clear all cookies from the store.

  Args:
    store: Cookie store

  Returns:
    {ok, new-store}"
  ((store)
   (let ((new-store (gun_cookies:clear store)))
     `#(ok ,new-store))))

(defun gc
  "Garbage collect expired cookies.

  Args:
    store: Cookie store

  Returns:
    {ok, new-store}"
  ((store)
   (gc store (erlang:universaltime))))

(defun gc
  "Garbage collect cookies expired before the given time.

  Args:
    store: Cookie store
    time: Erlang datetime tuple {{Y,M,D},{H,M,S}}

  Returns:
    {ok, new-store}"
  ((store time)
   (let ((new-store (gun_cookies:gc store time)))
     `#(ok ,new-store))))

;;; ===================================================================
;;; Request/Response Integration
;;; ===================================================================

(defun add-cookie-header
  "Add Cookie header to request headers based on cookie store.

  Args:
    headers: Request headers (list of tuples)
    uri: Request URI
    store: Cookie store

  Returns:
    {ok, new-headers, new-store} or {error, error-map}

  This function queries the cookie store for cookies matching the URI
  and adds a Cookie header with all applicable cookies."
  ((headers uri store)
   (case (query store uri)
     (`#(ok ,cookies ,new-store)
      (if (> (length cookies) 0)
        ;; Build Cookie header value
        (let* ((cookie-header (build-cookie-header cookies))
               (new-headers (cons `#(#"cookie" ,cookie-header) headers)))
          `#(ok ,new-headers ,new-store))
        ;; No cookies
        `#(ok ,headers ,new-store)))
     (error error))))

(defun build-cookie-header
  "Build Cookie header value from cookie list.

  Args:
    cookies: List of cookie tuples from gun_cookies

  Returns:
    Binary cookie header value (e.g., #\"name1=value1; name2=value2\")"
  ((cookies)
   (let ((cookie-strings
          (lists:map
           (lambda (cookie)
             (case cookie
               (`#(,name ,value)
                (binary name #"=" value))
               ;; gun_cookies may return maps or different formats
               (cookie-map (when (is_map cookie-map))
                (let ((name (maps:get 'name cookie-map))
                      (value (maps:get 'value cookie-map)))
                  (binary name #"=" value)))))
           cookies)))
     (list_to_binary (string:join (lists:map #'binary_to_list/1 cookie-strings) "; ")))))

(defun extract-set-cookies
  "Extract Set-Cookie headers from response.

  Args:
    headers: Response headers (list of tuples)

  Returns:
    List of Set-Cookie header values (binaries)

  Multiple Set-Cookie headers are returned as separate list items."
  ((headers)
   (lists:filtermap
    (lambda (header)
      (case header
        (`#(,name ,value)
         (if (=:= (lhc-util:lowercase (lhc-util:ensure-binary name))
                  #"set-cookie")
           `#(true ,value)
           'false))
        (_ 'false)))
    headers)))

(defun update-store-from-response
  "Update cookie store with Set-Cookie headers from response.

  Args:
    store: Cookie store
    response: Response map (with 'headers key)
    uri: Request URI

  Returns:
    {ok, new-store} or {error, error-map}

  This automatically extracts Set-Cookie headers and stores them."
  ((store response uri)
   (let* ((headers (maps:get 'headers response '()))
          (set-cookies (extract-set-cookies headers)))
     (if (> (length set-cookies) 0)
       (store store set-cookies uri)
       `#(ok ,store)))))

;;; ===================================================================
;;; Helper Functions
;;; ===================================================================

(defun uri-map-to-gun-format
  "Convert LHC URI map to gun_cookies URI format.

  gun_cookies expects a map with scheme, host, port, path keys.

  Args:
    uri-map: LHC URI map

  Returns:
    Map in gun_cookies format"
  ((uri-map)
   (maps:from_list
    `(#(scheme ,(scheme-to-atom (maps:get 'scheme uri-map)))
      #(host ,(maps:get 'host uri-map))
      #(port ,(get-port-or-default uri-map))
      #(path ,(maps:get 'path uri-map))))))

(defun scheme-to-atom
  "Convert scheme binary to atom for gun_cookies."
  ((scheme) (when (is_atom scheme)) scheme)
  ((scheme) (when (=:= scheme #"http")) 'http)
  ((scheme) (when (=:= scheme #"https")) 'https)
  ((scheme) (when (is_binary scheme))
   (binary_to_atom scheme 'utf8)))

(defun get-port-or-default
  "Get port from URI map or use scheme default."
  ((uri-map)
   (case (maps:get 'port uri-map 'undefined)
     ('undefined
      (case (maps:get 'scheme uri-map)
        (#"https" 443)
        (#"http" 80)
        (_ 80)))
     (port port))))
