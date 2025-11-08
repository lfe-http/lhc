;;;; HTTP test server handler for LHC integration tests
;;;; Provides various endpoints to test HTTP client functionality
(defmodule lhc-test-handler
  (behavior cowboy_handler)
  (export
   (init 2)))

(defun init (req0 opts)
  (let* ((method (cowboy_req:method req0))
         (type (proplists:get_value 'type opts 'undefined))
         (method-opt (proplists:get_value 'method opts 'undefined))
         (req (handle-request method type method-opt req0 opts)))
    `#(ok ,req ,opts)))

;; Handle different request types
(defun handle-request
  ([_ 'status _ req _opts]
   (handle-status req))
  ([_ 'headers _ req _opts]
   (handle-headers req))
  ([_ 'set_cookies _ req _opts]
   (handle-set-cookies req))
  ([_ 'get_cookies _ req _opts]
   (handle-get-cookies req))
  ([_ 'delay _ req _opts]
   (handle-delay req))
  ([_ 'redirect _ req _opts]
   (handle-redirect req))
  ([#"GET" _ _ req _opts]
   (handle-get req))
  ([#"POST" _ _ req _opts]
   (handle-post req))
  ([#"PUT" _ _ req _opts]
   (handle-put req))
  ([#"DELETE" _ _ req _opts]
   (handle-delete req))
  ([method _ _ req _opts]
   ;; Default handler - echo request info
   (handle-echo method req)))

;; GET endpoint - returns request info as JSON
(defun handle-get (req0)
  (let* ((headers (cowboy_req:headers req0))
         (response (jsx:encode
                    (map 'method #"GET"
                         'headers headers
                         'url (cowboy_req:uri req0)))))
    (cowboy_req:reply 200
                      (map #"content-type" #"application/json")
                      response
                      req0)))

;; POST endpoint - echoes back the body
(defun handle-post (req0)
  (let ((`#(ok ,body ,req1) (cowboy_req:read_body req0)))
    (let* ((headers (cowboy_req:headers req1))
           (response (jsx:encode
                      (map 'method #"POST"
                           'headers headers
                           'data body
                           'url (cowboy_req:uri req1)))))
      (cowboy_req:reply 200
                        (map #"content-type" #"application/json")
                        response
                        req1))))

;; PUT endpoint - echoes back the body
(defun handle-put (req0)
  (let ((`#(ok ,body ,req1) (cowboy_req:read_body req0)))
    (let* ((headers (cowboy_req:headers req1))
           (response (jsx:encode
                      (map 'method #"PUT"
                           'headers headers
                           'data body
                           'url (cowboy_req:uri req1)))))
      (cowboy_req:reply 200
                        (map #"content-type" #"application/json")
                        response
                        req1))))

;; DELETE endpoint
(defun handle-delete (req0)
  (let* ((headers (cowboy_req:headers req0))
         (response (jsx:encode
                    (map 'method #"DELETE"
                         'headers headers
                         'url (cowboy_req:uri req0)))))
    (cowboy_req:reply 200
                      (map #"content-type" #"application/json")
                      response
                      req0)))

;; Status code endpoint - returns requested status code
(defun handle-status (req0)
  (let* ((code (cowboy_req:binding 'code req0))
         (status-code (binary_to_integer code)))
    (cowboy_req:reply status-code
                      (map)
                      #""
                      req0)))

;; Headers endpoint - returns all request headers as JSON
(defun handle-headers (req0)
  (let* ((headers (cowboy_req:headers req0))
         (response (jsx:encode (map 'headers headers))))
    (cowboy_req:reply 200
                      (map #"content-type" #"application/json")
                      response
                      req0)))

;; Set cookies endpoint - sets cookies from query parameters
(defun handle-set-cookies (req0)
  ;; Get query parameters
  (let* ((qs-vals (cowboy_req:parse_qs req0))
         ;; Set each query parameter as a cookie
         (req1 (lists:foldl
                (lambda (name-value req-acc)
                  (let ((`#(,name ,value) name-value))
                    (cowboy_req:set_resp_cookie name value req-acc
                                                 (map 'path #"/"))))
                req0
                qs-vals))
         (response (jsx:encode (map 'cookies #"set"))))
    (cowboy_req:reply 200
                      (map #"content-type" #"application/json")
                      response
                      req1)))

;; Get cookies endpoint - returns cookies from request
(defun handle-get-cookies (req0)
  (let* ((cookies (cowboy_req:parse_cookies req0))
         (response (jsx:encode (map 'cookies (maps:from_list cookies)))))
    (cowboy_req:reply 200
                      (map #"content-type" #"application/json")
                      response
                      req0)))

;; Delay endpoint - waits before responding
(defun handle-delay (req0)
  (let* ((seconds (cowboy_req:binding 'seconds req0))
         (delay (* (binary_to_integer seconds) 1000)))
    (timer:sleep delay)
    (let ((response (jsx:encode (map 'delay seconds))))
      (cowboy_req:reply 200
                        (map #"content-type" #"application/json")
                        response
                        req0))))

;; Redirect endpoint
(defun handle-redirect (req0)
  (let ((count (cowboy_req:binding 'count req0)))
    (case (binary_to_integer count)
      (0
       ;; Final destination
       (let ((response (jsx:encode (map 'redirected 'true))))
         (cowboy_req:reply 200
                           (map #"content-type" #"application/json")
                           response
                           req0)))
      (n (when (> n 0))
       ;; Redirect to next
       (let* ((next (integer_to_binary (- n 1)))
              (location (binary #"/redirect/" next)))
         (cowboy_req:reply 302
                           (map #"location" location)
                           #""
                           req0))))))

;; Echo handler - echoes request info
(defun handle-echo (method req0)
  (let ((`#(ok ,body ,req1) (cowboy_req:read_body req0)))
    (let* ((headers (cowboy_req:headers req1))
           (response (jsx:encode
                      (map 'method method
                           'headers headers
                           'data body
                           'url (cowboy_req:uri req1)))))
      (cowboy_req:reply 200
                        (map #"content-type" #"application/json")
                        response
                        req1))))
