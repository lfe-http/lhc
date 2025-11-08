(defmodule lhc-error
  "Error normalization and formatting for LHC.

  This module provides unified error handling across different HTTP backends
  (httpc and gun). It normalizes backend-specific errors into a common format
  and provides error categorization and formatting utilities."
  (export
   (normalize 2)
   (category 1)
   (format 1)
   (make-error 3)
   (make-error 4)))

;; Error categories as defined in lhc-types
;; connection, timeout, protocol, parse, ssl, auth, redirect, not-implemented, unknown

(defun normalize
  "Normalize an error from a backend into a standard error map.

  Args:
    backend: The backend that generated the error (httpc or gun)
    error: The raw error from the backend

  Returns:
    {error, error-map} where error-map contains:
      - reason: error category atom
      - message: human-readable binary string
      - backend: the backend that generated the error
      - details: the original error"
  ((backend error)
   (let ((error-map (normalize-error backend error)))
     `#(error ,error-map))))

(defun normalize-error
  "Internal function to normalize errors to error maps."
  ;; httpc errors
  (('httpc `#(error ,reason))
   (normalize-httpc-error reason))

  ;; httpc errors (direct reason, not wrapped in error tuple)
  (('httpc reason) (when (is_tuple reason))
   (normalize-httpc-error reason))

  ;; httpc errors (atom reasons)
  (('httpc reason) (when (is_atom reason))
   (normalize-httpc-error reason))

  ;; gun errors
  (('gun `#(error ,reason))
   (normalize-gun-error reason))

  ;; Already normalized gun errors from messages
  (('gun reason) (when (is_tuple reason))
   (normalize-gun-error reason))

  ;; lhc internal errors
  (('lhc reason)
   (normalize-lhc-error reason))

  ;; Unknown backend
  ((backend error)
   (make-error 'unknown
               (list_to_binary
                (++ "Unknown error from backend "
                    (atom_to_list backend)
                    ": "
                    (format-term error)))
               backend
               error)))

;; httpc Error Normalization
(defun normalize-httpc-error
  ;; Connection errors
  (('nxdomain)
   (make-error 'connection "Domain name resolution failed" 'httpc 'nxdomain))

  (('econnrefused)
   (make-error 'connection "Connection refused by server" 'httpc 'econnrefused))

  (('enetunreach)
   (make-error 'connection "Network unreachable" 'httpc 'enetunreach))

  (('ehostunreach)
   (make-error 'connection "Host unreachable" 'httpc 'ehostunreach))

  (('econnreset)
   (make-error 'connection "Connection reset by peer" 'httpc 'econnreset))

  (('etimedout)
   (make-error 'connection "Connection timed out" 'httpc 'etimedout))

  ;; Timeout errors
  (('timeout)
   (make-error 'timeout "Request timeout" 'httpc 'timeout))

  ;; Connection failures - extract the actual error from failed_connect
  ((`#(failed_connect ,details))
   (let ((actual-error (extract-inet-error details)))
     (if (=/= actual-error 'undefined)
       ;; Re-normalize the extracted error
       (normalize-httpc-error actual-error)
       ;; Fall back to SSL error if we can't extract inet error
       (make-error 'ssl
                   (extract-ssl-message details)
                   'httpc
                   `#(failed_connect ,details)))))

  ;; Protocol errors
  ((`#(bad_status_line ,line))
   (make-error 'protocol
               (list_to_binary (++ "Invalid HTTP status line: " line))
               'httpc
               `#(bad_status_line ,line)))

  ;; Redirect errors
  ((`#(max_redirects ,count))
   (make-error 'redirect
               (list_to_binary
                (++ "Maximum redirects exceeded: "
                    (integer_to_list count)))
               'httpc
               `#(max_redirects ,count)))

  ;; Not implemented
  (('patch_not_supported)
   (make-error 'not-implemented
               "PATCH method not supported by httpc backend"
               'httpc
               'patch_not_supported))

  (('websocket_not_supported)
   (make-error 'not-implemented
               "WebSocket not supported by httpc backend"
               'httpc
               'websocket_not_supported))

  ;; Unknown httpc errors
  ((reason)
   (make-error 'unknown
               (list_to_binary (++ "httpc error: " (format-term reason)))
               'httpc
               reason)))

;; gun Error Normalization
(defun normalize-gun-error
  ;; Connection errors
  (('nxdomain)
   (make-error 'connection "Domain name resolution failed" 'gun 'nxdomain))

  (('econnrefused)
   (make-error 'connection "Connection refused by server" 'gun 'econnrefused))

  (('enetunreach)
   (make-error 'connection "Network unreachable" 'gun 'enetunreach))

  (('ehostunreach)
   (make-error 'connection "Host unreachable" 'gun 'ehostunreach))

  (('econnreset)
   (make-error 'connection "Connection reset by peer" 'gun 'econnreset))

  (('closed)
   (make-error 'connection "Connection closed" 'gun 'closed))

  ;; Timeout errors
  (('timeout)
   (make-error 'timeout "Request timeout" 'gun 'timeout))

  (('connect_timeout)
   (make-error 'timeout "Connection timeout" 'gun 'connect_timeout))

  ;; SSL/TLS errors
  ((`#(tls_alert ,alert))
   (make-error 'ssl
               (list_to_binary
                (++ "TLS alert: "
                    (format-term alert)))
               'gun
               `#(tls_alert ,alert)))

  ;; Protocol errors
  ((`#(stream_error ,reason ,_message))
   (make-error 'protocol
               (list_to_binary
                (++ "HTTP stream error: "
                    (atom_to_list reason)))
               'gun
               `#(stream_error ,reason)))

  ((`#(connection_error ,reason ,_message))
   (make-error 'protocol
               (list_to_binary
                (++ "HTTP connection error: "
                    (atom_to_list reason)))
               'gun
               `#(connection_error ,reason)))

  ;; WebSocket errors
  ((`#(ws_upgrade_failed ,status))
   (make-error 'protocol
               (list_to_binary
                (++ "WebSocket upgrade failed with status: "
                    (integer_to_list status)))
               'gun
               `#(ws_upgrade_failed ,status)))

  ;; Nested error tuples (unwrap and re-normalize)
  ((`#(error ,nested-reason))
   (normalize-gun-error nested-reason))

  ;; inet error tuples
  ((`#(error ,error-atom ,_details))
   (normalize-gun-error error-atom))

  ;; Unknown gun errors
  ((reason)
   (make-error 'unknown
               (list_to_binary (++ "gun error: " (format-term reason)))
               'gun
               reason)))

;; lhc Internal Errors
(defun normalize-lhc-error
  ((`#(unknown-backend ,backend))
   (make-error 'unknown
               (list_to_binary
                (++ "Unknown backend: "
                    (atom_to_list backend)))
               'lhc
               `#(unknown-backend ,backend)))

  ((`#(timeout ,message)) (when (is_binary message))
   (make-error 'timeout message 'lhc 'timeout))

  ((`#(invalid-uri ,uri))
   (make-error 'parse
               (list_to_binary
                (++ "Invalid URI: "
                    (format-term uri)))
               'lhc
               `#(invalid-uri ,uri)))

  ((`#(not-implemented ,feature))
   (make-error 'not-implemented
               (list_to_binary
                (++ "Feature not implemented: "
                    (atom_to_list feature)))
               'lhc
               `#(not-implemented ,feature)))

  ((reason)
   (make-error 'unknown
               (list_to_binary (++ "lhc error: " (format-term reason)))
               'lhc
               reason)))

;; Error Map Construction
(defun make-error
  "Create an error map with category, message, backend, and details."
  ((reason-category message backend details)
   (maps:from_list
    `(#(reason ,reason-category)
      #(message ,(ensure-binary message))
      #(backend ,backend)
      #(details ,details)))))

(defun make-error
  "Create an error map with category, message, and backend (details = reason)."
  ((reason-category message backend)
   (make-error reason-category message backend reason-category)))

;; Error Category Extraction
(defun category
  "Extract the error category from an error map.

  Args:
    error-map: The error map

  Returns:
    The error category atom"
  ((error-map) (when (is_map error-map))
   (maps:get 'reason error-map 'unknown))

  ((error-tuple) (when (is_tuple error-tuple))
   (case error-tuple
     (`#(error ,error-map) (when (is_map error-map))
      (category error-map))
     (_
      'unknown)))

  ((_)
   'unknown))

;; Error Formatting
(defun format
  "Format an error map into a human-readable binary string.

  Args:
    error-map: The error map

  Returns:
    A formatted binary string"
  ((error-map) (when (is_map error-map))
   (let ((reason (maps:get 'reason error-map 'unknown))
         (message (maps:get 'message error-map #"Unknown error"))
         (backend (maps:get 'backend error-map 'unknown)))
     (list_to_binary
      (++ "[" (string:uppercase (atom_to_list backend)) "] "
          (string:uppercase (atom_to_list reason)) ": "
          (binary_to_list message)))))

  ((error-tuple) (when (is_tuple error-tuple))
   (case error-tuple
     (`#(error ,error-map) (when (is_map error-map))
      (format error-map))
     (_
      (list_to_binary (++ "Error: " (format-term error-tuple))))))

  ((error)
   (list_to_binary (++ "Error: " (format-term error)))))

;; Helper Functions
(defun ensure-binary
  ((data) (when (is_binary data))
   data)
  ((data) (when (is_list data))
   (list_to_binary data))
  ((data) (when (is_atom data))
   (atom_to_binary data 'utf8))
  ((data)
   (list_to_binary (format-term data))))

(defun format-term
  "Format any Erlang term as a string for display."
  ((term)
   (lists:flatten (io_lib:format "~p" `(,term)))))

(defun extract-ssl-message
  "Extract a readable message from SSL connection failure details."
  ((details) (when (is_list details))
   (case (lists:keyfind 'inet `(,1) details)
     (`#(inet ,_transport ,reason)
      (ensure-binary reason))
     (_
      (case (lists:keyfind 'ssl `(,1) details)
        (`#(ssl ,reason)
         (ensure-binary reason))
        (_
         #"SSL connection failed")))))

  ((details)
   (list_to_binary
    (++ "SSL connection failed: "
        (format-term details)))))

(defun extract-inet-error
  "Extract the actual inet error from failed_connect details.

  Details format: [{to_address, ...}, {inet, [inet], Error}]
  Returns: Error atom or 'undefined"
  ((details) (when (is_list details))
   (case (lists:keyfind 'inet 1 details)
     (`#(inet ,_transport ,error)
      error)
     (_
      'undefined)))

  ((details)
   'undefined))
