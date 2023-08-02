(defmodule lhc-util
  (export all))

(defun user-agent ()
  ;; The following code renders a user agent string along these lines:
  ;;    "LFE Little HTTP Client/0.1.0 (LFE 0.10.0-dev; Erlang 18; backend=lhttpc) (+http://github.com/lfex/lhc)"
  (++ "LFE Little HTTP Client/"
      (lhc:version)
      " (LFE " (lhc-vsn:get 'lfe)
      "; Erlang " (erlang:system_info 'otp_release)
      "(+http://github.com/lfe-http/lhc)"))

(defun utf8-encode
  ((characters) (when (is_binary characters))
    characters)
  ((characters) (when (is_list characters))
    (if (io_lib:printable_unicode_list characters)
      (unicode:characters_to_binary characters 'utf8)
      (list_to_binary characters))))

(defun utf8-encode
  ((characters #(hex))
    (lists:map
      (lambda (x)
        (integer_to_list x 16))
      (utf8-encode characters #(list))))
  ((characters #(list))
    (binary_to_list (utf8-encode characters)))
  ((characters #(binary))
    (utf8-encode characters)))

(defun utf8-decode (binary)
  (binary_to_list binary))

;; TODO: Move to lutil-types.lfe
(defun proplist?
  ((data) (when (is_list data))
    (if (lists:all #'proplist-kv?/1 data)
      'true
      'false))
  ((_)
    'false))

;; TODO: Move to lutil-types.lfe
(defun proplist-kv?
  ((`#(,key ,_)) (when (is_atom key))
    'true)
  ((bool-key) (when (is_atom bool-key))
    'true)
  ((_)
    'false))
