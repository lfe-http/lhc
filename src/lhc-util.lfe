(defmodule lhc-util
  (export all))

(defun get-version ()
  (lutil:get-app-version 'lhc))

(defun get-versions ()
  (++ (lutil:get-versions)
      `(#(lhc ,(get-version)))))

(defun user-agent ()
  ;; The following code renders a user agent string along these lines:
  ;;    "LFE Little HTTP Client/0.1.0 (LFE 0.10.0-dev; Erlang 18; backend=lhttpc) (+http://github.com/lfex/lhc)"
  (++ "LFE Little HTTP Client/"
      (get-version)
      " (LFE " (lutil:get-lfe-version)
      "; Erlang " (erlang:system_info 'otp_release)
      "; backend=" (lhc:get-backend '(#(return list))) ") "
      "(+http://github.com/"
      (proplists:get_value 'github (lcfg-proj:get-repos))
      ")"))

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
