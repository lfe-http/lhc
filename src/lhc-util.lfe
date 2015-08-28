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
