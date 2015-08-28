(defmodule lhc-util
  (export all))

(defun get-version ()
  (lutil:get-app-version 'lhc))

(defun get-versions ()
  (++ (lutil:get-versions)
      `(#(lhc ,(get-version)))))

(defun user-agent ()
  (++ "LFE Little HTTP Client (lhc/LFE)/"
      (get-version)
      " (+http://github.com/"
      (proplists:get_value 'github (lcfg-proj:get-repos))
      ")"))
