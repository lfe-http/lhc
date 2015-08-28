(defmodule lhc-backend
  (export all))

(defun lhttpc (url method headers data timeout lhttpc-opts)
  (lhttpc:request url method headers data timeout lhttpc-opts))
