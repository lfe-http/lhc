(defmodule lhc-backend
  (export all))

;;; lhttpc Backend

(defun lhttpc (url method headers data timeout lhttpc-opts)
  (lhttpc:request url method headers data timeout lhttpc-opts))

;;; httpc Backend

(defun httpc (url method headers data timeout httpc-opts)
  (let ((request `#(,url ,headers))
        (`#(,http-opts ,opts ,profile) (get-httpc-opts httpc-opts)))
    (httpc:request (get-httpc-method method)
                   request
                   http-opts
                   opts
                   profile)))

(defun get-httpc-opts
  (('())
   #(() () lhc-profile))
  ((combined-opts)
   combined-opts))

(defun get-httpc-method (method)
  (list_to_atom
    (string:to_lower
      (atom_to_list method))))
