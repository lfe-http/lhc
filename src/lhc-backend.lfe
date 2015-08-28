(defmodule lhc-backend
  (export all))

;;; lhttpc Backend

(defun start-lhttpc ()
  `(#(lhttpc ,(lhttpc:start))))

(defun lhttpc (url method headers data timeout lhttpc-opts)
  (lhttpc:request url method headers data timeout lhttpc-opts))

;;; httpc Backend

(defun start-httpc ()
  '())

(defun httpc
  ((_ 'PATCH _ _ _ _)
   #(error #(httpc unsupported-client-method)))
  ((_ 'CONNECT _ _ _ _)
   #(error #(httpc unsupported-client-method)))
  ((url method headers data timeout httpc-opts)
   (let ((request `#(,url ,headers))
         (`#(,http-opts ,opts) (get-httpc-opts httpc-opts)))
     (httpc:request (get-httpc-method method)
                    request
                    http-opts
                    opts))))

(defun get-httpc-opts
  (('())
   `#(() ()))
  ((combined-opts)
   combined-opts))

(defun get-httpc-method (method)
  (list_to_atom
    (string:to_lower
      (atom_to_list method))))
