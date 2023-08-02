(defmodule lhc-backend
  (export all))

(include-lib "lhc/include/lhc-options.lfe")

;;; lhttpc Backend

;;(defun start-lhttpc ()
;;  `(#(lhttpc ,(lhttpc:start))))

;;(defun lhttpc (url method headers data timeout lhttpc-opts)
;;  (lhttpc:request url method headers data timeout lhttpc-opts))

;;(defun lhttpc-default-options ()
;;  (lhc:get-default-options))

;;; httpc Backend

(defun start-httpc ()
  '())

(defun httpc
  ((_ 'PATCH _ _ _ _)
   #(error #(httpc unsupported-client-method)))
  ((_ 'CONNECT _ _ _ _)
   #(error #(httpc unsupported-client-method)))
  ((url method headers data timeout httpc-opts)
   ;; XXX set up timeout option
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

(defun httpc-default-options ()
  (lhc:get-default-options))

;;; dlhttpc Backend

;;(defun start-dlhttpc ()
;;  `(#(dispcout ,(application:start 'dispcount))
;;    #(dlhttpc ,(dlhttpc:start))))

;;(defun dlhttpc (url method headers data timeout dlhttpc-opts)
;;  (dlhttpc:request url method headers data timeout dlhttpc-opts))

;;(defun dlhttpc-default-options ()
;;  (lhc:get-default-options))

;;; hackney

;;(defun start-hackney ()
;;  `(#(hackney ,(hackney:start))))

;;(defun hackney (url method headers data timeout hackney-opts)
;;  ;; XXX set up timeout option
;;  (hackney:request (get-httpc-method method)
;;                   (list_to_binary url)
;;                   headers
;;                   data
;;                   hackney-opts))
;;
;;(defun hackney-default-options ()
;;  `(#(return body)
;;    #(callback ,#'lhc-backend:hackney-parse-results/3)))
;;
;;(defun hackney-parse-results
;;  ((args opts result) (when (is_list opts))
;;   ;; we want to make sure that any library using lhc and setting their own
;;  ;; parse-results callback has *their* callback executed
;;   (funcall (proplists:get_value 'callback opts #'hackney-parse-results/3)
;;            args
;;            (lhc:opts->rec opts)
;;            result))
;;  ((_ (match-lhc-opts return 'status) `#(ok ,sts ,_ ,_))
;;   sts)
;;  ((_ (match-lhc-opts return 'headers) `#(ok ,_ ,hdrs ,_))
;;   hdrs)
;;  ((_ (match-lhc-opts return 'body) `#(ok ,_ ,_ ,ref))
;;   (case (hackney:body ref)
;;     (`#(ok ,result)
;;      (binary_to_list result))
;;    (x `#(error x))))
;;  ((_ (match-lhc-opts return 'body) `#(ok ,_ ,hdrs))
;;    hdrs)
;;  ((_ (match-lhc-opts return 'binary) `#(ok ,_ ,_ ,bdy))
;;   bdy)
;;  ((_ (match-lhc-opts return 'all) `#(ok ,sts ,hdrs ,ref))
;;   `#(ok (#(status ,sts)
;;          #(headers ,hdrs)
;;          #(body ,(element 2 (car (hackney:body ref)))))))
;;  ((_ _ (= `#(error ,_) err))
;;   err)
;;  ((args opts all)
;;   (io:format "No match.~n")
;;   `(,args ,opts ,all)))

;;; ibrowse

;;(defun start-ibrowse ()
;;  (let ((`#(,msg ,_pid) (ibrowse:start)))
;;  `(#(ibrowse ,msg))))

;;(defun ibrowse (url method headers data timeout ibrowse-opts)
;;  (ibrowse:send_req url
;;                    headers
;;                    (get-httpc-method method)
;;                    data
;;                    ibrowse-opts
;;                    timeout))

;;(defun ibrowse-default-options ()
;;  `(#(return body)
;;    #(callback ,#'lhc-backend:ibrowse-parse-results/3)))

;;(defun ibrowse-parse-results
;;  ((args opts result) (when (is_list opts))
;;   ;; we want to make sure that any library using lhc and setting their own
;;   ;; parse-results callback has *their* callback executed
;;   (funcall (proplists:get_value 'callback opts #'ibrowse-parse-results/3)
;;            args
;;            (lhc:opts->rec opts)
;;            result))
;;  ((_ (match-lhc-opts return 'status) `#(ok ,sts ,_ ,_))
;;   sts)
;;  ((_ (match-lhc-opts return 'headers) `#(ok ,_ ,hdrs ,_))
;;   hdrs)
;;  ((_ (match-lhc-opts return 'body) `#(ok ,_ ,hdrs ()))
;;    hdrs)
;;  ((_ (match-lhc-opts return 'body) `#(ok ,_ ,hdrs))
;;    hdrs)
;;  ((_ (match-lhc-opts return 'body) `#(ok ,_ ,_ ,bdy))
;;   bdy)
;;  ((_ (match-lhc-opts return 'binary) `#(ok ,_ ,_ ,bdy))
;;   bdy)
;;  ((_ (match-lhc-opts return 'all) `#(ok ,sts ,hdrs ,bdy))
;;   `#(ok (#(status ,sts)
;;          #(headers ,hdrs)
;;          #(body ,bdy))))
;;  ((_ _ (= `#(error ,_) err))
;;   err)
;;  ((args opts all)
;;   (io:format "No match.~n")
;;   `(,args ,opts ,all)))

