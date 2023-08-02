(defmodule lhc
  (export all))

(include-lib "lhc/include/lhc-options.lfe")

(defun start ()
  (start (get-backend-cfg)))

(defun start (backend)
  (change-backend backend)
  (code:ensure_all_started 'lhc))

;;; GET

(defun get (url)
  (get url '() (make-options)))

(defun get (url options)
  (get url '() (make-options options)))

(defun get (url headers options)
  (request url
           'GET
           (make-headers headers)
           ""
           (make-options options)))

;;; HEAD
(defun head (url)
  (head url '() (make-options)))

(defun head (url options)
  (head url '() (make-options options)))

(defun head (url headers options)
  (request url
           'HEAD
           (make-headers headers)
           ""
           (make-options options)))

;;; POST

(defun post (url)
  (post url "" '() (make-options)))

(defun post (url data)
  (post url data '() (make-options)))

(defun post (url data options)
  (post url data '() (make-options options)))

(defun post (url data headers options)
  (request url
           'POST
           (make-headers headers)
           data
           (make-options options)))

;;; PUT

(defun put (url)
  (put url "" '() (make-options)))

(defun put (url data)
  (put url data '() (make-options)))

(defun put (url data options)
  (put url data '() (make-options options)))

(defun put (url data headers options)
  (request url
           'PUT
           (make-headers headers)
           data
           (make-options options)))

;;; DELETE

(defun delete (url)
  (delete url '() (make-options)))

(defun delete (url options)
  (delete url '() (make-options options)))

(defun delete (url headers options)
  (request url
           'DELETE
           (make-headers headers)
           ""
           (make-options options)))

;;; TRACE

(defun trace (url)
  (trace url "" '() (make-options)))

(defun trace (url data)
  (trace url data '() (make-options)))

(defun trace (url data options)
  (trace url data '() (make-options options)))

(defun trace (url data headers options)
  (request url
           'TRACE
           (make-headers headers)
           data
          (make-options options)))

;;; OPTIONS

(defun options (url)
  (options url '() (make-options)))

(defun options (url options)
  (options url '() (make-options options)))

(defun options (url headers options)
  (request url
           'OPTIONS
           (make-headers headers)
           ""
           (make-options options)))

;;; CONNECT

;;; PATCH

(defun patch (url)
  (patch url "" '() (make-options)))

(defun patch (url data)
  (patch url data '() (make-options)))

(defun patch (url data options)
  (patch url data '() (make-options options)))

(defun patch (url data headers options)
  (request url
           'PATCH
           (make-headers headers)
           data
           (make-options options)))

;;; Request wrapper of lhttpc

(defun request (url method data)
  (request url method data (make-options)))

(defun request (url method data options)
  (request url method '() data options))

(defun request (url method headers data options)
  (request url method headers data (* 30 1000) options))

(defun request (url method headers data timeout options)
  (request url method headers data timeout '() options))

(defun request (url method headers data timeout backend-opts lhc-opts)
  (let ((opts (make-options lhc-opts)))
    (funcall (proplists:get_value 'callback opts)
             (list url method headers data timeout backend-opts opts)
             opts
             (call (get-backend-module)
                   (get-backend)
                   url method headers data timeout backend-opts))))

;;; Callback

(defun parse-results
  ((args opts result) (when (is_list opts))
   ;; we want to make sure that any library using lhc and setting their own
   ;; parse-results callback has *their* callback executed
   (funcall (proplists:get_value 'callback opts #'parse-results/3)
            args
            (opts->rec opts)
            result))
  ((_ (match-lhc-opts return 'status) `#(ok #(,sts ,_ ,_)))
   sts)
  ((_ (match-lhc-opts return 'headers) `#(ok #(,_ ,hdrs ,_)))
   hdrs)
  ((_ (match-lhc-opts return 'body) `#(ok #(,_ ,_ ,bdy)))
   (car (io_lib:format "~ts" `(,bdy))))
  ((_ (match-lhc-opts return 'binary) `#(ok #(,_ ,_ ,bdy)))
   bdy)
  ((_ (match-lhc-opts return 'all) `#(ok #(,sts ,hdrs ,bdy)))
   `#(ok (#(status ,sts)
          #(headers ,hdrs)
          #(body ,bdy))))
  ((_ _ (= `#(error ,_) err))
   err)
  ((_ _ all)
   all))

;;; Options

(defun get-default-options ()
  `(#(return body)
    #(callback ,#'lhc:parse-results/3)))

(defun make-options ()
  (get-backend-options))

(defun make-options (opts)
  (++ opts (get-default-options)))

(defun opts->rec (opts)
  (make-lhc-opts return (proplists:get_value 'return opts)
                 callback (proplists:get_value 'callback opts)))

;;; Headers

(defun get-default-headers ()
 `(#("User-Agent" ,(lhc-util:user-agent))))

(defun make-headers (headers)
  (++ headers (get-default-headers)))

;;; Backend

(defun get-backend-key ()
  'lhc-backend)

(defun get-backend-module ()
  'lhc-backend)

(defun get-backend ()
  (let ((default (erlang:get (get-backend-key))))
   (case default
     ('undefined (get-backend-cfg))
     (_ default))))

(defun get-backend
  ((`(#(return list)))
   (atom_to_list (get-backend))))

(defun get-backend-cfg ()
  (lcfg:get-in '(lhc backend)))

(defun change-backend (backend)
  `(#(backend
       #(previous ,(erlang:put (get-backend-key) backend))
       #(current ,backend))))

(defun get-backend-options ()
  (call (get-backend-module)
        (list_to_atom (++ (atom_to_list (get-backend))
                          "-default-options"))))

;;; Metadata

(defun version ()
  (lhc-vsn:get))

(defun versions ()
  (lhc-vns:all))
