(defmodule lhc
  (export all))

(include-lib "lhc/include/lhc-options.lfe")

(defun start ()
  `(#(inets ,(inets:start))
    #(ssl ,(ssl:start))
    #(lhttpc ,(lhttpc:start))
    #(lhc ok)))

;;; GET

(defun get (url)
  (get url (get-default-options)))

(defun get (url options)
  (get url '() options))

(defun get (url headers options)
  (request url 'GET headers "" options))

;;; HEAD
(defun head (url)
  (head url (get-default-options)))

(defun head (url options)
  (head url '() options))

(defun head (url headers options)
  (request url 'HEAD headers "" (++ '(#(return headers)) options)))

;;; POST

(defun post (url)
  (post url "" '() (get-default-options)))

(defun post (url data)
  (post url data '() (get-default-options)))

(defun post (url data options)
  (post url data '() options))

(defun post (url data headers options)
  (request url 'POST headers data options))

;;; PUT

(defun put (url)
  (put url "" '() (get-default-options)))

(defun put (url data)
  (put url data '() (get-default-options)))

(defun put (url data options)
  (put url data '() options))

(defun put (url data headers options)
  (request url 'PUT headers data options))

;;; DELETE

;;; TRACE

;;; OPTIONS

;;; CONNECT

;;; PATCH

;;; Request wrapper of lhttpc

(defun request (url method data)
  (request url method data (get-default-options)))

(defun request (url method data options)
  (request url method '() data options))

(defun request (url method headers data options)
  (request url method headers data (* 30 1000) options))

(defun request (url method headers data timeout options)
  (request url method headers data timeout '() options))

(defun request (url method headers data timeout lhttpc-opts lhc-opts)
  (let ((options (++ lhc-opts (get-default-options))))
    (funcall (proplists:get_value 'callback options)
             (list url method headers data timeout lhttpc-opts options)
             options
             (lhttpc:request url method headers data timeout lhttpc-opts))))

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

(defun opts->rec (opts)
  (make-lhc-opts return (proplists:get_value 'return opts)
                 callback (proplists:get_value 'callback opts)))

