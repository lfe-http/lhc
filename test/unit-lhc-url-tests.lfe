(defmodule unit-lhc-url-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "lhc/include/lhc-records.lfe")

;;; Test Data

(defun test-query-1 () '(#(c 3) #(d 4)))
(defun test-query-2 () #m(e 5 f 6 g 7))
(defun test-url-1 () (lhc-url:new))
(defun test-url-2 ()
  (lhc-url:new `(#(scheme "proto")
                 #(username "alice")
                 #(password "secret")
                 #(host "example.com")
                 #(port "5099")
                 #(path "/mnop/qrst/5678")
                 #(params (#(a 1) #(b 2)))
                 #(query ,(test-query-1))
                 #(fragment "#toc"))))
(defun test-url-3 ()
  (lhc-url:new `(#(scheme "proto")
                 #(netloc ,(make-netloc host "example.com"
                                        port "5099"))
                 #(path "/mnop/qrst/5678")
                 #(params (#(a 1) #(b 2)))
                 #(query ,(test-query-2))
                 #(fragment "#toc"))))

;;; Test constructors

(deftest new-0
  (let ((record-data (test-url-1)))
    (is (is_record record-data 'urldata))))

(deftest new-1
  (let ((url (test-url-2)))
    (is (is_record url 'urldata))
    (is-equal "proto" (urldata-scheme url))))

;;; Test accessors

(deftest get-scheme
  (let ((expected (lhc-url:get-scheme (test-url-2))))
    (is-equal "proto" expected)))

(deftest get-username
  (let ((expected (lhc-url:get-username (test-url-2))))
    (is-equal "alice" expected)))

(deftest get-password
  (let ((expected (lhc-url:get-password (test-url-2))))
    (is-equal "secret" expected)))

(deftest get-host
  (let ((expected (lhc-url:get-host (test-url-2))))
    (is-equal "example.com" expected)))

(deftest get-port
  (let ((expected (lhc-url:get-port (test-url-2))))
    (is-equal "5099" expected)))

(deftest get-path
  (let ((expected (lhc-url:get-path (test-url-2))))
    (is-equal "/mnop/qrst/5678" expected)))

(deftest get-params
  (let ((expected (lhc-url:get-params (test-url-2))))
    (is-equal '(#(a 1) #(b 2)) expected)))

(deftest get-query
  (let ((expected (lhc-url:get-query (test-url-2))))
    (is-equal '(#(c 3) #(d 4)) expected)))

(deftest get-fragment
  (let ((expected (lhc-url:get-fragment (test-url-2))))
    (is-equal "#toc" expected)))

;;; Test utility functions

(deftest ->string
  (is-equal "c=3&d=4" (lhc-url:->qstring (test-query-1)))
  (is-equal "e=5&f=6&g=7" (lhc-url:->qstring (test-query-2)))
  (is-equal #(error unsupported-type) (lhc-url:->qstring (test-url-1)))
  (is-equal "c=3&d=4" (lhc-url:->qstring (test-url-2)))
  (is-equal "e=5&f=6&g=7" (lhc-url:->qstring (test-url-3))))

;;; Test support functions

(deftest parse-netloc-host
  (let* ((netloc-str "example.com")
         (netloc (lhc-url:parse-netloc netloc-str)))
    (is-equal (netloc-host netloc) netloc-str)))

(deftest parse-netloc-host-port
  (let* ((netloc-str "example.com:5099")
         (netloc (lhc-url:parse-netloc netloc-str)))
    (is-equal "example.com"(netloc-host netloc))
    (is-equal "5099" (netloc-port netloc))))

(deftest parse-netloc-user-host
  (let* ((netloc-str "alice@example.com")
         (netloc (lhc-url:parse-netloc netloc-str)))
    (is-equal "example.com" (netloc-host netloc))
    (is-equal "alice" (netloc-username netloc))))

(deftest parse-netloc-user-pass-host-port
  (let* ((netloc-str "alice:secret@example.com:5099")
         (netloc (lhc-url:parse-netloc netloc-str)))
    (is-equal "example.com" (netloc-host netloc))
    (is-equal "5099" (netloc-port netloc))
    (is-equal "alice" (netloc-username netloc))
    (is-equal "secret" (netloc-password netloc))))

(deftest parse-netloc-error
  (let* ((netloc-str ":::")
         (netloc (lhc-url:parse-netloc netloc-str)))
    (is-equal #(error unparsable-string) netloc)))
