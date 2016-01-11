(defmodule unit-lhc-url-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "lhc/include/lhc-records.lfe")
(include-lib "ltest/include/ltest-macros.lfe")

;;; Test Data

(defun test-url-1 () (lhc-url:new))
(defun test-url-2 ()
  (lhc-url:new '(#(scheme "proto")
                 #(username "alice")
                 #(password "secret")
                 #(host "example.com")
                 #(path "/mnop/qrst/5678")
                 #(port "5099")
                 #(params (#(a 1) #(b 2)))
                 #(query (#(c 3) #(d 4)))
                 #(fragment "#toc"))))

;;; Constructors

(deftest new-0
  (let ((record-data (test-url-1)))
    (is (is_record record-data 'urldata))))

(deftest new-1
  (let ((url (test-url-2)))
    (is (is_record url 'urldata))
    (is-equal (urldata-scheme url) "proto")))

;;; Accessors

(deftest get-scheme
  (let ((expected (lhc-url:get-scheme (test-url-2))))
    (is-equal expected "proto")))

(deftest get-username
  (let ((expected (lhc-url:get-username (test-url-2))))
    (is-equal expected "alice")))

(deftest get-password
  (let ((expected (lhc-url:get-password (test-url-2))))
    (is-equal expected "secret")))

(deftest get-host
  (let ((expected (lhc-url:get-host (test-url-2))))
    (is-equal expected "example.com")))

(deftest get-port
  (let ((expected (lhc-url:get-port (test-url-2))))
    (is-equal expected "5099")))

(deftest get-path
  (let ((expected (lhc-url:get-path (test-url-2))))
    (is-equal expected "/mnop/qrst/5678")))

(deftest get-params
  (let ((expected (lhc-url:get-params (test-url-2))))
    (is-equal expected '(#(a 1) #(b 2)))))

(deftest get-query
  (let ((expected (lhc-url:get-query (test-url-2))))
    (is-equal expected '(#(c 3) #(d 4)))))

(deftest get-fragment
  (let ((expected (lhc-url:get-fragment (test-url-2))))
    (is-equal expected "#toc")))


;;; Utility functions

;;; Support functions

(deftest parse-netloc-host
  (let* ((netloc-str "example.com")
         (netloc (lhc-url:parse-netloc netloc-str)))
    (is-equal (netloc-host netloc) netloc-str)))

(deftest parse-netloc-host-port
  (let* ((netloc-str "example.com:5099")
         (netloc (lhc-url:parse-netloc netloc-str)))
    (is-equal (netloc-host netloc) "example.com")
    (is-equal (netloc-port netloc) "5099")))

(deftest parse-netloc-user-host
  (let* ((netloc-str "alice@example.com")
         (netloc (lhc-url:parse-netloc netloc-str)))
    (is-equal (netloc-host netloc) "example.com")
    (is-equal (netloc-username netloc) "alice")))

(deftest parse-netloc-user-pass-host-port
  (let* ((netloc-str "alice:secret@example.com:5099")
         (netloc (lhc-url:parse-netloc netloc-str)))
    (is-equal (netloc-host netloc) "example.com")
    (is-equal (netloc-port netloc) "5099")
    (is-equal (netloc-username netloc) "alice")
    (is-equal (netloc-password netloc) "secret")))
