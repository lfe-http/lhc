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

(deftest parse-host
  (let* ((url-str "http://example.com")
         (url (lhc-url:parse url-str)))
    (is-equal "http"(lhc-url:get-scheme url))
    (is-equal "example.com" (lhc-url:get-host url))))

(deftest parse-host-port
  (let* ((url-str "http://example.com:5099")
         (url (lhc-url:parse url-str)))
    (is-equal "http"(lhc-url:get-scheme url))
    (is-equal "example.com"(lhc-url:get-host url))
    (is-equal "5099" (lhc-url:get-port url))))

(deftest parse-user-host
  (let* ((url-str "http://alice@example.com")
         (url (lhc-url:parse url-str)))
    (is-equal "http"(lhc-url:get-scheme url))
    (is-equal "example.com" (lhc-url:get-host url))
    (is-equal "alice" (lhc-url:get-username url))))

(deftest parse-user-pass-host-port
  (let* ((url-str "http://alice:secret@example.com:5099")
         (url (lhc-url:parse url-str)))
    (is-equal "http"(lhc-url:get-scheme url))
    (is-equal "example.com" (lhc-url:get-host url))
    (is-equal "5099" (lhc-url:get-port url))
    (is-equal "alice" (lhc-url:get-username url))
    (is-equal "secret" (lhc-url:get-password url))))

(deftest parse-all
  (let* ((url-str "http://alice:sekrit@example.com:5099/a/b/c?q=lfe#toc")
         (url (lhc-url:parse url-str)))
    (is-equal "http"(lhc-url:get-scheme url))
    (is-equal "example.com" (lhc-url:get-host url))
    (is-equal "5099" (lhc-url:get-port url))
    (is-equal "alice" (lhc-url:get-username url))
    (is-equal "sekrit" (lhc-url:get-password url))
    (is-equal "/a/b/c" (lhc-url:get-path url))
    (is-equal "q=lfe" (lhc-url:get-query url))
    (is-equal "#toc" (lhc-url:get-fragment url))))

(deftest parse-no-user-password
  (let* ((url-str "http://example.com:5099/a/b/c?q=lfe#toc")
         (url (lhc-url:parse url-str)))
    (is-equal "http"(lhc-url:get-scheme url))
    (is-equal "example.com" (lhc-url:get-host url))
    (is-equal "5099" (lhc-url:get-port url))
    (is-equal "/a/b/c" (lhc-url:get-path url))
    (is-equal "q=lfe" (lhc-url:get-query url))
    (is-equal "#toc" (lhc-url:get-fragment url))))

(deftest parse-no-user-password-short-query
  (let* ((url-str "http://example.com:5099/a/b/c?#toc")
         (url (lhc-url:parse url-str)))
    (is-equal "http"(lhc-url:get-scheme url))
    (is-equal "example.com" (lhc-url:get-host url))
    (is-equal "5099" (lhc-url:get-port url))
    (is-equal "/a/b/c" (lhc-url:get-path url))
    (is-equal "" (lhc-url:get-query url))
    (is-equal "#toc" (lhc-url:get-fragment url))))

(deftest parse-no-user-password-short-query-frag
  (let* ((url-str "http://example.com:5099/a/b/c?#")
         (url (lhc-url:parse url-str)))
    (is-equal "http"(lhc-url:get-scheme url))
    (is-equal "example.com" (lhc-url:get-host url))
    (is-equal "5099" (lhc-url:get-port url))
    (is-equal "/a/b/c" (lhc-url:get-path url))
    (is-equal "" (lhc-url:get-query url))
    (is-equal "#" (lhc-url:get-fragment url))))

(deftest parse-all-short-query
  (let* ((url-str "http://alice:sekrit@example.com:5099/a/b/c?#toc")
         (url (lhc-url:parse url-str)))
    (is-equal "http"(lhc-url:get-scheme url))
    (is-equal "example.com" (lhc-url:get-host url))
    (is-equal "5099" (lhc-url:get-port url))
    (is-equal "alice" (lhc-url:get-username url))
    (is-equal "sekrit" (lhc-url:get-password url))
    (is-equal "/a/b/c" (lhc-url:get-path url))
    (is-equal "" (lhc-url:get-query url))
    (is-equal "#toc" (lhc-url:get-fragment url))))

(deftest parse-all-short-query-frag
  (let* ((url-str "http://alice:sekrit@example.com:5099/a/b/c?#")
         (url (lhc-url:parse url-str)))
    (is-equal "http"(lhc-url:get-scheme url))
    (is-equal "example.com" (lhc-url:get-host url))
    (is-equal "5099" (lhc-url:get-port url))
    (is-equal "alice" (lhc-url:get-username url))
    (is-equal "sekrit" (lhc-url:get-password url))
    (is-equal "/a/b/c" (lhc-url:get-path url))
    (is-equal "" (lhc-url:get-query url))
    (is-equal "#" (lhc-url:get-fragment url))))

(deftest parse-all-short-path-query-frag
  (let* ((url-str "http://alice:sekrit@example.com:5099/?#")
         (url (lhc-url:parse url-str)))
    (is-equal "http"(lhc-url:get-scheme url))
    (is-equal "example.com" (lhc-url:get-host url))
    (is-equal "5099" (lhc-url:get-port url))
    (is-equal "alice" (lhc-url:get-username url))
    (is-equal "sekrit" (lhc-url:get-password url))
    (is-equal "/" (lhc-url:get-path url))
    (is-equal "" (lhc-url:get-query url))
    (is-equal "#" (lhc-url:get-fragment url))))

(deftest parse-no-path-short-query-frag
  (let* ((url-str "http://alice:sekrit@example.com:5099?#")
         (url (lhc-url:parse url-str)))
    (is-equal "http"(lhc-url:get-scheme url))
    (is-equal "example.com" (lhc-url:get-host url))
    (is-equal "5099" (lhc-url:get-port url))
    (is-equal "alice" (lhc-url:get-username url))
    (is-equal "sekrit" (lhc-url:get-password url))
    (is-equal "" (lhc-url:get-path url))
    (is-equal "" (lhc-url:get-query url))
    (is-equal "#" (lhc-url:get-fragment url))))

(deftest parse-no-path-port-short-query-frag
  (let* ((url-str "http://alice:sekrit@example.com?#")
         (url (lhc-url:parse url-str)))
    (is-equal "http"(lhc-url:get-scheme url))
    (is-equal "example.com" (lhc-url:get-host url))
    (is-equal "" (lhc-url:get-port url))
    (is-equal "alice" (lhc-url:get-username url))
    (is-equal "sekrit" (lhc-url:get-password url))
    (is-equal "" (lhc-url:get-path url))
    (is-equal "" (lhc-url:get-query url))
    (is-equal "#" (lhc-url:get-fragment url))))

(deftest parse-no-query
  (let* ((url-str "http://alice:sekrit@example.com:5099/a/b/c#toc")
         (url (lhc-url:parse url-str)))
    (is-equal "http"(lhc-url:get-scheme url))
    (is-equal "example.com" (lhc-url:get-host url))
    (is-equal "5099" (lhc-url:get-port url))
    (is-equal "alice" (lhc-url:get-username url))
    (is-equal "sekrit" (lhc-url:get-password url))
    (is-equal "/a/b/c" (lhc-url:get-path url))
    (is-equal "" (lhc-url:get-query url))
    (is-equal "#toc" (lhc-url:get-fragment url))))

(deftest parse-no-frag
  (let* ((url-str "http://alice:sekrit@example.com:5099/a/b/c?")
         (url (lhc-url:parse url-str)))
    (is-equal "http"(lhc-url:get-scheme url))
    (is-equal "example.com" (lhc-url:get-host url))
    (is-equal "5099" (lhc-url:get-port url))
    (is-equal "alice" (lhc-url:get-username url))
    (is-equal "sekrit" (lhc-url:get-password url))
    (is-equal "/a/b/c" (lhc-url:get-path url))
    (is-equal "" (lhc-url:get-query url))
    (is-equal "" (lhc-url:get-fragment url))))

(deftest parse-no-frag-query
  (let* ((url-str "http://alice:sekrit@example.com:5099/a/b/c")
         (url (lhc-url:parse url-str)))
    (is-equal "http"(lhc-url:get-scheme url))
    (is-equal "example.com" (lhc-url:get-host url))
    (is-equal "5099" (lhc-url:get-port url))
    (is-equal "alice" (lhc-url:get-username url))
    (is-equal "sekrit" (lhc-url:get-password url))
    (is-equal "/a/b/c" (lhc-url:get-path url))
    (is-equal "" (lhc-url:get-query url))
    (is-equal "" (lhc-url:get-fragment url))))

(deftest parse-no-frag-query-port
  (let* ((url-str "http://alice:sekrit@example.com/a/b/c")
         (url (lhc-url:parse url-str)))
    (is-equal "http"(lhc-url:get-scheme url))
    (is-equal "example.com" (lhc-url:get-host url))
    (is-equal "" (lhc-url:get-port url))
    (is-equal "alice" (lhc-url:get-username url))
    (is-equal "sekrit" (lhc-url:get-password url))
    (is-equal "/a/b/c" (lhc-url:get-path url))
    (is-equal "" (lhc-url:get-query url))
    (is-equal "" (lhc-url:get-fragment url))))

(deftest parse-scheme-host-path
  (let* ((url-str "http://example.com/a/b/c")
         (url (lhc-url:parse url-str)))
    (is-equal "http"(lhc-url:get-scheme url))
    (is-equal "example.com" (lhc-url:get-host url))
    (is-equal "" (lhc-url:get-port url))
    (is-equal "" (lhc-url:get-username url))
    (is-equal "" (lhc-url:get-password url))
    (is-equal "/a/b/c" (lhc-url:get-path url))
    (is-equal "" (lhc-url:get-query url))
    (is-equal "" (lhc-url:get-fragment url))))

(deftest parse-error
  (let* ((url-str ":::")
         (url (lhc-url:parse url-str)))
    (is-equal #(error unparsable-string) url)))

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
