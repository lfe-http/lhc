;;;; Unit tests for LHC utility functions
;;;; Tests UTF-8 encoding functionality

(include-file "test_server.lfe")
(include-lib "lhc/include/lhc-records.lfe")

(defmodule lhc_util_SUITE
  (export (all 0) (suite 0)
          (init_per_suite 1) (end_per_suite 1)
          (init_per_testcase 2) (end_per_testcase 2))
  (export (test_utf8_encode_latin1 1)
          (test_utf8_encode_unicode_binary 1)
          (test_utf8_encode_unicode_list 1)
          (test_utf8_encode_unicode_hex 1)))

;;; Suite Configuration

(defun all ()
  "Return list of all test cases"
  '(test_utf8_encode_latin1
    test_utf8_encode_unicode_binary
    test_utf8_encode_unicode_list
    test_utf8_encode_unicode_hex))

(defun suite ()
  "Return suite-level configuration"
  ())

(defun init_per_suite (config)
  "Initialize suite - minimal setup for unit tests"
  config)

(defun end_per_suite (config)
  "Clean up suite resources"
  'ok)

(defun init_per_testcase (func config)
  (let ((dog (: test_server timetrap (: test_server seconds 30))))
    (cons `#(watchdog ,dog) config)))

(defun end_per_testcase (func config)
  (let ((dog (config 'watchdog config)))
    (: test_server timetrap_cancel dog)))

;;; Test Cases

(defun test_utf8_encode_latin1
  (['suite] ())
  (['doc] "Test UTF-8 encoding of basic Latin characters in various formats")
  ([config] (when (is_list config))
   (line (test-pat (binary "latin") (lhc-util:utf8-encode "latin")))
   (line (test-pat #"latin" (lhc-util:utf8-encode "latin")))
   (line (test-pat #"latin" (lhc-util:utf8-encode #"latin")))
   (line (test-pat #"latin" (lhc-util:utf8-encode "latin" #(binary))))
   (line (test-pat "latin" (lhc-util:utf8-encode "latin" #(list))))
   (line (test-pat '("6C" "61" "74" "69" "6E") (lhc-util:utf8-encode "latin" #(hex))))
   'ok))

(defun test_utf8_encode_unicode_binary
  (['suite] ())
  (['doc] "Test UTF-8 encoding of Unicode characters to binary format")
  ([config] (when (is_list config))
   (let ((expected (binary ("Ůŋîçøðé" utf8))))
     ;; Sanity check - verify expected binary is correct
     (line (test-pat expected #"Ůŋîçøðé"))
     ;; Test encoding from binary input
     (line (test-pat expected (lhc-util:utf8-encode #"Ůŋîçøðé")))
     ;; Note: Tests with string input "Ůŋîçøðé" are commented out in original
     ;; as they work in REPL but not in unit tests. Keeping commented for now:
     ;; (line (test-pat expected (lhc-util:utf8-encode "Ůŋîçøðé")))
     ;; (line (test-pat expected (lhc-util:utf8-encode "Ůŋîçøðé" #(binary))))
     'ok)))

(defun test_utf8_encode_unicode_list
  (['suite] ())
  (['doc] "Test UTF-8 encoding of Unicode characters to list of byte values")
  ([config] (when (is_list config))
   (let ((expected '(197 174 197 139 195 174 195 167 195 184 195 176 195 169)))
     (line (test-pat expected (lhc-util:utf8-encode "Ůŋîçøðé" #(list))))
     'ok)))

(defun test_utf8_encode_unicode_hex
  (['suite] ())
  (['doc] "Test UTF-8 encoding of Unicode characters to hex string format")
  ([config] (when (is_list config))
   (let ((expected '("C5" "AE" "C5" "8B" "C3" "AE" "C3" "A7" "C3" "B8" "C3" "B0" "C3" "A9")))
     (line (test-pat expected (lhc-util:utf8-encode "Ůŋîçøðé" #(hex))))
     'ok)))
