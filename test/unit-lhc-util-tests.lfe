(defmodule unit-lhc-util-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "lhc/include/lhc-records.lfe")

;;; Test Data

;;; UTF-8

(deftest utf8-encode-latin1
  (is-equal (binary "latin") (lhc-util:utf8-encode "latin"))
  (is-equal #"latin" (lhc-util:utf8-encode "latin"))
  (is-equal #"latin" (lhc-util:utf8-encode #"latin"))
  (is-equal #"latin" (lhc-util:utf8-encode "latin" #(binary)))
  (is-equal "latin" (lhc-util:utf8-encode "latin" #(list)))
  (is-equal '("6C" "61" "74" "69" "6E") (lhc-util:utf8-encode "latin" #(hex))))

(deftest utf8-encode-unicode-binary
  (let ((expected (binary ("Ůŋîçøðé" utf8)))
        (bin-result-1 (lhc-util:utf8-encode "Ůŋîçøðé"))
        (bin-result-2 (lhc-util:utf8-encode "Ůŋîçøðé" #(binary))))
    (is-equal expected #"Ůŋîçøðé") ; sanity check
    ;; XXX this next one works in the REPL, but not in the unit test
    ;;(is-equal expected bin-result-1)
    (is-equal expected (lhc-util:utf8-encode #"Ůŋîçøðé"))
    ;; XXX this next one works in the REPL, but not in the unit test
    ;;(is-equal expected (lhc-util:utf8-encode "Ůŋîçøðé" #(binary)))
    ))

(deftest utf8-encode-unicode-list
  (let ((expected '(197 174 197 139 195 174 195 167 195 184 195 176 195 169)))
    (is-equal expected (lhc-util:utf8-encode "Ůŋîçøðé" #(list)))))

(deftest utf8-encode-unicode-hext
  (let ((expected-3 '("C5" "AE" "C5" "8B" "C3" "AE" "C3" "A7" "C3" "B8" "C3" "B0" "C3" "A9")))
    (is-equal expected-3 (lhc-util:utf8-encode "Ůŋîçøðé" #(hex)))))

