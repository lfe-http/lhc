;; Test server utility macros for Common Test in LFE
;; Based on LFE compiler test utilities

;; As close as we can get to a vanilla erlang if, a case with no match.
(defmacro eif args
   (fletrec ((r ([(list* t v as)] `((_ (when ,t) ,v) . ,(r as)))
                ([()] ())))
     `(case 1 . ,(r args))))

(defmacro test-pat (pat expr)
  `(let* ((val ,expr)
          (,pat val))
     val))

;; Backwards compatibility for test_server test suites only.
;; DO NOT USE IN NEW TEST SUITES.
(defmacro line (expr) expr)

(defmacro config args
  `(: test_server lookup_config . ,args))
