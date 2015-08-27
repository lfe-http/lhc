(defrecord lhc-opts
  (return 'body)                   ; can be status, headers, body, or all
  (callback #'lhc:parse-results/3) ; what to call after obtaining results
                                   ;   from lhttpc
  )

(defun loaded-lhc-options ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).

  This function needs to be the last one in this include."
  'ok)
