(defrecord netloc
  username
  password
  host
  port)

(defrecord urldata
  scheme
  (netloc (make-netloc))
  path
  params
  query
  fragment)

(defun loaded-lhc-records ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).

  This function needs to be the last one in this include."
  'ok)
