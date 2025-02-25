(defmodule lhc-vsn
  (export
   (get 0) (get 1)
   (all 0)))

(defun get ()
  (get 'lhc))

(defun get (app-name)
  (application:load app-name)
  (case (application:get_key app-name 'vsn)
    (`#(ok ,vsn) vsn)
    (default default)))

(defun version-arch ()
  `#(architecture ,(erlang:system_info 'system_architecture)))

(defun version+name (app-name)
  `#(,app-name ,(get app-name)))

(defun versions-rebar ()
  `(,(version+name 'rebar)
    ,(version+name 'rebar3_lfe)))

(defun versions-langs ()
  `(,(version+name 'lfe)
    #(erlang ,(erlang:system_info 'otp_release))
    #(emulator ,(erlang:system_info 'version))
    #(driver ,(erlang:system_info 'driver_version))))

(defun all ()
  (lists:append `((,(version+name 'lhc))
                  ,(versions-langs)
                  ,(versions-rebar)
                  (,(version-arch)))))
