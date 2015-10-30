(defmodule sc-app
  (behaviour application)
  (export (start 2) (stop 1)))

(defun WAIT-FOR-RESOURCES () 2500)


;;;===================================================================
;;; API
;;;===================================================================

(defun start (_type _args)
  (let (('ok (ensure-contact))))
  (resource-discovery:add-local-resource 'simple_cache (node))
  (resource-discovery:add-target-resource-type 'simple_cache)
  (resource-discovery:trade-resources)
  (timer:sleep (WAIT-FOR-RESOURCES))
  (sc-store:init)
  (case (sc-sup:start_link)
    (`#(ok ,pid)
     (sc-event-logger:add-handler)
     `#(ok ,pid))
    (other
     `#(error ,other))))

(defun stop (_state) 'ok)


;;;===================================================================
;;; Internal functions
;;;===================================================================

(defun ensure-contact ()
  (let ((default-nodes '[contact1@localhost contact2@localhost]))
    (case (get-env 'simple_cache 'contact-nodes default-nodes)
      ('[]           '#(error no-contact-nodes))
      (contact-nodes (ensure-contact contact-nodes)))))

(defun ensure-contact (contact-nodes)
  (let ((answering (lc ((<- n contact-nodes) (=:= (net_adm:ping n) 'pong)))))
    (case answering
      ('[] '#(error no-contact-nodes-reachable))
      (_
       (let* ((default-time 6000)
              (wait-time    (get-env 'simple_cache 'wait-time default-time)))
         (wait-for-nodes (length answering) wait-time))))))

(defun wait-for-nodes (min-nodes wait-time)
  (let* ((slices     10)
         (slice-time (round (/ wait-time slices))))
    (wait-for-nodes min-nodes slice-time slices)))

(defun wait-for-nodes
  ([_min-nodes _slice-time 0]        'ok)
  ([min-nodes slice-time iterations]
   (case (> (length (nodes)) min-nodes)
     ('true  'ok)
     ('false
      (timer:sleep slice-time)
      (wait-for-nodes min-nodes slice-time (- iterations 1))))))

(defun get-env (app-name key default)
  (case (application:get_env app-name key)
    ('undefined    default)
    (`#(ok ,value) value)))
