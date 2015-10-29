(defmodule simple-cache
  (export (start 0) (start 1)
          (insert 2)
          (lookup 1)
          (delete 1)))

(defun start ()
  "Start the simple_cache application."
  (application:start 'simple_cache))

(defun start
  "Call start/0 and parse any given `options'. Currently the only the option is
event-logger, which enables event logging."
  ([options] (when (is_list options))
   (start)
   (lists:foreach
     (lambda (option)
       (case option
         ('event-logger (sc-event-logger:add-handler))
         (_       'noop)))
     options)))

(defun insert (key value)
  "Given a `key' and a `value', create or replace the corresponding element
in the cache."
  (case (sc-store:lookup key)
    (`#(ok ,pid)
     (sc-event:replace key value)
     (sc-element:replace pid value))
    (`#(error ,_)
     (let ((`#(ok ,pid) (sc-element:create value)))
       (sc-store:insert key pid)
       (sc-event:create key value)))))

(defun lookup (key)
  "Given a `key', lookup its value. If found, return #(ok value),
otherwise #(error not-found)."
  (sc-event:lookup key)
  (try
    (let* ((`#(ok ,pid)   (sc-store:lookup key))
           (`#(ok ,value) (sc-element:fetch pid)))
      `#(ok ,value))
    (catch
      (`#(,_ ,_ ,_) '#(error not-found)))))

(defun delete (key)
  "Given a `key', delete the corresponding element from the cache."
  (sc-event:delete key)
  (case (sc-store:lookup key)
    (`#(ok ,pid)        (sc-element:delete pid))
    (`#(error ,_reason) 'ok)))
