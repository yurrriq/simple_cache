(defmodule simple-cache
  (export (start 0) (insert 2) (lookup 1) (delete 1)))

(defun start () (application:start 'simple_cache))

(defun insert (key value)
  (case (sc-store:lookup key)
    (`#(ok ,pid)
     (sc-event:replace key value)
     (sc-element:replace pid value))
    (`#(error ,_)
     (let ((`#(ok ,pid) (sc-element:create value)))
       (sc-store:insert key pid)
       (sc-event:create key value)))))

(defun lookup (key)
  (sc-event:lookup key)
  (try
    (let* ((`#(ok ,pid)   (sc-store:lookup key))
           (`#(ok ,value) (sc-element:fetch pid)))
      `#(ok ,value))
    (catch
      (`#(,_ ,_ ,_) '#(error not-found)))))

(defun delete (key)
  (sc-event:delete key)
  (case (sc-store:lookup key)
    (`#(ok ,pid)        (sc-element:delete pid))
    (`#(error ,_reason) 'ok)))
