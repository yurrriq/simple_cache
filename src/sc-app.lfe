(defmodule sc-app
  (behaviour application)
  (export (start 2) (stop 1)))

(defun start (_type _args)
  (sc-store:init)
  (case (sc-sup:start_link)
    (`#(ok ,pid) `#(ok    ,pid))
    (other       `#(error ,other))))

(defun stop (_state) 'ok)
