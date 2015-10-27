(defmodule rd-app
  (behaviour application)
  (export (start 2) (stop 1)))

(defun start (_type _args)
  (case (rd-sup:start_link)
    (`#(ok ,pid) `#(ok    ,pid))
    (other       `#(error ,other))))

(defun stop (_state) 'ok)
