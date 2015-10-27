(defmodule sc-store
  (export (init 0) (insert 2) (lookup 1) (delete 1)))

(defun table-id () 'sc-store)

(defun init ()
  (ets:new (table-id) '(public named_table))
  'ok)

(defun insert (key pid) (ets:insert (table-id) `#(,key ,pid)))

(defun lookup (key)
  (case (ets:lookup (table-id) key)
    (`(#(,key ,pid)) `#(ok ,pid))
    ('()             '#(error not-found))))

(defun delete (pid) (ets:match_delete (table-id) `#(_ ,pid)))
