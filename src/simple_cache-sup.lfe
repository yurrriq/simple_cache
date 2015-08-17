(defmodule simple_cache-sup
  (behaviour supervisor)
  ;; API
  (export (start_link 0))
  ;; Supervisor callbacks
  (export (init 1)))

(defun server-name ()
  'simple_cache-sup)

(defun start_link ()
  (: supervisor start_link
    (tuple 'local (server-name)) (MODULE) '()))

(defun init (args)
  (let* ((server (tuple
                   'simple_cache-server
                   (tuple 'simple_cache-server 'start_link '())
                   'permanent
                   2000
                   'worker
                   (list 'simple_cache-server)
                   ))
         (children (list server))
         (restart-strategy (tuple 'one_for_one 3 1)))
    (tuple 'ok (tuple restart-strategy children))))
