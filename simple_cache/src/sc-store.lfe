(defmodule sc-store
  (export (init 0) (insert 2) (lookup 1) (delete 1)))

(defun TABLE-ID        () 'key-to-pid)

(defun WAIT-FOR-TABLES () 5000)

(defrecord key-to-pid key pid)


;;;===================================================================
;;; API
;;;===================================================================

(defun init ()
  "TODO: write docstring"
  (mnesia:stop)
  (mnesia:delete_schema `[,(node)])
  (mnesia:start)
  (let ((`#(ok ,cache-nodes) (resource-discovery:fetch-resources 'simple_cache)))
    (dynamic-db-init (lists:delete (node) cache-nodes))))

(defun insert (key pid)
  "TODO: write docstring"
  (mnesia:dirty_write (make-key-to-pid key key pid pid)))

(defun lookup (key)
  "TODO: write docstring"
  (case (mnesia:dirty_read (TABLE-ID) key)
    (`[#(key-to-pid ,key* ,pid)]  (when (=:= key key*))
     (case (pid-alive? pid)
       ('true  `#(ok ,pid))
       ('false '#(error not-found))))
    ('[]
     '#(error not-found))))

(defun delete (pid)
  "TODO: write docstring"
  (case (mnesia:dirty_index_read (TABLE-ID) pid (key-to-pid-pid))
    ((= record (make-key-to-pid)) (mnesia:dirty_delete_object record))
    (_                            'ok)))


;;;===================================================================
;;; Internal functions
;;;===================================================================

(defun dynamic-db-init
  (['()]
   (mnesia:create_table (TABLE-ID)
                        `[#(index [pid])
                          #(attributes ,(fields-key-to-pid))]))
  ([cache-nodes]
   (add-extra-nodes cache-nodes)))

(defun add-extra-nodes
  ([`(,node . ,t)]
   (case (mnesia:change_config 'extra_db_nodes `[,node])
     (`#(ok [,node])
      (mnesia:add_table_copy (TABLE-ID) (node) 'ram_copies)
      (let ((tables (mnesia:system_info 'tables)))
        (mnesia:wait_for_tables tables (WAIT-FOR-TABLES))))
     (_ (add-extra-nodes t)))))

(defun pid-alive?
  ([pid] (when (=:= (node pid) (node)))
   (is_process_alive pid))
  ([pid]
   (case (lists:member (node pid) (nodes))
     ('false 'false)
     ('true  (case (rpc:call (node pid) 'erlang 'is_process_alive `[,pid])
               ('true               'true)
               ('false              'false)
               (`#(badrpc ,_reason) 'false))))))
