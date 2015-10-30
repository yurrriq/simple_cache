(defmodule sc-event
  ;; API
  (export (start_link 0)
          (create 2)
          (lookup 1)
          (replace 2)
          (delete 1))
  ;; gen_event wrappers
  (export (add-handler 2)
          (delete-handler 2)))

(defun SERVER () (MODULE))


;;;===================================================================
;;; API
;;;===================================================================

(defun start_link () (gen_event:start_link `#(local ,(SERVER))))

(defun create (key value)
  "TODO: write docstring"
  (notify 'create `(,key ,value)))

(defun lookup (key)
  "TODO: write docstring"
  (notify 'lookup `(,key)))

(defun replace (key value)
  "TODO: write docstring"
  (notify 'replace `(,key ,value)))

(defun delete (key)
  "TODO: write docstring"
  (notify 'delete `(,key)))


;;;===================================================================
;;; gen_event wrappers
;;;===================================================================

(defun add-handler (handler args)
  (gen_event:add_handler (SERVER) handler args))

(defun delete-handler (handler args)
  (gen_event:delete_handler (SERVER) handler args))


;;;===================================================================
;;; Internal functions
;;;===================================================================

(defun notify
  ([tag (data . '())]                   ; (when (=:= (length data) 1))
   (gen_event:notify (SERVER) `#(,tag ,data)))
  ([tag data]                           ; (when (is_list data))
   (gen_event:notify (SERVER) `#(,tag ,(list_to_tuple data)))))
