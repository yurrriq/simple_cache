(defmodule sc-event
  ;; API
  (export (start_link 0)
          (lookup 1)
          (create 2)
          (replace 2)
          (delete 1))
  ;; gen_event wrappers
  (export (add-handler 2)
          (delete-handler 2)))

(include-lib "include/sc-macros.lfe")

(defun server-name () (MODULE))

(defun notify
  ([tag (data . '())]                   ; (when (=:= (length data) 1))
   (gen_event:notify (server-name) `#(,tag ,data)))
  ([tag data]                           ; (when (is_list data))
   (gen_event:notify (server-name) `#(,tag ,(list_to_tuple data)))))


;;;===================================================================
;;; API
;;;===================================================================

(defun start_link () (gen_event:start_link `#(local ,(server-name))))

(defnote lookup (key))

(defnote create (key value))

(defnote replace (key value))

(defnote delete (key))


;;;===================================================================
;;; gen_event wrappers
;;;===================================================================

(defun add-handler (handler args)
  (gen_event:add_handler (server-name) handler args))

(defun delete-handler (handler args)
  (gen_event:delete_handler (server-name) handler args))
