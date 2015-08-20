(defmodule sc-event-logger
  (behaviour gen_event)
  ;; API
  (export (add_handler 0)
          (delete_handler 0))
  ;; gen_event callbacks
  (export (init 1)
          (handle_event 2)
          (handle_call 2)
          (handle_info 2)
          (code_change 3)
          (terminate 2)))

(defrecord state '())


;;;===================================================================
;;; API
;;;===================================================================

(defun add_handler () (sc-event:add_handler (MODULE) '()))

(defun delete_handler () (sc-event:delete_handler (MODULE) '()))


;;;===================================================================
;;; gen_event callbacks
;;;===================================================================

(defun init
  (['()] `#(ok ,(make-state))))

(defun handle_event
  ([`#(create #(,key ,value)) state]
   (error_logger:info_msg "(create ~w ~w)~n" `(,key ,value))
   `#(ok ,state))
  ([`#(lookup ,key) state]
   (error_logger:info_msg "(lookup ~w)~n" `(,key))
   `#(ok ,state))
  ([`#(delete ,key) state]
   (error_logger:info_msg "(delete ~w)~n" `(,key))
   `#(ok ,state))
  ([`#(replace #(,key ,value)) state]
   (error_logger:info_msg "(replace ~w ~w)~n" `(,key ,value))
   `#(ok ,state)))

(defun handle_call (_request state)
  (let ((reply 'ok))
    `#(ok ,reply ,state)))

(defun handle_info (_info state) `#(ok ,state))

(defun code_change (_old-version state _extra) `#(ok ,state))

(defun terminate (_reason _state) 'ok)
