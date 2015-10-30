(defmodule sc-element-sup
  (behaviour supervisor)
  ;; API
  (export (start_link 0) (start_child 2))
  ;; Supervisor callbacks
  (export (init 1)))

(defun SERVER () 'sc-element-sup)


;;;===================================================================
;;; API
;;;===================================================================

(defun start_link ()
  (supervisor:start_link `#(local ,(SERVER)) (MODULE) '[]))

(defun start_child (value lease-time)
  (supervisor:start_child (SERVER) `(,value ,lease-time)))


;;;===================================================================
;;; Supervisor callbacks
;;;===================================================================

(defun init
  (['()]
   '#(ok #(#m(strategy  simple_one_for_one
              intensity 0
              period    1)
             [#m(id       sc-element
                 start    #(sc-element start_link [])
                 restart  temporary
                 shutdown brutal_kill
                 type     worker
                 modules  [sc-element])]))))
