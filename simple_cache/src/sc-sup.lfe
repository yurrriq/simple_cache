(defmodule sc-sup
  (behaviour supervisor)
  ;; API
  (export (start_link 0))
  ;; Supervisor callbacks
  (export (init 1)))

(defun SERVER () (MODULE))


;;;===================================================================
;;; API
;;;===================================================================

(defun start_link ()
  (supervisor:start_link `#(local ,(SERVER)) (MODULE) '()))


;;;===================================================================
;;; Supervisor callbacks
;;;===================================================================

(defun init
  (['()]
   '#(ok #(#m(strategy  one_for_one
              intensity 4
              period    3600)
             [#m(id       sc-element-sup
                 start    #(sc-element-sup start_link [])
                 restart  permanent
                 shutdown 2000
                 type     supervisor
                 modules  [sc-element])
              #m(id       sc-event
                 start    #(sc-event start_link [])
                 restart  permanent
                 shutdown 2000
                 type     worker
                 modules  [sc-event])]))))
