(defmodule rd-sup
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
;;; Supervisor callbakcs
;;;===================================================================

(defun init
  (['()]
   (let* ((server           '#(rd-server #(rd-server start_link [])
                               permanent 2000 worker [rd-server]))
          (children         `(,server))
          (restart-strategy '#(one_for_one 0 1)))
     `#(ok #(,restart-strategy ,children)))))
