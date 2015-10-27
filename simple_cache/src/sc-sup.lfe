(defmodule sc-sup
  (behaviour supervisor)
  ;; API
  (export (start_link 0))
  ;; Supervisor callbacks
  (export (init 1)))

(defun server-name () (MODULE))


;;;===================================================================
;;; API
;;;===================================================================

(defun start_link ()
  (supervisor:start_link `#(local ,(server-name)) (MODULE) '()))


;;;===================================================================
;;; Supervisor callbakcs
;;;===================================================================

(defun init
  (['()]
   (let* ((element-sup      `#(sc-element-sup #(sc-element-sup start_link ())
                               permanent 2000 supervisor (sc-element)))
          (event-manager    `#(sc-event #(sc-event start_link ())
                               permanent 2000 worker (sc-event)))
          (children         `(,element-sup ,event-manager))
          (restart-strategy #(one_for_one 4 3600)))
     `#(ok #(,restart-strategy ,children)))))
