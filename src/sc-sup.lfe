(defmodule sc-sup
  (behaviour supervisor)
  ;; API
  (export (start_link 0) (start_child 2))
  ;; Supervisor callbacks
  (export (init 1)))

(defun server-name () 'sc-sup)

(defun start_link ()
  (supervisor:start_link
    `#(local ,(server-name)) (MODULE) '()))

(defun start_child (value lease-time)
  (supervisor:start_child (server-name) `(,value ,lease-time)))

(defun init
  (['()]
   (let* ((cache-element    `#(sc-element #(sc-element start_link ())
                               temporary brutal_kill worker (sc-element)))
          (children         `(,cache-element))
          (restart-strategy `#(simple_one_for_one 0 1)))
     `#(ok #(,restart-strategy ,children)))))
