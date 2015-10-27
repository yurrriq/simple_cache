(defmodule resource_discovery
  (behaviour gen_server)
  ;; API
  (export (start_link 0)
          (add-target-resource-type 1)
          (add-local-resource 2)
          (fetch-resources 1)
          (trade-resources 0))
  ;; gen_server callbacks
  (export (init 1)
          (handle_call 3)
          (handle_cast 2)
          (handle_info 2)
          (terminate 2)
          (code_change 3)))
