(defmodule resource-discovery
  (export (start 0)
          (add-local-resource 2)
          (add-target-resource-type 1)

          (fetch-resources 1)
          (trade-resources 0)))

(defun start () (application:start 'resource_discovery))

(defun add-local-resource (type resource)
  (rd-server:add-local-resource type resource))

(defun add-target-resource-type (type)
  (rd-server:add-target-resource-type type))

(defun fetch-resources (type)
  (rd-server:fetch-resources type))

(defun trade-resources ()
  (rd-server:trade-resources))
