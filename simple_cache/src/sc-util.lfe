(defmodule sc-util
  (export all))

(defun get-version ()
  (lutil:get-app-version 'simple_cache))

(defun get-versions ()
  (++ (lutil:get-versions)
      `(#(simple_cache ,(get-version)))))
