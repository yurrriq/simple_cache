(defmodule unit-simple_cache-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "eunit/include/eunit.hrl")
(include-lib "ltest/include/ltest-macros.lfe")

(deftest code-change
  (is-equal #(ok "state")
            (sc-element:code_change "old version" "state""extra")))
