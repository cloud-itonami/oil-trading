#!/usr/bin/env nbb
;; run_tests.cljs — the whole suite, with a floor under what counts as a pass.
;;
;; Run it from the repo root:
;;
;;   nbb --classpath src:test run_tests.cljs
;;
;; Three exit codes, because "everything I checked was fine" and "I checked
;; enough to say anything" are different claims (CLAUDE.md, ADR-2608136000):
;;
;;   0  every test ran and passed, and enough of them ran to mean something
;;   1  something failed
;;   2  REFUSED — too little ran to report a pass at all
;;
;; Without the floor, a require that silently resolves to nothing, or a
;; namespace dropped from the list below, prints the same "0 failures" as a
;; full green run. The marker on stdout is only printed on a real pass.
(ns run-tests
  (:require [clojure.test :as t]
            [oil_trading.murakumo-test]
            [oil_trading.manifest-test]))

(def min-tests
  "Below this many tests, the run did not measure the suite it claims to.
  Set under the current count with room to edit, not at it."
  30)

(def min-assertions 300)

(def green-marker "oil-trading: all green")

(defmethod t/report [:cljs.test/default :end-run-tests] [m]
  (let [{:keys [test pass fail error]} m
        assertions (+ pass fail error)]
    ;; cljs.test's :summary report has already printed the counts; this method
    ;; only decides what they are worth.
    (cond
      (< test min-tests)
      (do (println (str "REFUSED: only " test " tests ran, floor is " min-tests
                        " — this run did not measure enough to report a pass"))
          (set! (.-exitCode js/process) 2))

      (< assertions min-assertions)
      (do (println (str "REFUSED: only " assertions " assertions ran, floor is "
                        min-assertions
                        " — this run did not measure enough to report a pass"))
          (set! (.-exitCode js/process) 2))

      (and (zero? fail) (zero? error))
      (println green-marker)

      :else
      (set! (.-exitCode js/process) 1))))

(t/run-tests 'oil_trading.murakumo-test
             'oil_trading.manifest-test)
