(ns osgi.graph
  "The graph namespace is for functions and data structures that have to do
  with analysis of graphs themselves. Examples may include tests for circular
  dependencies, IO edge counts, etc. See https://cljdoc.org/d/aysylu/loom/1.0.2/api/loom
  for a potential third party solution to these kinds of problems."
  (:require [osgi.deps :as deps]))

(defn follow-chain [chain next deps]
  (if (nil? next) chain
    (follow-chain (conj chain next) (get deps next) deps)))

(comment
  (let [bundles (deps/bundles)
        deps (deps/package-depmap deps/select-packages-ddf-only bundles)
        name (key (last deps))
        links (val (last deps))]
    (reduce
      (fn [chain next]
        (follow-chain chain next deps))
      []
      links)
    #_(reduce
        (fn [out in]
          (conj (pop out) (conj))
          #_(if (= (count in) 1)
              (conj (pop out) (conj (last out) (first in)))
              (conj out in)))
        [[]] deps))

  #_())