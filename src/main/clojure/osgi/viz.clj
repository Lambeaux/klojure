(ns osgi.viz
  (:require [clojure.java.shell :refer [sh]]
            [dorothy.core :as dot]
            [dorothy.jvm :refer [save! show!]]))

(defn view-after-save [graph]
  (let [png "/tmp/render.png"]
    (-> graph dot/dot (save! png {:format :png :layout :dot}))
    (sh "open" png)))

(defn view-by-show [graph]
  (-> (dot/digraph graph)
      (dot/dot)
      (show! {:layout :neato})))


(comment
  (view-by-show [
                 ; Define the nodes
                 [:a {}]
                 [:b]
                 [:c]
                 ; Define the edges
                 [:a :b]
                 [:a :c]
                 [:b :c {:arrowhead :empty}]]))