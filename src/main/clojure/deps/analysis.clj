(ns deps.analysis
  "Namespace for composing and rendering data and visualizations."
  (:require [osgi.core :as osgi]
            [deps.common :as c]
            [deps.bundles :as b]
            [deps.artifacts :as a]
            [clojure.java.shell :refer [sh]]
            [dorothy.core :as dot]
            [dorothy.jvm :refer [save! show!]]))

(defn- user-name []
  (System/getProperty "user.name"))

(def bundle-pars
  ["spatial-"
   "catalog-ui"
   "catalog-validator-"
   "catalog-transformer-"
   "catalog-security-"
   "catalog-rest-"
   "catalog-plugin-"
   "catalog-opensearch-"
   "catalog-core-"
   "catalog-admin-"
   "catalog-"
   "security-sts-"
   "security-servlet-"
   "security-rest-"
   "security-idp-"
   "security-handler-"
   "security-"
   "session-"
   "platform-"
   "persistence-"
   "mime-"
   "metrics-"
   "admin-"
   "action-"
   "solr-"])

;; ----------------------------------------------------------------------
;; # Rendering functions / graph support
;;
;; Reference for writing graphviz graphs with dorothy and the tools for viewing them.

(def graph-properties
  {:formats [:png :svg]
   :layouts [:dot :neato :circo]})

(defn view-by-show
  "Draws the graph using Java Swing."
  [graph]
  (-> (dot/digraph graph)
      (dot/dot)
      (show! {:layout :neato})))

(defn view-after-save
  "Draws the graph by writing a file to /tmp before opening it with the system's
  default viewing software."
  ([graph]
   (view-after-save graph {:format :svg :layout :dot}))
  ([graph props]
   (let [file "/tmp/render.svg"]
     (-> graph dot/dot (save! file props))
     (sh "open" file))))

(defn layer-create-edge
  "Used for mapping bundle defs to graphviz data structures."
  [f bundles]
  (into [{}
         (dot/edge-attrs {:color :black})]
        (c/collmap-collapsed (b/package-depmap f bundles))))

(defn layer-create-node
  "Used for mapping bundle defs to graphviz data structures."
  [layer-name layer-label bundles]
  (dot/subgraph
    layer-name
    (into [{:color :blue :label layer-label}
           (dot/node-attrs {:style :filled})]
          (map vector bundles))))

(defn layer-bulkcreate-nodes
  "Used for mapping bundle defs to graphviz data structures."
  [bundles]
  (->> bundles
       (b/partition-bundles-by-id)
       (map-indexed
           (fn [idx bundles]
             (let [cluster (keyword (str "cluster_" idx))]
               (layer-create-node cluster (str "test_" idx) (map :name bundles)))))))

(defn- graph-from-edges
  "Make a graph with only an edge list as input."
  [edges]
  (dot/digraph [(dot/subgraph :edges (into [{} (dot/edge-attrs {:color :black})] edges))]))

;; ----------------------------------------------------------------------
;; # Graphing experiments
;;
;; Define, document, and iterate on your graphs here.

(defn- partition-name-map
  ;; EXPERIMENTAL
  "Stolen from 'b/partition-bundles-by-name' so I can select the keys I care about."
  [bundles]
  (let [names bundle-pars
        first-match
        (fn [substrings bundle]
          (let [bundle-name (:name bundle)]
            (some #(when (.contains bundle-name %) %) substrings)))]
    (reduce
      (fn [out in]
        (let [group-name (first-match names in)
              group (get out group-name)]
          (assoc out group-name (conj group in))))
      {}
      bundles)))

(comment
  (partition-name-map (osgi/bundles)))

(defn- partitions-of-locations
  ;; EXPERIMENTAL
  "Bundle defs -> partition name map of just locations."
  [bundles]
  (->> bundles
       (partition-name-map)
       (map (fn [[key coll]] [key (map #(:location %) coll)]))
       (into {})))

(comment
  (partitions-of-locations (osgi/bundles)))

(defn- select-partitions
  ;; EXPERIMENTAL
  "Given a vector of keys and a map, aggregate the values into a single seq."
  [ks m]
  (reduce
    (fn [out in] (concat out (get m in)))
    '()
    ks))

(comment
  (select-partitions [:a :b :d] {:a [1 2 3] :b [4 5 6] :c [7 8 9] :d [11 12 13]}))

(defn- location->dep-edges
  ;; EXPERIMENTAL
  "Given a filter string and an artifact string, return an edge list for transitives matching the
  filter string."
  [filterstr artifact-location]
  (->> (do (println (str "Processing " artifact-location)) artifact-location)
       a/artifact-string->coord
       a/artifact-tree
       (a/artifact-tree-filter #(.contains (:art %) filterstr))
       (a/artifact-tree-edges artifact-location)
       (map (fn [[left right]] [(subs left 4) (subs right 4)]))
       (into [])))

(comment
  (location->dep-edges "guava" "mvn:ddf.catalog.core/catalog-core-api-impl/2.13.1"))

(comment
  ;; preview of selection
  (->> (osgi/bundles)
       (filter (b/select-bundles-built-by (user-name)))
       (partitions-of-locations)
       (select-partitions ["security-sts-"
                           "security-servlet-"
                           "security-rest-"
                           "security-idp-"
                           "security-handler-"
                           "security-"])
       #_(map #(location->dep-edges "guava" %)))
  ;; graphviz must be doing something special with colon ':' in the graph definition
  "Graph of transitive dependencies captured from Maven metadata."
  (view-after-save
    (graph-from-edges
      (location->dep-edges "guava" "mvn:ddf.catalog.core/catalog-core-api-impl/2.13.1"))
    {:format :svg :layout :dot}))

(comment
  "List of DDF bundle names"
  (->> (osgi/bundles)
       (filter (b/select-bundles-built-by (user-name)))
       (map :name)))

(comment
  "List of DDF bundle names partitioned into name groups"
  (let [pars bundle-pars]
    (->> (osgi/bundles)
         (filter (b/select-bundles-built-by (user-name)))
         (b/partition-bundles-by-name pars)
         (map (fn [coll] (map #(:name %) coll))))))

(comment
  "Graph of the security bundles, limiting dependencies to DDF packages only."
  (let [bundles (->> (osgi/bundles)
                     (filter
                       (b/select-on-all
                         [(b/select-bundles-built-by (user-name))
                          (b/select-bundles-by-name "security-")])))]
    (view-after-save
      (dot/digraph
        (into
          [(dot/subgraph :edges (layer-create-edge
                                  (b/select-packages-ddf-only) bundles))]
          (layer-bulkcreate-nodes bundles)))
      {:format :svg :layout :dot})))

(comment
  "Basic graphviz data sample"
  (view-after-save [
                    ; Define the nodes
                    [:a {}]
                    [:b]
                    [:c]
                    ; Define the edges
                    [:a :b]
                    [:a :c]
                    [:b :c {:arrowhead :empty}]]))

(comment
  "More advanced graphviz data sample, from http://www.graphviz.org/content/cluster"
  (view-after-save
    (dot/digraph
      [#_(dot/subgraph
           :cluster_0 [{:style :filled, :color :lightgrey, :label "platform"}
                       (dot/node-attrs {:style :filled, :color :white})
                       [:a0] [:a1] [:a2] [:a3]])
       #_(dot/subgraph
           :cluster_1 [{:color :blue, :label "catalog"}
                       (dot/node-attrs {:style :filled})
                       [:b0] [:b3]])
       #_(dot/subgraph
           :cluster_2 [{:color :green, :label "transformer"}
                       (dot/node-attrs {:style :filled})
                       [:b1] [:b2] [:b4]])
       #_(dot/subgraph
           :cluster_3 [{:color :purple, :label "feature"}
                       #_(dot/node-attrs {:style :filled})
                       [:c1] [:c2] [:c3]])
       (dot/subgraph
         :bundleDeps [
                      {}
                      (dot/edge-attrs {:color :red})
                      [:a1 :b3]
                      [:b2 :a3]
                      [:a3 :a0]
                      [:a0 :> :a1 :> :a2 :> :a3]
                      [:b0 :> :b1]
                      [:b2 :> :b3]
                      [:b1 :> :b2]
                      [:b1 :> :b4]])
       (dot/subgraph
         :otherDeps [{}
                     (dot/edge-attrs {:color :black})
                     [:b4 :b2]])])
    {:format :svg :layout :dot}))