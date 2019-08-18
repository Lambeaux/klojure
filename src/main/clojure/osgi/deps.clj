(ns osgi.deps
  "The deps namespace is for functions that help manipulate OSGi metadata and
  dependencies."
  (:require [osgi.core :as osgi]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [dorothy.core :as dot]
            [dorothy.jvm :refer [save! show!]]))

(defn render [graph]
  (let [png "/tmp/render.png"]
    (-> graph dot/dot (save! png {:format :png :layout :circo}))
    #_(sh "open" png)
    #_(sh "rm" png)))

(defn view-by-show [graph]
  (-> (dot/digraph graph)
      (dot/dot)
      (show! {:layout :neato})))

(defn view-after-save [graph props]
  (let [file "/tmp/render.svg"]
    (-> graph dot/dot (save! file props))
    (sh "open" file)))

(defn get-deps [bundles]
  (->> bundles
       (mapcat (fn [bundle]
                 (map #(-> [(first %) (:name bundle)])
                      (get-in bundle [:headers :export-package]))))
       (into {})))

(defn package-deps []
  (let [bundles (->> (osgi/bundle-list)
                     (map osgi/bundle->map))
        deps (get-deps bundles)]
    (dot/digraph
      [;(map #(-> [(:id %) {:label (:name %) :shape :box}]) bundles)
       (->> bundles
            (filter #(re-matches #"catalog-ui-search" (:name %)))
            (mapcat
              (fn [bundle]
                (map #(-> [(:name bundle) (get deps (first %))])
                     (get-in bundle [:headers :import-package])))))])))

(defn service-deps []
  (let [bundles (map osgi/bundle->map (take-last 10 (osgi/bundle-list)))]
    (dot/digraph
      [(map #(-> [(:id %) {:label (:name %) :shape :box}]) bundles)])))

(defn
  feature->map
  [f] {
       :id            (.getId f)
       :name          (.getName f)
       :description   (.getDescription f)
       :details       (.getDetails f)
       :version       (.getVersion f)
       :resolver      (.getResolver f)
       :install       (.getInstall f)
       :isHidden      (.isHidden f)
       :dependencies  (map (fn [d] {
                                    :name         (.getName d)
                                    :version      (.getVersion d)
                                    :isPrereq     (.isPrerequisite d)
                                    :isDependency (.isDependency d)})
                           (.getDependencies f))
       :bundles       (map (fn [b] {
                                    :location         (.getLocation b)
                                    :originalLocation (.getOriginalLocation b)
                                    :startLevel       (.getStartLevel b)
                                    :isStart          (.isStart b)
                                    :isDependency     (.isDependency b)})
                           (.getBundles f))
       :startLevel    (.getStartLevel f)
       :namespace     (.getNamespace f)
       :resourceRepos (.getResourceRepositories f)
       :repoUrl       (.getRepositoryUrl f)})

; ##################################################################

(defn bundles
  ([] (bundles (constantly true)))
  ([f]
   (->> (osgi/bundle-list)
        (map osgi/bundle->map)
        (filter f))))

(defn features []
  (->> (osgi/list-features)
       (map feature->map)))

(comment
  (->> (bundles (fn [x] true))
       (filter #(.contains (:name %) "directorymonitor"))
       (first)))

(comment
  (->> (features)
       (filter #(.contains (:name %) "directorymonitor"))
       (first)))

; ##################################################################

(defn bundles-partitioned
  "Example function that partitions bundles. Takes a
  coll of bundle defs and returns a coll of bundle def colls."
  [bundles]
  (let [cushion 1
        id-set (set (map :id bundles))]
    (->> bundles
         (partition-by
           #(and
              (contains? id-set (+ (:id %) cushion))
              (contains? id-set (- (:id %) cushion)))))))
(comment
  (map :name (bundles (fn [x] true)))
  (->> (bundles (fn [x] true))
       (bundles-partitioned)
       (map #(map :name %)))
  #_ ())


(defn get-cdm-bundle
  "Fetches the Content Directory Monitor bundle. Useful
  for examples and testing."
  []
  (->> (bundles (fn [x] true))
       (filter #(.contains (:name %) "directorymonitor"))
       (first)))

(defn get-klojure-bundle
  "Fetches the Content Directory Monitor bundle. Useful
  for examples and testing."
  []
  (->> (bundles (fn [x] true))
       (filter #(.contains (:name %) "klojure"))
       (first)))

(defn select-all
  "Predicate that always returns true, effectively filters
  nothing when a selection is needed. Works for any arg."
  [any]
  true)

(defn selectf-bundles-built-by
  "Predicate factory for bundle defs that eval true if a match is found
  against the name of the user who built the bundle."
  [user]
  (fn [b] (->> b (:headers) (:built-by) (= user))))

(defn selectf-bundles-by-name
  "Predicate factory for bundle defs that eval true if a match is found
  against the bundle name, if it contains the provided text."
  [text]
  (fn [b] (.contains (:name b) text)))

(defn select-packages-ddf-only
  "Predicate for package strings that only evals to true
  when the package is a direct DDF package, not third party."
  [p]
  (or (.contains p "org.codice") (.contains p "ddf.")))
(comment
  (select-packages-ddf-only "org.codice.ddf.catalog.monitor")
  (select-packages-ddf-only "ddf.data.types")
  (select-packages-ddf-only "net.opensaml.pki"))

(defn package-import-map
  "Turns a coll of bundle defs into a map of bundle name (key)
  to a coll of package names (value) imported by that bundle.
  Curries f as the package filter."
  [f bundles]
  (->> bundles
       (map (fn [bundle]
              (let [name (:name bundle)]
                {name (->> bundle
                           (:headers)
                           (:import-package)
                           (map key)
                           (filter f))})))
       (into {})))
(comment
  (package-import-map select-all (bundles))
  (package-import-map select-packages-ddf-only (bundles)))


(defn- bundle-extract-header-info [k]
  (fn [bundles]
    (->> bundles
         (map (fn [bundle]
                (let [name (:name bundle)]
                  {name (->> bundle
                             (:headers)
                             k
                             (map key))})))
         (into {}))))

(def bundle-imports (bundle-extract-header-info :import-package))
(def bundle-exports (bundle-extract-header-info :export-package))

(defn flat-map-invert
  "{:key [1 2 3]} => {1 :key 2 :key 3 :key}"
  [m]
  (into {}
        (map
          (fn [[k vs]]
            (map vector vs (repeat k)))
          m)))

(comment
  (flat-map-invert {:key [1 2 3]})
  (bundle-imports (bundles (selectf-bundles-built-by "lambeaux")))
  (bundle-exports (bundles (selectf-bundles-built-by "lambeaux"))))

(defn join [m1 m2])


; This is the backwards guy
(defn package-export-map
  "Turns a coll of bundle defs into a map of package name (key)
  to a bundle name (value), which is the exporter of that package.
  Curries f as the package filter."
  [f bundles]
  (->> bundles
       (map (fn [bundle]
              (let [name (:name bundle)]
                (->> bundle
                     (:headers)
                     (:export-package)
                     (map key)
                     (filter f)
                     (map (fn [package] {package name}))))))
       (flatten)
       (into {})))

(comment
  (package-export-map select-all (bundles))
  (package-export-map select-packages-ddf-only (bundles)))

bundle -> exports

bundle -> imports

bundle -> imports <-> exports <- bundle
bundle -> consumes <-> supplies <- bundle

bundle -> [bundle]

(defn package-depmap
  "Turns a coll of bundle defs into an edge list of
  bundle dependencies using packages to define the links.
  Curries f as the package filter."
  [f bundles]
  (let [imports (package-import-map f bundles)
        exports (package-export-map f bundles)]
    (into {}
      (map
        (fn [[bundle package-imports]]
          [bundle (->> package-imports
                       (map #(get exports %))
                       (filter #(not (= nil %)))
                       (set))])
        imports))))
(comment
  (package-depmap select-all (bundles))
  (package-depmap select-packages-ddf-only (bundles)))


(defn depmap->edge-list
  "Given a dependency map of node name (key) to a coll
  of node names (value) will return a new coll of vector
  pairs representing the dependency map's edges."
  [m]
  (mapcat
    (fn [[k v]]
      (map vector (repeat k) v)) m))
(comment
  (depmap->edge-list {"a" ["x" "y" "z"], "b" ["x" "s" "t"]}))

; ##################################################################

(defn layer-create-edge
  "Used for mapping bundle defs to graphviz data structures."
  [f bundles]
  (into [{}
         (dot/edge-attrs {:color :black})]
        (depmap->edge-list (package-depmap f bundles))))


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
       (bundles-partitioned)
       (map-indexed
         (fn [idx bundles]
           (let [cluster (keyword (str "cluster_" idx))]
             (layer-create-node cluster (str "test_" idx) (map :name bundles)))))))

; ##################################################################

(->> (bundles)
     (select-bundles-built-by "lambeaux")
     (select-bundles-by-name "solr"))

(comment

  ; Create a graph of just catalog bundles, limiting dependencies to DDF packages only
  (let [bundles (bundles
                  (and
                    (selectf-bundles-built-by "lambeaux")
                    (selectf-bundles-by-name "solr")))]
    (view-after-save
      (dot/digraph
        (into
          [(dot/subgraph :edges (layer-create-edge select-all bundles))]
          (layer-bulkcreate-nodes bundles)))
      {:format :svg :layout :dot}))

  #_ ())

; Basic graphviz data sample
(comment
  (view-after-save [
                    ; Define the nodes
                    [:a {}]
                    [:b]
                    [:c]
                    ; Define the edges
                    [:a :b]
                    [:a :c]
                    [:b :c {:arrowhead :empty}]]))

; More advanced graphviz data sample, from http://www.graphviz.org/content/cluster
(comment
  (view-after-save
    (dot/digraph
      [(dot/subgraph
         :cluster_0 [{:style :filled, :color :lightgrey, :label "platform"}
                     (dot/node-attrs {:style :filled, :color :white})
                     [:a0]
                     [:a1]
                     [:a2]
                     [:a3]
                     #_[:a0 :> :a1 :> :a2 :> :a3]])
       (dot/subgraph
         :cluster_1 [{:color :blue, :label "catalog"}
                     (dot/node-attrs {:style :filled})
                     [:b0]
                     [:b3]
                     #_[:b0 :> :b1]
                     #_[:b2 :> :b3]])
       (dot/subgraph
         :cluster_2 [{:color :green, :label "transformer"}
                     (dot/node-attrs {:style :filled})
                     [:b1]
                     [:b2]
                     [:b4]
                     #_[:b1 :> :b2]
                     #_[:b1 :> :b4]])
       (dot/subgraph
         :cluster_3 [{:color :purple, :label "feature"}
                     [:b0]
                     [:b1]
                     [:b2]])
       (dot/subgraph
         :bundleDeps [
                      {}
                      (dot/edge-attrs {:color :red})
                      #_ [:start :a0]
                      #_[:start :b0]
                      [:a1 :b3]
                      [:b2 :a3]
                      [:a3 :a0]
                      #_[:a3 :end]
                      #_[:b3 :end]

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

(comment
  (clojure.pprint/pprint (map :location (map osgi/bundle->map (osgi/bundle-list))))
  (render (package-deps))
  (render (service-deps)))

(defn merge-left
  "Doc"
  [v]
  (reduce
    (fn [out in]
      (if (= (count in) 1)
        (conj (pop out) (conj (last out) (first in)))
        (conj out in)))
    [[]] v))

(comment
  (merge-left [[1] [2] [3 4 5] [6] [7] [8 9 10] [11] [12]]))