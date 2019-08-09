(ns osgi.deps
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
  (let [png "/tmp/render.png"]
    (-> graph dot/dot (save! png props))
    (sh "open" png)))

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

(defn bundles []
  (->> (osgi/bundle-list)
       (map osgi/bundle->map)))

(defn features []
  (->> (osgi/list-features)
       (map feature->map)))

(comment
  (->> (bundles)
       (filter #(.contains (:name %) "directorymonitor"))
       (first)))

(comment
  (->> (features)
       (filter #(.contains (:name %) "directorymonitor"))
       (first)))

(defn exported-packages-in-ddf [bundle]
  (let [name (:name bundle)]
    (->> bundle
         (:headers)
         (:export-package)
         (map key)
         (filter #(.contains % "org.codice"))
         (map (fn [package] {package name})))))

(comment
  (->> (bundles)
       (map exported-packages-in-ddf)
       (flatten)
       (into (sorted-map))))

(comment (view-after-save (package-deps) {:format :png, :layout :dot}))

(comment (let [bundles (->> (osgi/bundle-list)
                            (map osgi/bundle->map))
               deps (get-deps bundles)]
              (->> bundles
                   (filter #(re-matches #"catalog-ui-search" (:name %)))
                   (mapcat
                     (fn [bundle]
                       (map #(-> [(:name bundle) (get deps (first %))])
                            (get-in bundle [:headers :import-package])))))))

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

; From http://www.graphviz.org/content/cluster
(comment
  (view-after-save
    (dot/digraph
      [(dot/subgraph :cluster_0 [{:style :filled, :color :lightgrey, :label "platform"}
                                 (dot/node-attrs {:style :filled, :color :white})
                                 [:a0]
                                 [:a1]
                                 [:a2]
                                 [:a3]
                                 #_[:a0 :> :a1 :> :a2 :> :a3]])
       (dot/subgraph :cluster_1 [{:color :blue, :label "catalog"}
                                 (dot/node-attrs {:style :filled})
                                 [:b0]
                                 [:b3]
                                 #_[:b0 :> :b1]
                                 #_[:b2 :> :b3]])
       (dot/subgraph :cluster_2 [{:color :green, :label "transformer"}
                                 (dot/node-attrs {:style :filled})
                                 [:b1]
                                 [:b2]
                                 [:b4]
                                 #_[:b1 :> :b2]
                                 #_[:b1 :> :b4]])
       (dot/subgraph :cluster_3 [{:color :purple, :label "feature"}
                                 [:b0]
                                 [:b1]
                                 [:b2]])
       (dot/subgraph :bundleDeps [
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
       (dot/subgraph :otherDeps [{}
                                 (dot/edge-attrs {:color :black})
                                 [:b4 :b2]])])
    {:format :png :layout :dot}))

(comment
  (clojure.pprint/pprint (map :location (map osgi/bundle->map (osgi/bundle-list))))
  (render (package-deps))
  (render (service-deps)))
