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

(defn feature->dependency-map [d] {
                                   :name (.getName d)
                                   :version (.getVersion d)
                                   :isPrereq (.isPrerequisite d)
                                   :isDependency (.isDependency d)})

(defn feature->bundle-info-map [bInf] {
                                       :location (.getLocation bInf)
                                       :originalLocation (.getOriginalLocation bInf)
                                       :startLevel (.getStartLevel bInf)
                                       :isStart (.isStart bInf)
                                       :isDependency (.isDependency bInf)})

(defn feature->map [f] {
                        :id           (.getId f)
                        :name         (.getName f)
                        :description  (.getDescription f)
                        :details      (.getDetails f)
                        :version      (.getVersion f)
                        :resolver     (.getResolver f)
                        :install      (.getInstall f)
                        :isHidden     (.isHidden f)
                        :dependencies (map feature->dependency-map (.getDependencies f))
                        :bundles      (map feature->bundle-info-map (.getBundles f))
                        :startLevel   (.getStartLevel f)
                        :namespace (.getNamespace f)
                        :resourceRepos (.getResourceRepositories f)
                        :repoUrl (.getRepositoryUrl f)})

(comment (->> (osgi/bundle-list)
              (map osgi/bundle->map)
              (filter #(.contains (:name %) "directorymonitor"))
              (first)))

(comment (->> (osgi/list-features)
              (map feature->map)
              (filter #(.contains (:name %) "directorymonitor"))
              (first)))
              ;(filter (.contains (:name ) "directorymonitor"))))

(comment
  (clojure.pprint/pprint (map :location (map osgi/bundle->map (osgi/bundle-list))))
  (render (package-deps))
  (render (service-deps)))
