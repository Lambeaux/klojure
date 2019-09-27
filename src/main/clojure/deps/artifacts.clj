(ns deps.artifacts
  "Namespace that supports queries against artifacts in the current user's .m2 directory
  and any pom XML files within."
  (:require [osgi.core :as osgi]
            [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.xml :as xml]))

(defn- user-home []
  (System/getProperty "user.home"))

(defn size-as-mb
  "Given a number representing bytes, returns the equivalent number
  of megabytes."
  [bytes]
  (/ bytes 1e+6))

(comment "Sample invocation." (size-as-mb 1000000))

(defn- artifact-path-with-ext
  "Given a maven coordinate, returns a string path to the file in the coordinate's .m2
  directory with the provided file extension (ext)."
  [mvn-coord ext]
  (let [group (:g mvn-coord)
        artifact-id (:a mvn-coord)
        version (:v mvn-coord)]
    (str (user-home)
         "/.m2/repository/"
         (s/replace group "." "/")
         "/" artifact-id "/" version "/" artifact-id "-" version ext)))

(defn- artifact-path-to-jar
  "Given a maven coordinate, returns a string path to the .jar file."
  [mvn-coord]
  (artifact-path-with-ext mvn-coord ".jar"))

(defn- artifact-path-to-pom
  "Given a maven coordinate, returns a string path to the .pom file."
  [mvn-coord]
  (artifact-path-with-ext mvn-coord ".pom"))

(comment
  "Sample invocations."
  (artifact-path-to-jar {:g "djblue.github.com" :a "klojure" :v "0.0.1-SNAPSHOT"})
  (artifact-path-to-pom {:g "djblue.github.com" :a "klojure" :v "0.0.1-SNAPSHOT"}))

(defn artifact-coord->string [mvn-coord]
  (let [g (:g mvn-coord)
        a (:a mvn-coord)
        v (:v mvn-coord)]
    (if (= nil (and g a v))
      (throw (IllegalArgumentException.
               (str "Invalid maven coordinate, g: " g " a: " a " v: " v)))
      (str "mvn:" g "/" a "/" v))))

(defn artifact-string->coord [mvn-string]
  (let [str-no-mvn (subs mvn-string 4)
        group (subs str-no-mvn 0 (.indexOf str-no-mvn "/"))
        str-no-group (subs str-no-mvn (+ 1 (count group)))
        artifact (subs str-no-group 0 (.indexOf str-no-group "/"))
        remaining (subs str-no-group (+ 1 (count artifact)))]
    {:g group :a artifact :v remaining}))

(comment
  (artifact-coord->string {:g "djblue.github.com" :a "klojure" :v "0.0.1-SNAPSHOT"})
  (artifact-string->coord "mvn:djblue.github.com/klojure/0.0.1-SNAPSHOT")
  (artifact-string->coord "mvn:ddf.admin.core/admin-core-configpolicy/2.18.0-SNAPSHOT"))

(defn artifact-size
  "Given a maven coordinate, returns the size of the artifact in megabytes."
  [mvn-coord]
  (let [path (artifact-path-to-jar mvn-coord)
        file (io/file path)]
    (size-as-mb (.length file))))

(comment
  "Sample invocation."
  (artifact-size {:g "djblue.github.com" :a "klojure" :v "0.0.1-SNAPSHOT"})
  (artifact-size {:g "ddf.platform.util",
                  :a "platform-util",
                  :v "2.16.0-SNAPSHOT"}))

(defn artifact-xml
  "Given a maven coordinate, returns the parsed XML data of the artifact's pom."
  [mvn-coord]
  (let [path (artifact-path-to-pom mvn-coord)
        file (io/file path)]
    (if (.exists file) (xml/parse file) {})))

(comment
  "Sample invocation."
  (artifact-xml {:g "djblue.github.com" :a "klojure" :v "0.0.1-SNAPSHOT"})
  (artifact-xml {:g "ddf.platform.util",
                 :a "platform-util",
                 :v "2.16.0-SNAPSHOT"}))

(defn- xml-reduction
  "Given f, a vector of xml tags (keywords) that form a path down the xml tree, and a parsed
  xml payload, reduce the xml payload by filtering for tags in path and returning the result
  of calling f on the filtered xml nodes.

  The document's root tag can optionally be the first element in path.
  Return semantics are defined by f."
  [f path xml]
  (if (empty? path)
    xml
    (let [root-tag (:tag xml)
          first-tag (first path)
          path-list (apply list path)
          p (if (= root-tag first-tag) (pop path-list) path-list)]
      (reduce
        (fn [out in]
          (let [filtered (filter #(= (:tag %) in) out)]
            (f filtered in)))
        (:content xml) p))))

(defn- xml-select
  ;; TODO Can we write a macro to add better support for multiple occurrences of multiple tags?
  ;; Would need to map-on-map for each subsequent multi-tag, then flatten only 1 level
  "Given a vector of xml tags (keywords) that form a path down the xml tree, and a parsed
  xml payload, return the contents of the final tag in the sequence. This function preserves
  the xml's grouping of elements, thus no tag but the final tag in the vector may appear
  multiple times in the xml document. Returns an empty list if no data was found or the path
  did not exist."
  [path xml]
  (xml-reduction
    (fn [filtered in]
      ;; This is not necessarily what we want
      ;; What if a pom has a clause with only 1 dependency in it?
      (if (= 1 (count filtered))
        (:content (first filtered))
        (map :content filtered)))
    path xml))

(defn- xml-select-single
  "Like xml-select, but always assumes filtering yields a single node for the whole path."
  [path xml]
  (xml-reduction
    (fn [filtered in]
      (:content (first filtered)))
    path xml))

(defn- xml-select-multi
  "Like xml-select, but always assumes filtering yields a single node until the last element
  of the path, which is assumed to be referring to multiple nodes in the current context."
  [path xml]
  (xml-reduction
    (fn [filtered in]
      (if (= in (last path))
        (map :content filtered)
        (:content (first filtered))))
    path xml))

(defn- xml-gather
  "Given a vector of xml tags (keywords) that form a path down the xml tree, and a parsed
  xml payload, return the contents of the final tag in the sequence. This function ignores
  the xml's grouping of elements, thus any tag may appear multiple times in the xml document.
  Returns an empty list if no data was found or the path did not exist."
  [path xml]
  (xml-reduction
    (fn [filtered in]
      (if (= 1 (count filtered))
        (:content (first filtered))
        (->> filtered
             (map :content)
             flatten
             (apply vector))))
    path xml))

(comment
  (artifact-xml
    {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1"})
  (xml-select
    [:project :dependencies :dependency]
    (artifact-xml
      {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1"}))
  (xml-select-single
    [:project :dependencies :dependency]
    (artifact-xml
      {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1"}))
  (xml-select-multi
    [:project :dependencies :dependency]
    (artifact-xml
      {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1"}))
  (xml-gather
    [:project :dependencies :dependency :artifactId]
    (artifact-xml
      {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1"})))

(defn- xml-aggregate
  "Takes a vector of different XML nodes and aggregates them into a single map. Does
  not preserve multiple values across the same tag; first declared wins."
  [xml-nodes]
  (reduce
    (fn [out in]
      (assoc out (:tag in) (first (:content in))))
    {} xml-nodes))

(comment
  (xml-aggregate
    [{:tag :groupId, :attrs nil, :content ["org.clojure"]}
     {:tag :artifactId, :attrs nil, :content ["clojure"]}
     {:tag :version, :attrs nil, :content ["1.9.0"]}
     {:tag :scope, :attrs nil, :content ["provided"]}]))

(comment
  (->> {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1"}
       artifact-xml
       (xml-select-multi [:project :dependencies :dependency])
       (map xml-aggregate)))

(defn- pom-props
  "Given a pom xml payload, returns a map of properties defined in the pom and used
  throughout the pom."
  [xml]
  (->> xml
       (xml-select-single [:properties])
       xml-aggregate))

(comment
  (->> {:g "djblue.github.com" :a "klojure" :v "0.0.1-SNAPSHOT"}
       artifact-xml
       pom-props))

(defn- pom-parent
  "Given a pom xml payload, returns the maven coordinate of the parent document."
  [xml]
  (let [content (xml-select-single [:parent] xml)]
    (if (nil? content)
      nil
      (let [parent (xml-aggregate content)]
        {:g (:groupId parent)
         :a (:artifactId parent)
         :v (:version parent)}))))

(comment
  (->> {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1"}
       artifact-xml
       pom-parent
       artifact-xml
       pom-parent))

(defn- pom-deps-management
  "Given a pom xml payload, get the deps from the dependency management clause."
  [xml]
  (let [content (xml-select-multi [:dependencyManagement :dependencies :dependency] xml)]
    (map xml-aggregate content)))

(comment
  "Get the raw xml only."
  (artifact-xml {:g "ddf.catalog", :a "catalog", :v "2.13.1"})
  "Get named pieces of the pom."
  (->> {:g "ddf.catalog", :a "catalog", :v "2.13.1"}
       artifact-xml
       pom-props)
  (->> {:g "ddf.catalog", :a "catalog", :v "2.13.1"}
       artifact-xml
       pom-parent)
  (->> {:g "ddf.catalog", :a "catalog", :v "2.13.1"}
       artifact-xml
       pom-deps-management))

(defn- pom-prop?
  "Returns true if the provided string is a pom property, false otherwise."
  [str]
  (and (not= str nil)
       (s/starts-with? str "${")
       (s/ends-with? str "}")))

(comment
  (pom-prop? nil)
  (pom-prop? "")
  (pom-prop? "karaf.version")
  (pom-prop? "${karaf.version")
  (pom-prop? "karaf.version}")
  (pom-prop? "${karaf.version}"))

(defn- pom-eval-prop
  "Given a string and a map of pom properties, determines if the string matches maven property
  substitution conventions, and if so, returns the value of the property. Returns the original
  string otherwise."
  [str props]
  (if (not (pom-prop? str))
    str
    (let [prop-value
          (->> (subs str 2 (- (count str) 1))
               (keyword)
               (get props))]
      (if (= prop-value nil) str prop-value))))

(comment
  (pom-eval-prop nil {})
  (pom-eval-prop "${karaf.version}" {:karaf.version "4.1.0"})
  (pom-eval-prop "${karaf.version}" {})
  (pom-eval-prop "4.1.0" {}))

(defn pom-deps
  "Given a pom xml payload, get the deps from the dependencies clause."
  [xml]
  (let [props (pom-props xml)
        content (xml-select-multi [:dependencies :dependency] xml)]
    (->> content
         (map xml-aggregate)
         (map (fn [m] {:g (:groupId m)
                       :a (:artifactId m)
                       :v (pom-eval-prop (:version m) props)
                       :s (:scope m)})))))

(comment
  "Get the dependencies of the provided artifact."
  (->> {:g "djblue.github.com" :a "klojure" :v "0.0.1-SNAPSHOT"}
       artifact-xml
       pom-deps))

(defn- pom-parent-chain
  "Aggregates data into a map from the entire pom parent chain."
  [xml]
  (let [parent (pom-parent xml)]
    (if (nil? parent)
      {:properties {:project.version
                    (->> xml
                         (xml-select-single [:version])
                         first)}
       :dependencyManagement #{}}
      (let [parent-xml (artifact-xml parent)
            chain (pom-parent-chain parent-xml)]
        {:properties
         (into (:properties chain) (pom-props parent-xml))
         :dependencyManagement
         (into (:dependencyManagement chain) (pom-deps-management parent-xml))}))))

(defn- pom-parent-chain-itr
  "Aggregates data into a map from the entire pom parent chain, but does so iteratively."
  [xml]
  (let [xml-chain
        (loop [pars (conj '() xml)]
          (let [p (pom-parent (peek pars))]
            (if (nil? p) pars
              (recur (conj pars (artifact-xml p))))))]
    (reduce
      (fn [out in]
        {:properties
         (into (:properties out) (pom-props in))
         :dependencyManagement
         (into (:dependencyManagement out) (pom-deps-management in))})
      {:properties
       {:project.version
        (->> (peek xml-chain) (xml-select-single [:version]) first)
        :project.groupId ;; I think this is a bad assumption
        (->> (peek xml-chain) (xml-select-single [:groupId]) first)}
       :dependencyManagement #{}}
      xml-chain)))

(comment
  (->> {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1"}
       artifact-xml
       pom-parent-chain-itr))

(comment
  ;;
  ;; Tracing the parent
  (up-one-level {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1"})
  (up-one-level {:g "ddf.catalog.core", :a "core", :v "2.13.1"})
  (up-one-level {:g "ddf.catalog", :a "catalog", :v "2.13.1"})
  (up-one-level {:g "ddf", :a "ddf", :v "2.13.1"})
  ;;
  ;; Debugging the chain
  (->> {:g "ddf", :a "ddf", :v "2.13.1"}
       artifact-xml
       pom-parent-chain-itr)
  (->> {:g "ddf.catalog", :a "catalog", :v "2.13.1"}
       artifact-xml
       pom-parent-chain-itr)
  (->> {:g "ddf.catalog.core", :a "core", :v "2.13.1"}
       artifact-xml
       pom-parent-chain-itr)
  (->> {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1"}
       artifact-xml
       pom-parent-chain-itr))

(defn pom-deps-deep
  "Given a pom xml payload, get the deps from the dependencies clause, and will visit all
  parent poms to ensure the returned information is accurate and complete, with all versions
  properly specified."
  [xml]
  (let [deps (pom-deps xml)
        chain (pom-parent-chain-itr xml)
        props (:properties chain)
        dep-mgmt (:dependencyManagement chain)]
    (concat
      (reduce
        (fn [out in]
          (do
            #_(println in)
            (let [g (:g in)
                  a (:a in)
                  v (:v in)]
              (conj out {:g (pom-eval-prop g props)
                         :a a
                         :v (if (nil? v)
                              (let [f (fn [d] (and (= g (:groupId d)) (= a (:artifactId d))))
                                    managed (->> dep-mgmt (filter f) first)]
                                (pom-eval-prop (:version managed) props))
                              (pom-eval-prop v props))})
              #_(if (nil? v)
                  (let [f (fn [d] (and (= g (:groupId d)) (= a (:artifactId d))))
                        managed (->> dep-mgmt (filter f) first)]
                    (->> props (pom-eval-prop (:version managed)) (assoc in :v) (conj out)))
                  (->> props (pom-eval-prop v) (assoc in :v) (conj out))))))
        [] deps))))

;; Possibly useful for hiding the XML args so that, publicly, only coords are used.
;;
(defn artifact-deps
  "Given a maven coordinate, returns a coll of maven coordinates that are dependencies
  of the provided artifact."
  [mvn-coord]
  ())

(defn- artifact-coord?->string
  "Supports null versions"
  [coord]
  (let [v (:v coord)]
    (if (nil? v)
      (artifact-coord->string (assoc coord :v "nil"))
      (artifact-coord->string coord))))

(defn artifact-tree
  "Given a maven coordinate, returns the dependency tree."
  [mvn-coord]
  (let [first-layer (filter #(not= (:s %) "test") (pom-deps-deep (artifact-xml mvn-coord)))]
    (map #(let [m {:art (artifact-coord?->string %)}
                deps (artifact-tree %)]
            (if (empty? deps) m (assoc m :deps deps)))
          first-layer)))

;;
;; TODO Revisit this impl later, recursive soln without test scope is good enough for now.
;; More flexibility will be needed later, however.
(defn- artifact-tree-itr
  "Given a maven coordinate, returns the dependency tree, but does so iteratively."
  [coord]
  (let [target coord
        next (pom-deps-deep (artifact-xml coord))
        prev '()
        result {}]
    (loop []
      ())))

;;
;; Tools for testing the primitives

(defn down-one-level [mvn-coord]
  (->> mvn-coord
       artifact-xml
       pom-deps))

(defn down-one-level-deep [mvn-coord]
  (->> mvn-coord
       artifact-xml
       pom-deps-deep
       (filter #(not= (:s %) "test"))))

(defn up-one-level [coord]
  (->> coord
       artifact-xml
       pom-parent))

(defn get-chain [coord]
  (->> coord
       artifact-xml
       pom-parent-chain-itr))

(comment

  ;;
  ;; Tree seq experiment, but does not preserve depth levels.

  (tree-seq
    #(> (count (down-one-level-deep %)) 0)
    down-one-level-deep
    {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1"})

  ;;
  ;; Samples to experiment with.

  (down-one-level
    {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1"})
  (down-one-level-deep
    {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1"})
  (get-chain
    {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1"})

  (down-one-level
    {:g "djblue.github.com" :a "klojure" :v "0.0.1-SNAPSHOT"})
  (down-one-level-deep
    {:g "djblue.github.com" :a "klojure" :v "0.0.1-SNAPSHOT"})
  (get-chain
    {:g "djblue.github.com" :a "klojure" :v "0.0.1-SNAPSHOT"})

  (down-one-level
    {:g "org.apache.karaf.shell", :a "org.apache.karaf.shell.core", :v "4.1.2"})
  (down-one-level-deep
    {:g "org.apache.karaf.shell", :a "org.apache.karaf.shell.core", :v "4.1.2"})
  (get-chain
    {:g "org.apache.karaf.shell", :a "org.apache.karaf.shell.core", :v "4.1.2"}))

(defn- or-reduce
  "Turns the or macro into a function."
  [p coll]
  (reduce (fn [x y] (or x (p y))) false coll))

(comment
  (or-reduce #(.contains % "a") ["b" "c" "d" "e"]))

(defn- keep?
  "Determines if a branch of a tree can be kept or not."
  [p a]
  (let [deps (:deps a)
        keep-me (p a)]
    (if (nil? deps) keep-me (or keep-me (or-reduce #(keep? p %) deps)))))

(comment
  (keep? #(.contains (:art %) "a")
         {:art "c"
          :deps (list {:art "b"
                       :deps (list {:art "a"})})}))

(defn artifact-tree-filter
  "Filters entire branches based upon the predicate."
  [p tree]
  (->> tree
       (map
         #(let [art (:art %)
                deps (:deps %)]
            (if (nil? deps)
              {:art art}
              {:art art :deps (artifact-tree-filter p deps)})))
       (filter #(keep? p %))))

(defn artifact-tree-edges
  "Create an edge list from a deeply-nested artifact tree."
  [parent tree]
  (reduce
    (fn [out in]
      (let [art (:art in)
            deps (:deps in)]
        (if (or (nil? deps) (empty? deps))
          (conj out [parent art])
          (let [edges (artifact-tree-edges art deps)]
            (into (conj out [parent art]) edges)))))
    #{} tree))

(def data-tree
  (list
   {:art "alpha"
    :deps (list {:art "delta"
                 :deps (list {:art "oscar"})}
                {:art "echo"
                 :deps (list {:art "oscar"}
                             {:art "mike"})})}
   {:art "bravo"
    :deps (list {:art "foxtrot"
                 :deps (list {:art "zulu"})}
                {:art "gulf"
                 :deps (list {:art "mike"})}
                {:art "hotel"})}
   {:art "charlie"
    :deps nil}
   {:art "niner"
    :deps (list {:art "mike"})}))

(comment
  (let [ex (list {:art "a"} {:art "b"} {:art "c"
                                        :deps (list {:art "1"} {:art "2"})})]
    (artifact-tree-edges "STEVE" ex))
  (artifact-tree-edges "MASTER" data-tree)
  ;; testing the full thing
  (let [root {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1"}
        root-name (artifact-coord->string root)]
    (->> root
         artifact-tree
         (artifact-tree-filter #(.contains (:art %) "guava"))
         (artifact-tree-edges root-name)
         (into []))))

(comment
  (do data-tree)
  (artifact-tree-filter #(.contains (:art %) "zulu") data-tree)
  ;; opensaml
  (->> {:g "org.opensaml" :a "opensaml-saml-impl" :v "3.3.0"}
       artifact-tree
       #_(artifact-tree-filter #(.contains (:art %) "guava")))
  ;; Guava
  (->> {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1"}
       artifact-tree
       (artifact-tree-filter #(.contains (:art %) "guava")))
  ;; OpenEJB
  (->> {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1"}
       artifact-tree
       (artifact-tree-filter #(.contains (:art %) "openejb")))
  ;; Log4j
  (->> {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1"}
       artifact-tree
       (artifact-tree-filter #(.contains (:art %) "log4j")))
  ;; Java EE
  (->> {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1"}
       artifact-tree
       (artifact-tree-filter #(.contains (:art %) "javaee-api")))
  ;; Javax Mail
  (->> {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1"}
       artifact-tree
       (artifact-tree-filter #(.contains (:art %) "javax.mail")))
  ;;
  ;; New stuff will aggregate the missing version problem
  ;; We're also filtering out test scope
  ;; TODO - Need to fix Maven vars elsewhere, i.e.
  (comment {:art "mvn:${cxf.asm.groupId}/${cxf.asm.artifactId}/3.3.1"})
  ;;
  (artifact-tree {:g "djblue.github.com" :a "klojure" :v "0.0.1-SNAPSHOT"})
  (artifact-tree {:g "org.clojure", :a "clojure", :v "1.9.0"})
  ;;
  ;; Best test case for Guava
  (down-one-level-deep {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1"})
  (artifact-tree {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1"})

  ;;
  ;; Subtree / branch filtering
  ;; Determine if an entire path is relevant or not
  ;; TODO - Recursive, make iterative later
  ;; test with openejb, log4j, c3p0, javaee-api, or javax.mail
  (let [dep-tree
        (artifact-tree {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1"})
        pred #(.contains (:art %) "guava")
        mapper (fn [x] {})]

    (map mapper dep-tree))
  ;;
  ;; Edge list?
  ;; (1) Go down length of tree, (2) aggregate dep list with parent name held constant,
  ;; (3) remove dupes
  (let [tree
        (artifact-tree {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1"})
        rdx
        (fn [])]
    ()))

(comment
  "Output for one level of dependency information."
  ({:g "org.clojure", :a "clojure", :v "1.9.0", :s nil}
   {:g "org.clojure", :a "test.check", :v "0.9.0", :s nil}
   {:g "org.clojure", :a "tools.nrepl", :v "0.2.12", :s nil}
   {:g "com.bhauman", :a "rebel-readline", :v "0.1.4", :s nil}
   {:g "cljfmt", :a "cljfmt", :v "0.5.1", :s nil}
   {:g "dorothy", :a "dorothy", :v "0.0.7", :s nil}
   {:g "org.apache.felix", :a "org.apache.felix.utils", :v "1.10.2", :s nil}
   {:g "org.slf4j", :a "slf4j-api", :v "1.7.24", :s "provided"}
   {:g "org.osgi", :a "org.osgi.core", :v "6.0.0", :s "provided"}
   {:g "org.apache.karaf.shell", :a "org.apache.karaf.shell.core", :v "4.1.2", :s "provided"}
   {:g "org.apache.shiro", :a "shiro-core", :v "1.4.0", :s "provided"}
   {:g "ddf.security", :a "ddf-security-common", :v "2.13.1", :s "provided"}
   {:g "ddf.catalog.core", :a "catalog-core-api", :v "2.13.1", :s "provided"}
   {:g "ddf.platform.util", :a "platform-util", :v "2.13.1", :s nil}
   {:g "ddf.catalog.core", :a "catalog-core-api-impl", :v "2.13.1", :s nil}
   {:g "ddf.catalog.core", :a "catalog-core-standardframework", :v "2.13.1", :s "provided"}))

(defn artifacts-embedded
  "Returns a seq of maps representing JARs."
  []
  (->> (osgi/bundles)
       (map (fn [bundle] (get-in bundle [:headers :embedded-artifacts])))
       (mapcat vals)
       (frequencies)
       (map (fn [[artifact count]]
              (let [size (artifact-size artifact)]
                {:artifact artifact
                 :count count
                 :size size
                 :total-size (* size count)})))))

(comment
  "Sample invocation."
  (artifacts-embedded)
  "Sample map representing a JAR."
  {:artifact {:g "org.apache.httpcomponents",
              :a "httpclient",
              :v "4.5.6"},
   :count 18,
   :size 0.76714,
   :total-size 13.808520000000001})

(comment "How many total artifacts does DDF embed?"
  (->> (artifacts-embedded)
       (map :count)
       (reduce +)))

(comment "What are DDF's most embedded artifacts?"
  (->> (artifacts-embedded)
       (sort-by :count >)
       (take 10)))

(comment "How much of DDF's size comes from embedding?"
  (->> (artifacts-embedded)
       (map :total-size)
       (reduce +)))

(comment "Which artifacts most impact DDF's total embed cost?"
  (->> (artifacts-embedded)
       (sort-by :total-size >)
       (take 10)))