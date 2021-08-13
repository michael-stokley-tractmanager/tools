(ns mmastokley.update-deps
  (:require [rewrite-clj.zip :as z]))

(def ^:dynamic filename-root "/home/michael/tract-manager/projects/")

(def projects
  #{:workflow
    :docusign
    :doc-services
    :customer-details
    :contract
    :communication
    :authenticate})

(defn read-project-clj
  [project]
  (let [filename (str filename-root (name project) "/project.clj")]
    (slurp filename)))

(defn read-deps-edn
  [project]
  (let [filename (str filename-root (name project) "/deps.edn")]
    (slurp filename)))

(defn write-project-clj
  [content project]
  (let [filename (str filename-root (name project) "/project.clj")]
    (spit filename content)))

(defn write-project-deps-edn
  [content project]
  (let [filename (str filename-root (name project) "/deps.edn")]
    (spit filename content)))

(defn update-project-clj
  [dependency-system-file-string dependency version]
  (let [root           (z/of-string dependency-system-file-string)
        sought?        #(and (-> % z/sexpr (= dependency))
                             (-> % z/right z/sexpr string?))
        update-version #(-> % z/right (z/replace version) z/left)]

    (loop [loc root]
      (cond
        (z/end? loc)
        (z/root-string loc)

        (sought? loc)
        (-> loc update-version z/next recur)

        :else
        (-> loc z/next recur)))))

(defn update-deps-edn
  [dependency-system-file-string dependency version]
  (let [root (z/of-string dependency-system-file-string)
        sought?        #(and (-> % z/sexpr (= dependency))
                             (-> % z/right z/sexpr map?)
                             (-> % z/right z/sexpr (contains? :mvn/version)))
        update-version #(-> %
                            z/right
                            (z/edit (fn [m] (assoc m :mvn/version version)))
                            z/left)]
    (loop [loc root]
      (cond
        (z/end? loc)
        (z/root-string loc)

        (sought? loc)
        (-> loc update-version z/next recur)

        :else
        (-> loc z/next recur)))))

(defmulti update-dependency
  (fn [{:keys [dependency version]} dependency-system project]
    dependency-system))

(defmethod update-dependency
  :project-clj
  [{:keys [dependency version]} dependency-system project]
  (-> project
      read-project-clj
      (update-project-clj dependency version)
      (write-project-clj project)))

(defmethod update-dependency
  :deps-edn
  [{:keys [dependency version]} dependency-system project]
  (-> project
      read-deps-edn
      (update-deps-edn dependency version)
      (write-project-deps-edn project)))

(comment

  (update-dependency {:dependency 'tractsoft/commons
                      :version    "2"}
                     :project-clj
                     :workflow)

  (doseq [project           #{:workflow :contract}
          dependency-system #{:project-clj :deps-edn}]
    (update-dependency {:dependency 'tractsoft/commons
                        :version    "1"}
                       dependency-system
                       project))

  )

