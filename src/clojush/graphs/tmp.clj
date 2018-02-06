(ns clojush.graphs.tmp)


(require 'plumbing.fnk.schema)
(require 'plumbing.fnk.pfnk)
(require '[plumbing.graph :as g])
(require 'rhizome.viz)
(require 'clojush.graphs.config)
(require 'clojush.graphs.generation)
(require 'clojush.graphs.init)
(require 'clojush.graphs.individual)

(defn graph-edges [g]
  (for [[k node] g
        parent (keys (plumbing.fnk.pfnk/input-schema node))
        :when (keyword? parent)]
    [parent k]))

(defn save-graph [g_ name]
  (let [edges (graph-edges g_)
        es (reduce (fn [m [parent child]] (assoc m parent (conj (or (parent m) #{}) child ))) {} edges)]
    (spit name
      (rhizome.viz/graph->svg
       (set (mapcat set (conj (vals es) (keys es))))
       es
       :node->descriptor (fn [n] {
                                  :label n
                                  :style (if (contains? (set (plumbing.fnk.pfnk/input-schema-keys g_)) n) "dashed" "solid")})))))


(defn -main []
  (save-graph clojush.graphs.config/graph  "config.svg")
  (save-graph clojush.graphs.generation/graph  "generation.svg")
  (save-graph clojush.graphs.init/graph  "init.svg")
  (save-graph clojush.graphs.individual/graph  "individual.svg"))
