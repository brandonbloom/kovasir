(ns kovasir.thorin.cfg)

(defrecord Node [id bound call])

(defrecord Graph [nodes])

(def null (Graph. {}))

(defn add-node [g {:keys [id] :as n}]
  (assoc-in g [:nodes id] n))


(defn value? [x]
  (or (symbol? x)
      (number? x)
      (nil? x)))

(defn edn->node [[id bound call]]
  {:pre [(symbol? id)
         (vector? bound)
         (every? symbol? bound)
         (symbol? (first call))
         (every? value? (next call))]}
  (Node. id bound call))

(defn edn->cfg [nodes]
  (->> nodes
    (map edn->node)
    (reduce add-node null)))

(defn node->edn [{:keys [id bound call]}]
  [id bound call])

(defn cfg->edn [{:keys [nodes]}]
  (mapv node->edn (vals nodes)))



(ns-unmap *ns* 'anf->cfg)
(defmulti anf->cfg (fn [[g k] ast] (prn ast) (:op ast)))

(def ^:dynamic *env*)

(defn block->cfg [ctx xs]
  (reduce (fn [[g k] [n e]]
            (add-node g [n]
            )
          ctx
          (reverse xs))))

(defn root->cfg [root]
  (binding [*env* {}]
    (let [ctx [null (gensym "ret__")]]
      (block->cfg ctx root))))

(defmethod anf->cfg :const [[g k] {:keys [value]}]
  (let [id (gensym "const__")
        g (add-node g (Node. id [] (list k value)))]
    [g id]))


(comment

(-> '[fac [n ret] (cmp 1 2 3)]
  edn->node
  node->edn
  )

;; Maybe correct factorial encoding?
(def fac '[[fac [n done] (rec 1 2)]
           [rec [r i] (invoke <= i n k1)]
           [k1 [x1] (branch x1 body exit)]
           [body [] (invoke * r i k2)]
           [k2 [x2] (invoke inc i k3)]
           [k3 [x3] (rec x2 x3)]
           [exit [] (done r)]])

(-> fac
    edn->cfg
    cfg->edn
    fipp.edn/pprint
    )


(require '[kovasir.parse :refer [parse]])
(require '[kovasir.anf :refer [anf-block]])
(defn party [x]
  (-> x parse anf-block root->cfg cfg->edn fipp.edn/pprint))

(party '1)
(party '(if 1 2 3))
(party '(if (if 1 2 3) (if 4 5 6) (if 7 8 9)))
;(party '(do 1 2 3))
(party '(fn []))
(party '(fn [x]))
(party '(f 1 2 3))
(party '(fn [x] (f x)))
(party '(fn [f] (f 1)))

(party '(fn [n]
          (if (<= n 1)
            1
            (loop [r 1 i 2]
              (if (<= i n)
                (recur (* r i) (inc i))
                r)))))

)
