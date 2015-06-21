(ns kovasir.cfg)

;; Inspiration: "A Graph-Based Higher-Order Intermediate Representation"

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



(defmulti anf->val :op)

(def ^:dynamic *g*)
(def ^:dynamic *env*)
(def ^:dynamic *k*)

(defn val->call [x]
  (if (symbol? x)
    (list x)
    (list 'const x)))

(defn block->val [block]
  (assert (seq block))
  (binding [*env* *env*]
    (last (for [[sym expr] block]
            (let [x (anf->val expr)
                  id (gensym "let__")
                  n (Node. id [] (val->call x))]
              (set! *g* (add-node *g* n))
              (set! *env* (assoc *env* sym id))
              id)))))

(defn anf->cfg [root]
  (binding [*g* null
            *k* 'ret
            *env* {}]
    (block->val root)
    *g*))

(defmethod anf->val :const [{:keys [value]}]
  value)

(defmethod anf->val :ref [{:keys [name]}]
  (*env* name name))

(defmethod anf->val :if [{:keys [test then else]}]
  (let [b (*env* test)
        t (block->val then)
        f (block->val else)
        id (gensym "if__")
        n (Node. id [] (list 'branch b t f))]
    (set! *g* (add-node *g* n))
    id))

;(defmethod anf->val :do [{:keys [stmts]}]
;  (assert false "TODO")) ;XXX

(defmethod anf->val :fn [{:keys [params block]}]
  (let [syms (mapv #(gensym (str % "__")) params)
        _ (set! *env* (into *env* (map vector params syms)))
        _ (prn *env*)
        id (gensym "fn__")
        n (Node. id syms (-> block block->val val->call))]
    (set! *g* (add-node *g* n))
    id))

(defmethod anf->val :call [{:keys [f args]}]
  ;XXX if symbol is known, emit it as a cfg call. If free, emit as invoke.
  (let [id (gensym (str f "__"))
        fsym (*env* f)
        n (if (*env* fsym)
            (let [asyms (mapv #(*env* % %) args)]
              (prn "!!" fsym)
              (Node. id [] (list* id asyms)))
            (let [asyms (mapv #(*env* % %) (concat [f] args [*k*]))]
              (Node. id [] (list* 'invoke asyms))))]
    (set! *g* (add-node *g* n))
    id))


(comment

(-> '[fac [n ret] (cmp 1 2 3)]
  edn->node
  node->edn
  )

;; Maybe correct factorial encoding?
(def fac '[[fac [n ret] (cmp)]
           [cmp [] (invoke <= n 0 k1)]
           [k1 [x1] (branch x1 then else)]
           [then [] (ret 1)]
           [else [] (head 2 1)]
           [head [i r] (invoke <= i n k2)]
           [k2 [x2] (branch x2 body next)]
           [body [] (invoke + i 1 k3)]
           [k3 [x3] (invoke * i r k4)]
           [k4 [x4] (head x3 x4)]
           [next [] (ret r)]])

(-> fac
    edn->cfg
    cfg->edn
    fipp.edn/pprint
    )


(require '[kovasir.parse :refer [parse]])
(require '[kovasir.anf :refer [anf-block]])
(defn party [x]
  (-> x parse anf-block anf->cfg cfg->edn fipp.edn/pprint))

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
