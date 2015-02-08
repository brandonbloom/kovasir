(ns kovasir.graph)

(def ^:dynamic *graph*)
(def ^:dynamic *defs*)
(def ^:dynamic *ctx*) ;TODO delete/replace me?
(def ^:dynamic *eff* #{}) ;TODO conservative now. Handle known pure fns.
(def ^:dynamic *names* {})

(def empty-graph {:nodes {}
                  :root nil})

(def empty-context {:root nil})


(defmacro update! [v f & args]
  (list 'set! v (list* f v args)))

(def conjs (fnil conj #{}))



(defmulti dfg :op)


(defn block [ast]
  (binding [*ctx* empty-context]
    (let [ret (dfg ast)]
      (update! *graph* assoc :root ret)
      ret)))

(defn program [ast]
  (binding [*graph* empty-graph
            *defs* {}]
    (block ast)
    *graph*))

(defn bind! [x]
  (if-let [[_ y] (find *defs* x)]
    y
    (let [id (-> *graph* :nodes count inc)]
      (update! *defs* assoc x id)
      (update! *graph* assoc-in [:nodes id] x)
      (update! *ctx* assoc :root id)
      id)))


(defmethod dfg :ref
  [{:keys [name]}]
  (if-let [[_ bound] (find *names* name)]
    bound
    name))

(defmethod dfg :const
  [{:keys [value]}]
  (bind! {:op :const :value value}))

(defmethod dfg :call
  [{:keys [f args]}]
  (let [node {:op :call :f (dfg f) :args (mapv dfg args)}
        node (assoc node :deps (into *eff* (:args node)))]
    (bind! node)))

(defmethod dfg :do
  [{:keys [stmts]}]
  (assert (next stmts))
  (reduce (fn [name stmt]
            (binding [*eff* (conj *eff* name)]
              (dfg stmt)))
          (dfg (first stmts))
          (next stmts)))

(defmethod dfg :if
  [{:keys [test then else]}]
  (let [b (bind! (dfg test))]
    ;;TODO deps
    (bind! {:op :if
            :test b
            :then (block then)
            :else (block else)})))

(defmethod dfg :let
  [{:keys [name init expr]}]
  (let [bound (dfg init)]
    (binding [*names* (assoc *names* name bound)
              *eff* (conj *eff* bound)]
      (dfg expr))))

;TODO loop/recur



(comment

  (require 'kovasir.parse)
  (defn party [x]
    (-> x kovasir.parse/parse program fipp.edn/pprint))

  (party '1)
  (party '(+ (* 3 10) (* 2 5) (* 2 5) foo))
  (party '(do (f "foo") (+ 2 2)))
  (party '(do (f "foo") (do (+ 2 2) (+ 5 6))))
  (party '(if 1 2 3))
  (party '(let [x "a"] x))
  (party '(let [x "a" y "b"] (str x y)))

)
