(ns kovasir.graph)

;;TODO consider renaming to dataflow + creating a separate explicit control
;; flow graph before going to codegen. The control flow graph has basic blocks.

(def ^:dynamic *graph*)
(def ^:dynamic *defs*)
(def ^:dynamic *ctx*) ;TODO delete/replace me?
(def ^:dynamic *eff* #{}) ;TODO conservative now. Handle known pure fns.
(def ^:dynamic *names* {})

(def empty-graph {:nodes {}
                  :root nil
                  :counter 0})

(def empty-context {:root nil})


(defmacro update! [v f & args]
  (list 'set! v (list* f v args)))

(def conjs (fnil conj #{}))



(defmulti dfg :op)


(defn block [ast]
  (binding [*ctx* empty-context]
    (let [ret (dfg ast)]
      (update! *graph* assoc :root ret) ;TODO calculate another way?
      ret)))

(defn program [ast]
  (binding [*graph* empty-graph
            *defs* {}]
    (block ast)
    *graph*))

(defn fresh! []
  (let [i (-> *graph* :counter inc)]
    (update! *graph* assoc :counter i)
    i))

(defn bind! [x]
  (if-let [[_ y] (find *defs* x)]
    y
    (let [id (fresh!)]
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
        node (assoc node :deps (into *eff* (cons (:f node) (:args node))))]
    (bind! node)))

(defmethod dfg :do
  [{:keys [stmts]}]
  (assert (next stmts))
  (reduce (fn [name stmt]
            (binding [*eff* (conj *eff* name)]
              (dfg stmt)))
          (dfg (first stmts))
          (next stmts)))

(defn cold! [id]
  (bind! {:op :cold :expr id :deps #{id}}))

(defn hot! [id]
  (bind! {:op :hot :expr id :deps #{id}}))

(defmethod dfg :if
  [{:keys [test then else]}]
  (let [b (dfg test)
        t (cold! (block then))
        e (cold! (block else))]
    (bind! {:op :if :test b :then t :else e :deps #{b t e}})))

(defmethod dfg :let
  [{:keys [name init expr]}]
  (let [bound (dfg init)]
    ;;TODO boundSyms ???
    (binding [*names* (assoc *names* name bound)
              #_#_ ;XXX Disables conservative effect tracking
              *eff* (conj *eff* bound)]
      (dfg expr))))


(defmethod dfg :loop
  [{:keys [bindings expr]}]
  (let [syms (mapv first bindings)
        params (vec (repeatedly (count syms) fresh!))
        inits (mapv (comp bind! second) bindings)
        names (into {} (map vector syms params))
        ;; Loop bounds a dynamic "recur target" which recur depends on.
        target (fresh!)
        names (assoc names :recur target)
        bindings (mapv vector params inits)]
    (binding [*names* (merge *names* names)]
      (let [expr (hot! (block expr))]
        (bind! {:op :loop :bindings bindings :expr expr
                :deps (conj (set inits) expr)
                :bound (conj (set params) target)})))))

(defmethod dfg :recur
  [{:keys [args]}]
  (let [node {:op :recur :args (mapv dfg args)}
        node (assoc node :deps (into *eff* (:args node)))
        node (update-in node [:deps] conj (*names* :recur))]
    (bind! node)))

(defmethod dfg :fn
  [{:keys [params expr]}]
  (let [ids (vec (repeatedly (count params) fresh!))]
    (binding [*eff* #{}
              *names* (apply assoc *names* (interleave params ids))]
      (bind! {:op :fn :params params
              :bound (set ids) :expr (block expr)}))))



(comment

  (require 'kovasir.parse)
  (defn party [x]
    (-> x kovasir.parse/parse program fipp.edn/pprint))

  (party '100)
  (party '(+ (* 300 1000) (* 200 500) (* 200 500) foo))
  (party '(do (f "foo") (+ 2 2)))
  (party '(do (f "foo") (do (+ 200 200) (+ 500 600))))
  (party '(if 100 200 300))
  (party '(let [x "a"] x))
  (party '(let [x "a" y "b"] (str x y)))
  (party '(fn [x] (str x y)))
  (party '(loop [x 100 y 200] 300))
  (party '(loop [] (recur)))

)
