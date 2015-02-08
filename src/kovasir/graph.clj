(ns kovasir.graph)

(def ^:dynamic *graph*)
(def ^:dynamic *defs*)
(def ^:dynamic *ctx*)
(def ^:dynamic *eff* #{})

(def empty-graph {:bind {}
                  :free #{}
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

(defn bind!
  ([x]
   (bind! (gensym) x))
  ([sym x]
   (if-let [[_ y] (find *defs* x)]
     y
     (do
       (update! *defs* assoc x sym)
       (update! *graph* assoc-in [:bind sym] x)
       (update! *ctx* assoc :root sym)
       sym))))


(defmethod dfg :ref
  [{:keys [name]}]
  (when-not (contains? (:bound *graph*) name)
    (update! *graph* update-in [:free] conj name))
  name)

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



(comment

  (require 'kovasir.parse)
  (defn party [x]
    (-> x kovasir.parse/parse program fipp.edn/pprint))

  (party '1)
  (party '(+ (* 3 10) (* 2 5) (* 2 5) foo))
  (party '(do (f "foo") (+ 2 2)))
  (party '(do (f "foo") (do (+ 2 2) (+ 5 6))))
  (party '(if 1 2 3))

)
