(ns kovasir.staged
  (:refer-clojure :exclude [eval compile]))

(def ^:dynamic *env* nil)

(def std-env {'+ + '* *})

(defmulti eval-head first)

(defn lookup! [sym]
  (if-let [[_ x] (find *env* sym)]
    x
    (assert false (str "Undefined: " sym))))

(defn eval [x]
  (binding [*env* (or *env* std-env)]
    (cond

      (= x ()) x

      (seq? x)
      (eval-head x)

      (symbol? x)
      (lookup! x)

      :else x)))

(defn bind! [sym value]
  (set! *env* (assoc *env* sym value)))

(defmethod eval-head 'let*
  [[_ bindings & body]]
  (doseq [[sym init] (partition 2 bindings)]
    (bind! sym (eval init)))
  (eval (list* 'do body)))

(defmethod eval-head 'do
  [[_ & body]]
  (reduce (fn [_ x] (eval x))
          nil
          body))

(defmethod eval-head :default
  [[head & body]]
  (let [f (lookup! head)
        xs (mapv eval body)]
    (apply f xs)))


(comment

  (-> '
    (let* [x 3 y 10] (* x y))
    eval
    fipp.edn/pprint
    )

)
