(ns kovasir.thorin)

(defn make-env [prog]
  (reduce (fn [env [label params expr]]
            (assoc env label {:params params :bindings {} :expr expr}))
          {}
          prog))

(def ^:dynamic *trace* false)

(ns-unmap *ns* 'interp*)
(defmulti interp* (fn [{:keys [env expr]}]
                    (when *trace*
                      (prn expr))
                    (first expr)))

(def boolean? (partial instance? Boolean))

(defn literal? [x]
  (or (nil? x)
      (number? x)
      (sequential? x) ;XXX
      (fn? x)
      (boolean? x)))

(defn lookup [env x]
  (cond
    (literal? x) x
    (namespace x) (get-in env [(-> x namespace symbol)
                               :bindings
                               (-> x name symbol)])
    :else (env x)))

(defmethod interp* 'invoke [{[_ f k & args] :expr, :keys [env]}]
  {:env env
   :expr (let [f (if (namespace f) ;XXX
                   (lookup env f)
                   (resolve f))
               args (map #(lookup env %) args)]
           (list k (apply f args)))})

(defmethod interp* 'branch [{[_ b t f] :expr, :keys [env]}]
  {:env env
   :expr (list (if (lookup env b) t f))})

(defmethod interp* 'return [{[_ x] :expr, :keys [env]}]
  [(lookup env x)])

(defmethod interp* :default [{[head & args] :expr, :keys [env]}]
  (let [{:keys [params expr] :as f} (env head)]
    (assert f)
    {:env (update-in env [head :bindings] into
                     (map vector params (map #(lookup env %) args)))
     :expr expr}))

(defn interp [prog expr]
  (->> {:env (make-env prog) :expr expr}
       (iterate interp*)
       next
       (drop-while map?)
       ffirst
       ))

(def fac '[[fac [n] (rec 1 2)]
           [rec [r i] (invoke <= k1 rec/i fac/n)]
           [k1 [x1] (branch k1/x1 body exit)]
           [body [] (invoke * k2 rec/i rec/r)]
           [k2 [x2] (invoke inc k3 rec/i)]
           [k3 [x3] (rec k2/x2 k3/x3)]
           [exit [] (return rec/r)]])

(comment

  (-> fac
      make-env
      fipp.edn/pprint
      )

  (interp fac '(fac 5)) ;=> 120

)

(def each '[[each [f xs] (invoke seq k1 each/xs)]
            [k1 [s] (branch k1/s body exit)]
            [body [] (invoke first k2 each/xs)]
            [k2 [fst] (invoke next k3 each/xs)]
            [k3 [nxt] (invoke each/f k4 k2/fst)]
            [k4 [_] (each each/f k3/nxt)]
            [exit [] (return nil)]])

(comment

  (interp each (list 'each prn [1 2 3]))

)
