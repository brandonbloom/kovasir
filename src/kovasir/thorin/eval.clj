(ns kovasir.thorin.eval
  (:refer-clojure :exclude [eval]))

(defn make-env [prog]
  (reduce (fn [env [label params expr]]
            (assoc env label {:params params :bindings {} :expr expr}))
          {}
          prog))

(def boolean? (partial instance? Boolean))

(defn literal? [x]
  (or (nil? x)
      (number? x)
      (sequential? x) ;XXX treat seqs and vectors as quoted
      (map? x) ;XXX self-evaluate closures
      (fn? x)
      (boolean? x)))

(defn closure? [x]
  (:env x)) ;XXX abuse the map type

(defn eval [x env]
  (cond
    (literal? x) x
    (namespace x) (get-in env [(-> x namespace symbol)
                               :bindings
                               (-> x name symbol)])
    :else {:name x :env env}))

(def ^:dynamic *trace* false)

(ns-unmap *ns* 'step)
(defmulti step (fn [{:keys [env expr]}]
                 (when *trace*
                   (println)
                   (fipp.edn/pprint env {:width 160})
                   (prn expr)
                   (println))
                 (first expr)))

(defmethod step 'invoke [{[_ f k & args] :expr, :keys [env]}]
  {:env env
   :expr (let [f (if (namespace f) ;XXX
                   (eval f env)
                   (resolve f))
               args (map #(eval % env) args)]
           (list k (apply f args)))})

(defmethod step 'if [{[_ b t f] :expr, :keys [env]}]
  {:env env
   :expr (list (if (eval b env) t f))})

(defmethod step 'return [{[_ x] :expr, :keys [env]}]
  [(eval x env)])

(defmethod step :default [{[head & args] :expr, :keys [env]}]
  (let [args (map #(eval % env) args)
        {:keys [name env], :as f} (eval head env)
        {:keys [params expr]} (env name)]
    (assert f)
    {:env (update-in env [name :bindings] into
                     (map vector params args))
     :expr expr}))

(defn interp [prog expr]
  (->> {:env (make-env prog) :expr expr}
       (iterate step)
       next
       (drop-while map?)
       ffirst
       ))

(def fac '[[fac [n] (rec 1 2)]
           [rec [r i] (invoke <= k1 rec/i fac/n)]
           [k1 [x1] (if k1/x1 body exit)]
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

;; This version of each invokes host functions.
(def each '[[each [f xs] (invoke seq k1 each/xs)]
            [k1 [s] (if k1/s body exit)]
            [body [] (invoke first k2 each/xs)]
            [k2 [fst] (invoke next k3 each/xs)]
            [k3 [nxt] (invoke each/f k4 k2/fst)]
            [k4 [_] (each each/f k3/nxt)]
            [exit [] (return nil)]])

(comment

  (interp each (list 'each prn [1 2 3]))

)

(def both '[
  ;; Factorial
  [fac [n k] (frec 1 2)]
  [frec [r i] (invoke <= fk1 frec/i fac/n)]
  [fk1 [x1] (if fk1/x1 fbody fexit)]
  [fbody [] (invoke * fk2 frec/i frec/r)]
  [fk2 [x2] (invoke inc fk3 frec/i)]
  [fk3 [x3] (frec fk2/x2 fk3/x3)]
  [fexit [] (fac/k frec/r)]
  ;; Each (this version calls source functions)
  [each [f xs] (invoke seq ek1 each/xs)]
  [ek1 [s] (if ek1/s ebody eexit)]
  [ebody [] (invoke first ek2 each/xs)]
  [ek2 [fst] (invoke next ek3 each/xs)]
  [ek3 [nxt] (each/f ek2/fst ek4)]
  [ek4 [_] (each each/f ek3/nxt)]
  [eexit [] (return nil)]
  ;; Print Each Factorial
  [pf [x k] (fac pf/x pk)]
  [pk [y] (invoke prn pf/k pk/y)]
  [main [] (each pf [1 3 5])]
])

(comment

  (-> both make-env fipp.edn/pprint)

  (interp both '(main))

)
