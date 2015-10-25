(ns kovasir.dfg
  (:refer-clojure :exclude [eval]))

(defn make-env [prog]
  (reduce (fn [env [label params expr]]
            (assoc env label {:params params :expr expr}))
          {}
          prog))

(defn literal? [x]
  (number? x))

(defn lookup [x env]
  (if-let [ns (namespace x)]
    (get-in env [(symbol ns) :bindings (-> x name symbol)])
    (env x)))

(defn code? [x]
  (map? x)) ;XXX

(defn nullary? [x]
  (and (code? x) (-> x :params empty?)))

(defmulti eval-seq (fn [xs env] (first xs)))

(defn eval [x env]
  (let [y (cond
            (literal? x) x
            (symbol? x) (lookup x env)
            (seq? x) (eval-seq x env)
            :else (throw (ex-info "can't eval" {:expr x})))]
    (if (nullary? y)
      (recur (:expr y) env)
      y)))

(defmethod eval-seq 'if [[_ b t f] env]
  (if (eval b env)
    (eval t env)
    (eval f env)))

(defmethod eval-seq 'invoke [[_ f & args] env]
  (apply (resolve f) (map #(eval % env) args)))

(defmethod eval-seq :default [[f & xs :as fxs] env]
  (let [[{:keys [params expr]} & args] (map #(eval % env) fxs)]
    (eval expr (assoc-in env [f :bindings] (zipmap params args)))))

(defn interp [prog expr]
  (eval expr (make-env prog)))

(def fac '[[fac [n]   (rec 1 2)]
           [rec [r i] (if b bod rec/r)]
           [b   []    (invoke <= rec/i fac/n)]
           [ri  []    (invoke * rec/r rec/i)]
           [j   []    (invoke inc rec/i)]
           [bod []    (rec ri j)]])

(comment

  (-> fac
      make-env
      fipp.edn/pprint
      )

  (fipp.edn/pprint
    (interp fac '(fac 5))
    )

)

;;; destination passing style?
