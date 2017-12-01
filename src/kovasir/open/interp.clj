(ns kovasir.open.interp
  (:require [kovasir.util :refer :all]
            [kovasir.open.rt :as rt]))

(def ^:dynamic *graph*)
(def ^:dynamic *fuel*)

(defmulti -interp
  (fn [node env]
    (:op node)))

(defn interp [id env]
  (delay
    (when (zero? (change! *fuel* dec))
      (fail "out of fuel!"))
    (let [x (or (*graph* id)
                (fail "no such id" {:id id :graph *graph*}))]
      ;(pprint (list 'interping id x env))
      (-interp x env))))

(defmethod -interp :eval [{:keys [expr subst]} env]
  (interp expr (into env (for [[k v] subst]
                           [k (interp v env)]))))

(defn make-lazy [f]
  {:pre [(var? f)]}
  (if (-> f meta ::rt/lazy)
    f
    (fn [& args] (apply f (map rt/forced args)))))

(defmethod -interp :apply [{:keys [f args]} env]
  (let [x (map #(interp % env) args)
        y (apply (make-lazy f) x)]
    ;(pprint [(list* (list 'make-lazy f) x) '=> y])
    y))

(defmethod -interp :param [{:keys [name]} env]
  (if-let [[_ v] (find env name)]
    v
    (fail "undefined" {:name name :env env})))

(defmethod -interp :const [{:keys [value]} env]
  value)

(comment

  (require 'kovasir.parse)
  (require '[kovasir.open.graph :as g])
  (defn party [x]
    (let [{:keys [graph root]} (-> x kovasir.parse/parse g/program)]
      (binding [*graph* graph
                *fuel* 100]
        (pprint (rt/forced (interp root {}))))))

  (party '((fn [n]
             (loop [dst [], i 0]
               (if (< i n)
                 (recur (conj dst i) (inc i))
                 dst)))
           5))

  )
