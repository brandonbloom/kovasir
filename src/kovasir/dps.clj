(ns kovasir.dps
  "Explorations of destination passing style.")

((fn [n]
  (loop [r 1 i 2]
    (if (<= i n)
      (recur (* r i) (inc i))
      r)))
 5)


(def env (atom {}))

(defn fac-dps [n ret]
  (swap! env assoc
         :r 1
         :i 2)
  (loop []
    (if (<= (:i @env) n)
      (do
        (swap! env assoc
               :r (* (:r @env) (:i @env))
               :i (inc (:i @env)))
        (recur))
      (swap! env assoc ret (:r @env))))
  nil)

(do
  (reset! env {})
  (fac-dps 5 :ret)
  (:ret @env))

