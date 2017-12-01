(ns kovasir.open.rt)

(defn forced [x]
  (if (delay? x)
    (recur (force x))
    x))

(defn ^::lazy if* [test then else]
  (if (forced test)
    (forced then)
    (forced else)))
