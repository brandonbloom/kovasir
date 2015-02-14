(ns kovasir.codegen.clj)

(defn ->sym [id]
  (if (number? id)
    (symbol (str "$" id))
    id))

(defmulti gen :op)

(defn block [instructions]
  (let [ret (some-> (peek instructions) second gen)
        stmts (pop instructions)]
    (if (seq stmts)
      `(let [~@(mapcat (fn [[id stmt]]
                         [(->sym id) (gen stmt)])
                       stmts)]
         ~ret)
      ret)))

(defmethod gen :const
  [{:keys [value]}]
  value)

(defmethod gen :call
  [{:keys [f args]}]
  (list* (->sym f) (map ->sym args)))

(defmethod gen :fn
  [{:keys [params expr]}]
  `(fn ~(mapv ->sym params) ~:XXX-block)) ;TODO expr

(defmethod gen :if
  [{:keys [test then else]}]
  (list 'if (->sym test) (block then) (block else)))
