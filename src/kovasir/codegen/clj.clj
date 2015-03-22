(ns kovasir.codegen.clj
  (:require [kovasir.schedule :refer [schedule]]))

(defn ->sym [id]
  (if (number? id)
    (symbol (str "$" id))
    id))

; (ns-unmap *ns* 'gen)
(defmulti gen (fn [nodes {:keys [op] :as xx}] op))

(defn gen-block
  ([nodes root]
   (gen-block {:nodes nodes :root root}))
  ([graph]
   (let [{:keys [block unscheduled]} (schedule graph)
         ret (some->> (peek block) second (gen (:nodes graph)))
         ;_ (prn "!!" block unscheduled)
         stmts (pop block)]
     (if (seq stmts)
       `(let [~@(mapcat (fn [[id stmt]]
                          [(->sym id) (gen unscheduled stmt)])
                        stmts)]
          ~ret)
       ret))))

(defmethod gen :const
  [nodes {:keys [value]}]
  value)

(defmethod gen :call
  [nodes {:keys [f args]}]
  (list* (->sym f) (map ->sym args)))

(defmethod gen :recur
  [nodes {:keys [args]}]
  (list* 'recur (map ->sym args)))

;;TODO An earlier step should eliminate the hot/cold nodes.

(defmethod gen :cold
  [nodes {:keys [expr]}]
  (gen nodes (nodes expr)))

(defmethod gen :hot
  [nodes {:keys [expr]}]
  (gen nodes (nodes expr)))

(defmethod gen :fn
  [nodes {:keys [params expr]}]
  `(fn ~(mapv ->sym params) ~(gen-block nodes expr)))

(defmethod gen :if
  [nodes {:keys [test then else]}]
  (list 'if (->sym test) (gen-block nodes then) (gen-block nodes else)))

(defmethod gen :loop
  [nodes {:keys [bindings expr]}]
  (list 'loop (vec (apply concat (for [[param init] bindings]
                                   [(->sym param) (->sym init)])))
        (gen-block nodes expr)))


(comment

  (require 'kovasir.parse)
  (require 'kovasir.graph)
  (require 'kovasir.codegen.clj)
  (defn prepare [x]
    (-> x kovasir.parse/parse kovasir.graph/program))
  (defn party [x]
    (-> x prepare
        kovasir.codegen.clj/gen-block fipp.clojure/pprint
        ;schedule fipp.edn/pprint
        ))

  (party '(let [x "a"
                y "b"
                z "c"]
            (str x z)))

  (party '(if 100
            (f (+ 123 123) y)
            (f x (+ 123 123))))

  (party '(let [a (+ 123 456)]
            (if 100
              (f a y)
              (f x a))))

  (party '(let [f (fn [x] (+ x y (+ 500 1000)))
                a (+ 200 400)]
            (f a)))

  (party '(loop [x 0]
            (recur (+ (inc x) (* 2 2)))))

  (-> '
      ;(fn [x] (+ x y))
      ;(fn [x] x)
      (if x y z)
      ;(loop [x 0]
      ;  (recur (+ (inc x) (* 2 2))))
      kovasir.parse/parse
      kovasir.graph/program
      kovasir.schedule/schedule
      ;:nodes (kovasir.schedule/usages 1)
      ;:nodes kovasir.schedule/bound
      ;:nodes kovasir.schedule/nested
      ;:nodes (kovasir.schedule/ambient 3)
      ;(get-in [:nodes '$2]
      fipp.edn/pprint)

)
