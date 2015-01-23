(ns kovasir.core)

(defn head [x]
  (if (seq x)
    (first x)
    (class x)))

(defmulti anf head)

(defmethod anf 'bind
  [[_ sym init :as bind]]
  (let [cmds (anf init)
    (if (= cmds [init])
      [bind]
      (let [sym (gensym)]
        (concat cmds [(list 'bind 


  [(list 'bind sym init)])

(defmethod anf 'let
  [[_ bindings & body]]
  (concat ['(begin)]
          (->> bindings
               (partition 2)
               (mapcat (fn [[sym init]]
                         (anf (list 'bind sym init)))))
          (mapcat anf body)
          ['(end)]))

(defmethod anf clojure.lang.Symbol
  [x]
  [x])

(defmethod anf :default
  [xs]
  (let [[proc args]
        (reduce (fn [[proc args] x]
                  (if (symbol? x)
                    [proc (conj args x)]
                    (let [sym (gensym)]
                      [(anf (list 'bind sym x)) (conj args sym)])))
                [[] []]
                xs)]
    (concat proc [(seq args)])))

(comment

  (->
      '(let [x 1
             y 2]
         (+ x (* x 2) (* x 2) (inc (inc y))))
      anf
      fipp.edn/pprint
      )

)
