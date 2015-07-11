(ns kovasir.anf)

(defmulti anf :op)

(def ^:dynamic *block*)
(def ^:dynamic *loop*)

(defn anf-block [x]
  (binding [*block* []]
    (anf x)
    *block*))

(defn ref? [{:keys [op] :as ast}]
  (= op :ref))

(defn bind!
  ([x]
   (bind! (gensym) x))
  ([sym x]
    (set! *block* (conj *block* [sym x]))
    sym))

(defmethod anf :default [ast]
  (fipp.edn/pprint [:XXX ast]))

(defmethod anf :ref [ast]
  (bind! ast))

(defmethod anf :const [ast]
  (bind! ast))

(defmethod anf :if [ast]
  (-> ast
    (update :test anf)
    (update :then anf-block)
    (update :else anf-block)
    bind!))

(defmethod anf :call [ast]
  (-> ast
      (update :f anf)
      (update :args #(mapv anf %))
      bind!))

(defmethod anf :fn [{:keys [params expr]}]
  (binding [*loop* (gensym)]
    (bind! {:op :fn
            :name *loop* ;XXX what if already named?
            :params params
            :block (anf-block expr)})))

(defmethod anf :loop [{:keys [bindings expr]}]
  (anf {:op :call
        :f {:op :fn
            :params (mapv first bindings)
            :expr expr}
        :args (mapv second bindings)}))

(defmethod anf :recur [ast]
  (anf {:op :call
        :f {:op :ref :name *loop*}
        :args (:args ast)}))

;XXX let
;XXX do
;XXX letfn


(comment

(require 'kovasir.parse)
(defn party [x]
  (-> x kovasir.parse/parse anf-block fipp.edn/pprint))

(party '5)
(party '(if 1 2 3))
(party '(f x))
(party '((f 1) (g 2)))
(party '(f (g (h (x 1) (y 2)))))
(party '(loop [] 1))
(party '(loop [] (recur)))
(party '(loop [x 1] (recur 2)))
(party '(if (f x) y z))
(party '(fn [x] (f x)))

(party '(fn [n]
          (if (<= n 1)
            1
            (loop [r 1 i 2]
              (if (<= i n)
                (recur (* r i) (inc i))
                r)))))

(letfn [(even? [x] (or (zero? x) (not (odd? (dec x)))))
        (odd? [x] (and (not= x 0) (even? (dec x))))]
  ...)

)
