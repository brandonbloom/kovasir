(ns kovasir.open
  (:require [kovasir.util :refer :all]))

(def ^:dynamic *graph*)
(def ^:dynamic *defs*)
(def ^:dynamic *counter*)
(def ^:dynamic *env* {})
(def ^:dynamic *prompt* nil)
(def ^:dynamic *params* {})

(defn genint []
  (change! *counter* inc))

(defn node! [id x]
  (change! *graph* assoc id x)
  id)

(defn bind [id x]
  (or (*defs* x)
      (do (change! *defs* assoc x id)
          (node! id x))))

(def empty-graph {})

(defmulti -dfg
  (fn [id ast]
    (:op ast)))

(defn dfg
  ([ast]
   (-dfg (genint) ast))
  ([id ast]
   (-dfg id ast)))

(defn block [ast]
  (dfg ast))

(defn program [ast]
  (binding [*graph* empty-graph
            *defs* {}
            *counter* 0]
    (let [root (block ast)]
      {:graph *graph*
       :root root})))

(defmethod -dfg :const [id ast]
  (bind id ast))

(defn make-subst [fid args]
  (into {} (map (fn [param arg]
                  [param (dfg arg)])
                (get *params* fid)
                args)))

(defmethod -dfg :call [id {:keys [f args]}]
  (let [f (dfg f)
        args (mapv dfg args)]
    (bind id
      (cond
        (var? f) {:op :apply :f f :args args}
        (number? f) {:op :eval
                     :expr f
                     :subst (make-subst f args)}
        :else (fail "cannot call" {:f f})))))

(defmethod -dfg :ref [_ {:keys [name]}]
  (or (*env* name)
      (resolve name)))

(defmethod -dfg :fn [id {:keys [params expr]}]
  (let [ids (mapv (fn [name]
                    (node! (genint) {:op :param :name name}))
                  params)]
    (binding [*env* (into *env* (map vector params ids))
              *prompt* id
              *params* (assoc *params* id params)]
      (dfg id expr))))

(defmethod -dfg :loop [id {:keys [bindings expr]}]
  (dfg id {:op :call
           :f {:op :fn :params (mapv first bindings) :expr expr}
           :args (mapv second bindings)}))

(defn if* [test then else]
  (if test then else))

(defmethod -dfg :if [id {:keys [test then else]}]
  (let [test (dfg test)
        then (block then)
        else (block else)]
    (bind id {:op :apply
              :f (resolve `if*)
              :args [test then else]})))

(defmethod -dfg :recur [id {:keys [args]}]
  (when-not *prompt*
    (fail "nowhere to recur to"))
  (bind id {:op :eval
            :f *prompt*
            :subst (make-subst *prompt* args)}))

(comment

  (require 'kovasir.parse)
  (defn party [x]
    (-> x kovasir.parse/parse program fipp.edn/pprint))

  (party '100)
  (party '(+ 100 100))
  (party '(fn [x] x))
  (party '(fn [x] (inc x)))
  (party '(if 1 2 3))
  (party '(loop [] nil))
  (party '(loop [] (recur)))

  (party '(fn [n]
            (loop [dst [], i 0]
              (if (< i n)
                (recur (conj dst i) (inc i))
                dst))))

  )
