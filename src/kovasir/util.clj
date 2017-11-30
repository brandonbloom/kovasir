(ns kovasir.util
  (:require [fipp.edn]
            [fipp.repl]))

(defn fail
  ([msg] (fail msg {}))
  ([msg data]
   (throw (ex-info msg data))))

(defmacro change! [var f & args]
  `(set! ~var (~f ~var ~@args)))

(defn pprint [x]
  (fipp.edn/pprint x {:width 160, :print-level 20 :print-length 10}))

(def pst fipp.repl/pst)
