(ns kovasir.peg
  "Program Expression Graphs"
  (:require [kovasir.util :refer :all]))

(def ^:dynamic *prog*)
(def fuel (atom 0))

(defmulti -interp
  (fn [id x i]
    (first x)))

(defn interp [id i]
  (when (zero? (swap! fuel dec))
    (throw (Exception. "out of fuel!")))
  (if (symbol? id)
    (let [v (-interp id (*prog* id) i)]
      (pprint ['interp id (*prog* id) i '=> v])
      v)
    id))

(defn monotonize [loop-id val-id i]
  (some #(interp val-id (assoc i loop-id %)) (range (i loop-id))))

(defmethod -interp 'eval [id [_ loop idx] i]
  (when-let [v (interp idx i)]
    (monotonize id loop (assoc idx id (interp idx i)))))

(defmethod -interp 'pass [id [_ cond] i]
  (loop [n 0]
    (if (monotonize id cond (assoc i id n))
      n
      (recur (inc n)))))

(defmethod -interp 'theta [id [_ base loop] i]
  (if (zero? (i id 0))
    (interp base i)
    (interp loop (assoc i (dec (i id))))))

(defmethod -interp 'call [id [_ f & args] i]
  (pprint [id f args i])
  (apply (resolve f)
         (map #(interp % i) args)))

(defmethod -interp 'const [id [_ c] i]
  c)

(defn party [prog entry]
  (reset! fuel 500)
  (binding [*prog* prog]
    (interp entry {})))

; for (i := 0; i < 29; i++) {
;   i++;
; }
; return i;

(def fig-3-1-a
  '{e (eval t p)
    t (theta 0 s)
    s (call + 2 t)
    p (pass g)
    g (call >= t 29)})

(comment

  (party fig-3-1-a 'e)

)

; int factorial( int N )
; {
;   int product = 1;
;   for ( int j=1; j<=N; j++ )
;     product *= j;
;   return product;
; }
