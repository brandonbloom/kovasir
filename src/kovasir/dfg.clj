(ns kovasir.dfg)

(comment

(party '(fn [n]
          (loop [r 1 i 2]
            (if (<= i n)
              (recur (* r i) (inc i))
              r))))

;; Associative input, associative output dataflow.
{fac {:in [n]
      :out [ret]
      :flow {ret (rec 1 2)}}
 rec {:in [r i]
      :out [z]
      :flow {b (<= i n)
             c (choose b t f)
             z  (c)}}
 t {:in []
    :out [x]
    :flow {ri (* r i)
           j (inc i)
           x (rec ri j)}}
 f {:in []
    :out [y]
    :flow {y r}}}

;; Sequential arguments and single-output dataflow.
'[(fac n)   []                              (rec 1 2)
  (rec r i) [b (<= i n), c (choose b t f)]  (c)
  (t)       [ri (* r i), j (inc i)]         (rec ri j)
  (f)       []                              r]

)
