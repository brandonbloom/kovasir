(ns kovasir.dfg)

(comment

(party '(fn [n]
          (if (<= n 1)
            1
            (loop [r 1 i 2]
              (if (<= i n)
                (recur (* r i) (inc i))
                r)))))

{fac {:in [n]
      :out [ret]
      :flow {b1  (<= n 1)
             c1  (choose b1 t1 f1)
             ret (c1)}}
 t1 {:in []
     :out [x1]
     :flow {x1 1}}
 f1 {:in []
     :out [y1]
     :flow {y1 (rec 1 2)}}
 rec {:in [r i]
      :out [z]
      :flow {b2 (<= i n)
             c2 (choose b2 t2 f2)
             z  (c2)}}
 t2 {:in []
     :out [x2]
     :flow {ri (* r i)
            ii (inc i)
            x2 (rec ri ii)}}
 f2 {:in []
     :out [y2]
     :flow [y2 r]}}

)
