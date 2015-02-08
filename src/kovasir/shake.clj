(ns kovasir.shake)

;XXX This works (worked?), but conservative effect deps makes it hard to tell.

(defn live [{:keys [nodes root]}]
  (let [out (atom {})
        f (fn rec [id]
            (when-not (contains? @out id)
              (let [node (nodes id)]
                (swap! out assoc id node)
                (doseq [dep (:deps node)]
                  (rec dep)))))]
    (f root)
    {:nodes @out :root root}))

(comment

  (require 'kovasir.parse)
  (require 'kovasir.graph)
  (defn party [x]
    (-> x kovasir.parse/parse kovasir.graph/program live fipp.edn/pprint))

  (party '(let [x "a"
                y "b"
                z "c"]
            (str x z)))


)
