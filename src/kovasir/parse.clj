(ns kovasir.parse)

(defmulti parse-seq first)

(defn parse [x]
  (cond

    (or (number? x)
        (string? x)
        (keyword? x)
        (nil? x)
        (instance? Boolean x)
        (= () x))
    {:op :const :value x}

    (symbol? x)
    {:op :ref :name x}

    (seq? x)
    (parse-seq x)

    :else
    {:op :unknown :value x}

  ))

(defmethod parse-seq :default
  [[f & xs]]
  {:op :call :f (parse f) :args (mapv parse xs)})

(defmethod parse-seq 'let
  [[_ bindings & body]]
  (if-let [[name init & more] (seq bindings)]
    {:op :let
     :name name
     :init (parse init)
     :expr (parse-seq (list* 'let (vec more) body))}
    (parse (list* 'do body))))

(defmethod parse-seq 'do
  [[_ & body]]
  (cond
    (next body) {:op :do :stmts (mapv parse body)}
    (seq body) (parse (first body))
    :else {:op :const :value nil}))

(defmethod parse-seq 'if
  [[_ test then else]]
  {:op :if
   :test (parse test)
   :then (parse then)
   :else (parse else)})

;TODO loop/recur

(comment

  (defn party [x]
    (-> x parse fipp.edn/pprint))

  (party 1)
  (party 'x)
  (party :foo)
  (party ())
  (party '((juxt inc dec) (f x) (g y)))
  (party '(do))
  (party '(do 1))
  (party '(do 1 2 3))
  (party '(let [] (+ x y)))
  (party '(let [x 1 y 2] (+ x y)))
  (party '(if 1 2 3))
  (party '(if 1 2))

)
