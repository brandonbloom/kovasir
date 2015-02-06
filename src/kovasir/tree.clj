(ns kovasir.tree)

(def ^:dynamic *trace*)
(def ^:dynamic *defs* nil)

(defn run [f]
  (f))

(defn accumulate [f]
  (binding [*defs* (or *defs* (atom {}))
            *trace* (atom [])]
    (run f)
    @*trace*))

(defn sym [x]
  (fn []
    [:ref x]))

(defn bind!
  ([x]
   (bind! (gensym) x))
  ([sym x]
   [:ref (if-let [[_ y] (find @*defs* x)]
           y
           (do
             (swap! *trace* conj [:bind sym x])
             (swap! *defs* assoc x sym)
             sym))]))

(defn const [x]
  (fn []
    (bind! [:const x])))

(defn app [f & args]
  (fn []
    (bind! (vec (list* :call f (map run args))))))

(defn do2 [stmt expr]
  (fn []
    (stmt)
    (bind! (expr))))

(defn ifx [test then else]
  (fn []
    (let [b (bind! (test))]
      (bind! [:if b (accumulate then) (accumulate else)]))))

(defmulti emit first)

(def ^:dynamic *ret* nil)

(defn emit-block [xs]
  (binding [*ret* nil]
    (doseq [x xs]
      (emit x))))

(defn return []
  (print "return" *ret*)
  (println ";"))

(defmethod emit :const
  [[_ x]]
  (pr x))

(defmethod emit :ref
  [[_ sym]]
  (pr sym))

(defmethod emit :call
  [[_ f & args]]
  (print f)
  (print "(")
  (doseq [thunk (interpose #(print ", ") (map #(fn [] (emit %)) args))]
    (thunk))
  (print ")"))

(defmethod emit :bind
  [[_ sym x]]
  (set! *ret* sym)
  (print "var" sym "= ")
  (emit x)
  (println ";"))

(defmethod emit :if
  [[_ test then else]]
  (println "(function() {")
  (print "if(")
  (emit test)
  (println ") {")
  (emit-block then)
  (return)
  (println "} else { ")
  (emit-block else)
  (return)
  (print "})()"))

(defmethod emit :default
  [_]
  (print "."))

(comment

  (->

    ;(app 'add
    ;     (app 'mul (const 3) (const 10))
    ;     (app 'mul (const 2) (const 5))
    ;     (app 'mul (const 2) (const 5))
    ;     (sym 'foo))

    ;(do2 (app 'blah (const "foo"))
    ;     (app 'add (const 2) (const 2)))

    (ifx (const 1) (const 2) (const 3))

    accumulate
    emit-block
    ;fipp.edn/pprint

    )

)
