(ns kovasir.codegen)

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

