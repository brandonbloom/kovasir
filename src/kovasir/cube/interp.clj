(ns kovasir.cube.interp
  (:use [kovasir.util]))

(defn inject [defs]
  {:defs defs
   :procs {}
   :chans {}
   :queue #{}
   :counter 0})

(defn nextid [vm]
  (let [vm (update vm :counter inc)]
    [vm (:counter vm)]))

(defn spawn* [vm instrs]
  (let [[vm pid] (nextid vm)
         proc {:pid pid
               :instrs instrs
               :env {}
               :blocked nil}
         vm (-> vm
                (assoc-in [:procs pid] proc)
                (update :queue conj pid))]
    [vm pid]))

(defn spawn [vm instrs]
  (first (spawn* vm instrs)))

(defn bind [vm pid keyvals]
  (update-in vm [:procs pid :env] into keyvals))

(defn call [vm name & args]
  (let [[params & instrs] (get-in vm [:defs name])
        [vm pid] (spawn* vm instrs)]
    (bind vm pid (map vector params args))))

(defmulti interp
  (fn [vm pid instr]
    (first instr)))

(defmethod interp 'tnew
  [vm pid [_ & names]]
  (reduce (fn [vm name]
            (let [[vm cid] (nextid vm)
                  chan {:cid cid
                        :writers #{}
                        :readers #{}}]
              (-> vm
                  (update :chans assoc cid chan)
                  (bind pid {name chan}))))
          vm, names))

(defmethod interp 'par
  [vm pid [_ & branches]]
  (reduce spawn vm branches))

(defmethod interp 'call
  [vm pid [_ f & args]]
  (apply call vm f args))

(defn lookup [vm pid name]
  (get-in vm [:procs pid :env name]))

(defn inert? [x]
  (or (number? x)
      (boolean? x)))

(defn evaluate [vm pid x]
  (cond
    (inert? x) x
    (symbol? x) (lookup vm pid x)
    :else (fail "can't evaluate" {:form x})))

(defmethod interp '<
  [vm pid [_ a b out]]
  (bind vm pid {out (< (evaluate vm pid a) (evaluate vm pid b))}))

(defmethod interp 'if
  [vm pid [_ test then else]]
  ;;TODO: tailcall become instead of spawn?
  (spawn vm (if (evaluate vm pid test)
              then
              else)))

(defmethod interp 'send
  [vm pid [_ chan value]]
  (let [chan (evaluate vm pid value)
        value (evaluate vm pid value)]
    ;XXX PICK UP WORK HERE
    )


(defn step [vm]
  (let [pid (-> vm :queue first)
        {:keys [instrs] :as proc} (get-in vm [:procs pid])
        [instr & instrs] instrs
        vm (assoc-in vm [:procs pid :instrs] instrs)
        vm (interp vm pid instr)
        blocked (get-in vm [:procs pid :blocked])
        vm (cond-> vm
             (or (nil? instrs) blocked) (update :queue disj pid)
             (nil? instrs) (update :procs dissoc pid))]
    vm))

(defn done? [vm]
  (-> vm :queue empty?))

(defn run [vm]
  (if (done? vm)
    vm
    (recur (-> vm step))))

(comment

  (def defs
    '{cs ([n lock]
          (read lock x)
          (send x n)
          (send lock x))
      feedback ([fbk]
                (read fbk n)
                (str "Lock taken by" n s)
                (println s)
                (call feedback fbk))
      launch ([n max lock]
              (< n max b)
              (if b
                [(+ n 1 n')
                 (par [(call launch n' max lock)]
                      [(call cs n lock)])]
                []))
      main ([]
            (tnew lock fbk)
            (par [(call launch 0 10 lock)]
                 [(call feedback fbk)]
                 [(send lock fbk)]))})

  (-> (inject defs)
      (call 'main)
      run
      fipp.edn/pprint)

)
