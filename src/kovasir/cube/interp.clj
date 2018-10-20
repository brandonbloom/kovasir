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

(defn spawn* [vm env instrs]
  (let [[vm pid] (nextid vm)
         proc {:pid pid
               :instrs instrs
               :env env
               :park nil}
         vm (-> vm
                (assoc-in [:procs pid] proc)
                (update :queue conj pid))]
    [vm pid]))

(defn spawn [vm env instrs]
  (first (spawn* vm env instrs)))

(defn bind [vm pid keyvals]
  (update-in vm [:procs pid :env] into keyvals))

(defn call [vm name & args]
  (let [[params & instrs] (get-in vm [:defs name])
        [vm pid] (spawn* vm {} instrs)]
    (bind vm pid (map vector params args))))

(defmulti evaluate-call
  (fn [vm pid form]
    (first form)))

(defn inert? [x]
  (or (number? x)
      (string? x)
      (boolean? x)))

(defn lookup [vm pid name]
  (get-in vm [:procs pid :env name]))

(defn evaluate [vm pid x]
  (cond
    (inert? x) x
    (symbol? x) (lookup vm pid x)
    (seq? x) (evaluate-call vm pid x)
    :else (fail "can't evaluate" {:form x})))

(defmethod evaluate-call '<
  [vm pid [_ a b]]
  (< (evaluate vm pid a) (evaluate vm pid b)))

(defmethod evaluate-call '+
  [vm pid [_ a b]]
  (+ (evaluate vm pid a) (evaluate vm pid b)))

(defmethod evaluate-call 'str
  [vm pid [_ a b]]
  (str (evaluate vm pid a) (evaluate vm pid b)))

(defmulti -execute
  (fn [vm pid instr]
    (first instr)))

(defn execute [vm pid instr]
  ;(prn 'execute pid instr)
  (-execute vm pid instr))

(defmethod -execute 'tnew
  [vm pid [_ & names]]
  (reduce (fn [vm name]
            (let [[vm cid] (nextid vm)
                  chan {:cid cid
                        :writes {}
                        :readers #{}}]
              (-> vm
                  (update :chans assoc cid chan)
                  (bind pid {name cid}))))
          vm, names))

(defn get-env [vm pid]
  (get-in vm [:procs pid :env]))

(defmethod -execute 'par
  [vm pid [_ & branches]]
  (let [env (get-env vm pid)]
    (reduce (fn [vm instrs]
              (spawn vm env instrs))
            vm branches)))

(defmethod -execute 'call
  [vm pid [_ f & args]]
  (apply call vm f (map #(evaluate vm pid %) args)))

(defmethod -execute 'if
  [vm pid [_ test then else]]
  ;;TODO: tailcall become instead of spawn?
  (spawn vm
         (get-env vm pid)
         (if (evaluate vm pid test)
           then
           else)))

(defn park [vm pid reason]
  ;(prn 'park pid reason)
  (assoc-in vm [:procs pid :park] reason))

(defn wake [vm pid]
  ;(prn 'wake pid)
  (-> vm
      (assoc-in [:procs pid :park] nil)
      (update :queue conj pid)))

(defmethod -execute 'send
  [vm pid [_ chan init]]
  (let [cid (evaluate vm pid chan)
        value (evaluate vm pid init)
        vm (assoc-in vm [:chans cid :writes pid] value)
        vm (park vm pid [:send cid value])
        reader (first (get-in vm [:chans cid :readers]))]
    (cond-> vm
      reader (wake reader))))

(defn prefix [xs x]
  (cons x xs))

(defmethod -execute 'read
  [vm pid [_ chan name :as instr]]
  (let [cid (evaluate vm pid chan)]
    (if-let [[writer value] (first (get-in vm [:chans cid :writes]))]
      (-> vm
          (wake writer)
          (update-in [:chans cid :writes] dissoc writer)
          (update-in [:chans cid :readers] disj pid)
          (bind pid {name value}))
      (-> vm
          (park pid [:read cid])
          (update-in [:procs pid :instrs] prefix instr)
          (update-in [:chans cid :readers] conj pid)))))

(def printed (atom [])) ;XXX

(defmethod -execute 'println
  [vm pid [_ s]]
  (swap! printed conj (evaluate vm pid s))
  (println (evaluate vm pid s))
  vm)

(defn step [vm]
  (let [pid (-> vm :queue first)
        vm (update vm :queue disj pid)
        {:keys [instrs] :as proc} (get-in vm [:procs pid])]
    (if-let [[instr & instrs] (seq instrs)]
      (let [vm (assoc-in vm [:procs pid :instrs] instrs)
            vm (execute vm pid instr)
            park (get-in vm [:procs pid :park])]
        (if park
          vm
          (update vm :queue conj pid)))
      (update vm :procs dissoc pid))))

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
                (println (str "Lock taken by " n))
                (call feedback fbk))
      launch ([n max lock]
              (if (< n max)
                [(par [(call launch (+ n 1) max lock)]
                      [(call cs n lock)])]
                []))
      main ([]
            (tnew lock fbk)
            (par [(call launch 0 100 lock)]
                 [(call feedback fbk)]
                 [(send lock fbk)]))})

  (reset! printed [])

  (-> (inject defs)
      (call 'main)
      run
      fipp.edn/pprint)

  (= (set @printed)
     (set (map #(str "Lock taken by " %) (range 100))))

)
