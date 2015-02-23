(ns kovasir.schedule
  (:require [clojure.set :as set]))

; To implement this algorithm, we need to determine the set G of nodes that
; are forced inside and may not be part of the top level. We start with the
; block result R and a graph E that has all unnecessary nodes removed (DCE
; already performed):

; E = R union syms(E map findDefinition)
;;TODO refactor with dead code elimination etc
(defn reachable [nodes roots depf]
  (loop [out #{}
         in (seq roots)]
    (if in
      (let [[n & in] in]
        (if (contains? out n)
          (recur out in)
          (recur (conj out n)
                 (into in (depf (nodes n))))))
      out)))

; We then need a way to find all uses of a given symbol s, up to but not
; including the node where the symbol is bound:
; U(s) = {s} union { g in E | syms(findDefinition(g)) intersect U(s) <>􏰀
;                    && s notIn boundSyms(findDefinition(g))) }
(defn usages [nodes id]
  (into (if-let [n (nodes id)]
          {id n}
          {})
        (for [[k {:as v :keys [deps bound]}] nodes
              :when (and (contains? deps id)
                         (not (contains? bound id)))]
          [k v])))

; We collect all bound symbols and their dependencies. These cannot live on
; the current level, they are forced inside:
; B = boundSyms (E map findDefinition)
(defn bound [nodes]
  (set (mapcat :bound (vals nodes))))

; G = union (B map U) // must inside
(defn nested [nodes]
  (apply set/union (map #(usages nodes %) (bound nodes))))




(defn ambient
  "Dependencies which are neither not nor cold."
  [node]
  (set/difference (:deps node) (:hot node) (:cold node)))

; Code Motion Algorithm:
;
; Compute the set L of top level statements for the current block, from a set
; of available statements E, a set of forced-inside statements G subset E and
; a block result R .
;
; 1. Start with L containing the known
; top level statements, initially the (available) block result R ∩ E .
;
; 2. Add to L all nodes reachable from L via normal links (neither hot nor
; cold) through E - G (not forced inside).
;
; 3. For each hot ref from L to a statement in E−L, follow any links through G,
; i.e. the nodes that are forced inside, if there are any. The first
; non-forced-inside nodes (the “hot fringe”) become top level as well (add to
; L).
;
; 4. Continue with 2 until a fixpoint is reached.
(defn top-level [nodes root]
  ;(fipp.edn/pprint nodes)
  (loop [top #{root}]
    ;(fipp.edn/pprint (reachable nodes top))
    ;(fipp.edn/pprint (bound nodes))
    ;(fipp.edn/pprint (nested nodes))
    ;(fipp.edn/pprint (usages nodes 1))
    (let [inside (nested nodes)
          outside (apply dissoc nodes inside)
          top* (reachable outside top ambient)
          hot (for [t top*
                    hot (-> t nodes :hot)
                    :when (not (inside hot))]
                hot)
          ;_ (println "<<------------")
          ;_ (fipp.edn/pprint {:top* top* :hot hot :inside inside})
          ;_ (println "------------>>")
          top* (into top* hot)
          ]
      (if (= top top*)
        top
        (recur top*)))))


; Topological Sort
; L ← Empty list that will contain the sorted nodes
; while there are unmarked nodes do
;     select an unmarked node n
;     visit(n)
; function visit(node n)
;     if n has a temporary mark then stop (not a DAG)
;     if n is not marked (i.e. has not been visited yet) then
;         mark n temporarily
;         for each node m with an edge from n to m do
;             visit(m)
;         mark n permanently
;         unmark n temporarily
;         add n to head of L
(defn toposort [nodes root]
  ;TODO: ensure nodes/deps are ordered to force unique schedule
  (let [v (atom [])
        nodes (atom nodes)]
    (letfn [(visit [id]
              (when-let [n (@nodes id)]
                (swap! nodes dissoc id)
                (doseq [dep (:deps n)]
                  (visit dep))
                (swap! v conj [id n])))]
      (while (seq @nodes)
        (visit (-> @nodes keys first))))
    @v))


;TODO return top-level block + unscheduled nodes, let codegen drive recursion
(defn schedule [{:keys [nodes root] :as xx}]
  ;(fipp.edn/pprint xx)
  (let [top-ids (top-level nodes root)
        ;_ (fipp.edn/pprint top-ids)
        top-nodes (select-keys nodes top-ids)
        bottom-nodes (into {} (for [[id n :as kvp] nodes
                                    :when (not (contains? top-ids id))]
                                kvp))
        block (toposort top-nodes root)]
    {:block block
     :unscheduled bottom-nodes}))
