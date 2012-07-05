(ns learn-algorithm.mini-cut
  (:require [clojure.string :as string]))

;; graph is represnted like
;; grapch {:nodes {0 {:nodes [0] :edges [1 2]
;;                 2 {:nodes [2] :edges [0 3]}
;;                 :g0 {:nodes [0 2] :edges [1 3]}
;;                 :g1 {:nodes [1 3] :edges [0 2]}}
;;         :nodes-in-group {0 :g0 1 :g0 2 :g1} }

(defn contract-node [graph node1 node2]
  (let [node1-nodes (get-in graph [:nodes node1 :nodes])
        node2-nodes (get-in graph [:nodes node2 :nodes])
        node1-edges (get-in graph [:nodes node1 :edges])
        node2-edges (get-in graph [:nodes node2 :edges])
        new-nodes (set (concat node1-nodes node2-nodes))]
    {:nodes new-nodes
     :edges (filter (complement new-nodes)
                    (concat node1-edges node2-edges))}))

(defn update-in-group [graph node new-group]
  (reduce #(assoc-in %1 [:nodes-in-group %2] new-group)
          graph
          (get-in graph [:nodes node :nodes])))

(defn contract [graph]
  (let [nodes (:nodes graph)
        node-count (count nodes)]
    (if (> node-count 2)
      (let [n1 (rand-int node-count)
            node (nth (keys nodes) n1)
            edges (get-in graph [:nodes node :edges])
            n2 (rand-int (count edges))
            next-node (nth edges n2)
            next-node (or (get (:nodes-in-group graph)
                               next-node)
                          next-node)
            new-node (contract-node graph node next-node)]
        (-> graph
             (update-in-group next-node node)
             (assoc-in [:nodes node] new-node)
             (update-in [:nodes] dissoc next-node)))
      graph)))

(def graph {:nodes {0 {:nodes [0] :edges [1 4]} 1 {:nodes [1] :edges [2 0]} 2 {:nodes [2] :edges [3 1]} 3 {:nodes [3] :edges [4 2]} 4 {:nodes [4] :edges [0 3]}  }})

(defn mini-cut-once [graph]
  (loop [g graph]
    (let [nodes (:nodes g)]
      (cond
       (<= (count nodes) 2)
       (count (:edges (second (first nodes))))
       :else (recur (contract g))))))

(defn read-graph [f]
  (with-open [rdr (clojure.java.io/reader f)]
    (let [coll (->> (line-seq rdr)
                    (map #(map (fn [s] (Integer/parseInt s))
                               (string/split % #"\s+"))))]
      {:nodes (reduce #(assoc %1 (first %2)
                              {:nodes [(first %2)]
                               :edges (next %2)}) {} coll)})))

(defn caculate-mini-cut [f]
  (let [g (read-graph f)
        n (count (:nodes g))
        t (* n (dec n))
        t 100]
    (loop [c t m 10000000000]
      (if (<= c 0)
        m
        (recur (dec c) (min (mini-cut-once g) m))))))
