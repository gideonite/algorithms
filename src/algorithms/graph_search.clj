(ns algorithms.graph_search)

;;  1----2----3    11
;;  |    |         |
;;  4    5----6----10
;;  |    |    |
;;  7    8    9----12
(def test-graph1
  {1  [2 4]
   2  [1 3 5]
   3  [2]
   4  [1 7]
   5  [2 6 8]
   6  [5 9 10]
   7  [4]
   8  [5]
   9  [6 12]
   10 [6 11]
   11 [10]
   12 [9]
   })

;; 1----2----3----11
;; |    |         |
;; 4----5----6----10
;; |    |    |
;; 7    8    9----12
;; |    |
;; 13---14
(def test-graph2
  {1  [2 4]
   2  [1 3 5]
   3  [2 11]
   4  [1 5 7]
   5  [2 4 6 8]
   6  [5 9 10]
   7  [4 13]
   8  [5 14]
   9  [6 12]
   10 [6 11]
   11 [3 10]
   12 [9]})

(defn find-path
  "Finds a path (any path) from the start to the end in the graph by doing
  breadth-first search. Returns nil if there is no path."
  [start end graph]
  (loop [path [start]
         visited #{start}
         graph graph]
    (when-let [curr (last path)]
      (let [adjs (filter (comp not visited) (graph curr))]
        (if (= curr end)
          path
          (if (empty? adjs)
            (recur (pop path) (conj visited curr) graph)
            (recur (conj path (first adjs)) (conj visited curr) graph)))))))

(comment
  (find-path 1 10 test-graph1)
  (find-path 1 2 test-graph1)
  (find-path 1 -1 test-graph1))

(defn find-shortest-path
  "Finds the shortest path between the start and the end. Breadth-first search
  which halts when it finds the end. If no end is found, returns nil."
  [start end graph]
  (loop [path-queue [[start]]
         visited #{}
         graph graph]
    (when-let [curr-path (first path-queue)]
      (let [curr (first curr-path)
            adjs (filter (comp not visited) (graph curr))]
        (if (= end curr)
          (reverse curr-path)
          (let [subq (subvec path-queue 1)]
            (if (empty? adjs)
              (recur subq visited graph)
              (recur (apply conj subq (map #(cons % curr-path) adjs))
                     (conj visited curr)
                     graph))))))))

;; http://gist.io/7375570

(defprotocol IStore
  (store-rest [store])
  (store-peek [store])
  (store-conj [store v])
  (store-empty? [store]))

(defrecord StackStore [list]
  IStore
  (store-rest [store] (->StackStore (rest (:list store))))
  (store-peek [store] (peek (:list store)))
  (store-conj [store v] (->StackStore (conj (:list store) v)))
  (store-empty? [store] (empty? (:list store))))

(defn stack
  [& vs]
  (->StackStore vs))

(defn store-conj-all
  [store vs]
  (if (empty? vs)
    store
    (store-conj-all (store-conj store (first vs)) (rest vs))))

(defn print-graph
  "A graph is a map of vertices, i.e. an int to a (list of vertices). The store
  implements IStore."
  [graph store]
  (loop [vs '()
         store (store-conj store (first (keys graph)))]
    (let [curr (store-peek store)
          neighbors (filter #(and (not= % (first vs))
                                  (not= % curr))
                            (graph curr))]
      (if (store-empty? store)
        vs
        (recur (conj vs curr)
               (store-conj-all (store-rest store) (reverse neighbors)))))))

(defn -main [& args]
  (print-graph test-graph1 (stack)))

(comment
  (find-shortest-path 1 10 test-graph1)
  (find-shortest-path 1 2 test-graph1)
  (find-shortest-path 1 -1 test-graph1))
