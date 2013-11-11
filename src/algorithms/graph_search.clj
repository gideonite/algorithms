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
  "Finds a path (any path) from the start to the end in the graph.  Returns nil
  if there is no path."
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
  [start end graph]
  (loop [path-queue [[start]]
         visited #{}
         graph graph]

    (if (empty? path-queue)
      nil
      (let [curr-path (first path-queue)
            curr-node (first curr-path)
            adjs (filter (comp not visited) (graph curr-node))]

        (if (= end curr-node)
          (reverse curr-path)
          (if (empty? adjs)
            (recur (subvec path-queue 1)
                   visited
                   graph)
            (recur (apply conj (subvec path-queue 1)
                          (map #(cons % curr-path) adjs))
                   (conj visited curr-node)
                   graph)))))))

;;  1----2----3    11
;;  |    |         |
;;  4    5----6----10
;;  |    |    |
;;  7    8    9----12

(comment
  (find-shortest-path 1 10 test-graph1)
  (find-shortest-path 1 2 test-graph1)
  (find-shortest-path 1 -1 test-graph1)
  )
