(ns algorithms.heap)

;; In a heap, indices are synonymous with nodes.

(defn left
  "vector, index -> index."
  [v i]
  (let [left-index (+ 1 (* 2 i))]
    (when (nth v left-index nil)
      left-index)))

(defn right
  "vector, index -> index."
  [v i]
  (let [right-index (+ 2 (* 2 i))]
    (when (nth v right-index nil)
      right-index)))

(defn parent
  "index -> index."
  [i] (int (/ i 2)))

(defn flip
  "vector, index, index -> vector."
  [v i j]
  (assoc (assoc v i (nth v j))
    j (nth v i)))

(defn heapify
  "vector, index, (ints -> int) -> vector.
  Takes a vector and an index and heapifies that index,
  continuing up the heap until the heap property is satisfied
  for the given predicate."
  [v i max-min]
  (let [l (left v i)
        r (right v i)
        maxi (apply max-min (map #(nth v %)
                             (filter (complement nil?) [l r i])))]
    (if (= maxi (nth v i))
      v
      (if (= maxi (nth v l))
        (heapify (flip v l i) (parent i) max-min)
        (if (= maxi (nth v r))
          (heapify (flip v r i) (parent i) max-min)
          :fail)))))

  (heapify [1 2 3 4 5] 1 max)
  (heapify [1 2 3 4 5] 1 min)

  (left [1 2 3 4 5] 4)
  (right [1 2 3 4 5] 4)

(defn bubble-down
  "vector, index, (ints -> int) -> vector.
  In a word, the opposite of heapify. Instead
  of maintaining the heap property going up the
  heap parent by parent, you go down the heap
  child by child."
  [v i max-min]
  (let [l (left v i)
        r (right v i)
        maxi (apply max-min (map #(nth v %)
                             (filter (complement nil?) [l r i])))]
    (if (= maxi (nth v i))
      v
      (if (= maxi (nth v l))
        (bubble-down (flip v i l) l max-min)
        (if (= maxi (nth v r))
          (bubble-down (flip v i r) r max-min)
          :fail)))))

(defprotocol IHeap
  (is-empty? [this])
  (insert [this n])
  (find-top [this])
  (delete-top [this]))

(defrecord Heap [array max-min]
  IHeap
  (is-empty? [this] (empty? (:array this)))
  (insert [this n]
          (let [conjed (conj (:array this) n)]
            (->Heap (heapify conjed
                             (parent (dec (count conjed))) max-min) max-min)))
  (find-top [this] (first (:array this)))
  (delete-top [this]
              (->Heap
               (bubble-down
                (apply vector (conj (drop-last 1 (drop 1 (:array this)))
                                    (last (:array this))))
                0 max-min) max-min)))

(defn minheap [] (->Heap [] min))
(defn maxheap [] (->Heap [] max))
(def minh (minheap))
(is-empty? minh)

(def minh (reduce insert (minheap) [1 3 5 7 8 9]))
(def maxh (reduce insert (maxheap) [1 3 5 7 8 9]))

(delete-top minh)
(delete-top maxh)
