(ns algorithms.heap)

;; In a heap an index is synonymous to a node.

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
  for the given predicate.
  "
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

(comment
  (heapify [1 2 3 4 5] 1 max)
  (heapify [1 2 3 4 5] 1 min)
  )

(defprotocol IHeap
  (is-empty? [this])
  (insert [this])
  (find-min [this])
  (delete-min [this]))

(defrecord Heap [array])

(def h (->Heap [1 2 3]))
