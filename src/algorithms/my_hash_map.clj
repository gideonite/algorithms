(ns algorithms.my_hash_map)

;; A hash map provides the following basic interface:
;; - get O(1)
;; - put O(1)
;;
;; I am going to use a hashing function to map keys to indices.
;; As a backing store, I'm going to use a vector. A hashmap, is
;; precisely these two things.
;;
;; Represent an empty location in the backing array as `[nil nil]`.

(defn nil-map
  [size]
  (into [] (repeat size nil)))

(def the-empty-map (nil-map 8))

(defn- hash-index
  [hmap k]
  (mod (hash k) (count hmap)))

(defn- dump
  [hmap k v]
  (assoc hmap (mod (hash k) (count hmap)) [k v]))

(defn find-first
  [v coll]
  (first (filter #(= v (second %)) (map-indexed vector coll))))

(find-first nil [1 2 3 nil 4])
(find-first nil [1 2 3 4 5])

(defn- lookup
  [hmap k]
  (hmap (hash-index hmap k)))

(declare hash-put)
#_(defn resize
  [hmap]
  (loop [ret (nil-map (* 2 (count hmap))) hmap hmap]
    (while hmap
      (let [[k v] (first hmap)]
        (recur (hash-put ret k v) (rest hmap))))))

(reduce (fn [v [next-k next-v]]
          (hash-put v next-k next-v)) (nil-map (* 2 (count hmap))) hmap)

(defn resize [hmap] hmap)

(defn hash-put
  [hmap k v]
  (if-let [[currkey currvalue] (lookup hmap k)]
    (if (= currkey k)
      (assoc hmap (hash-index hmap k) [k v])
      (if-let [[index nil-value] (find-first nil (drop (inc (.indexOf hmap [currkey currvalue]))
                                                       hmap))]
      (assoc hmap index [k v])
      (if-let [[index nil-value] (find-first nil hmap)]
        (assoc hmap index [k v])
        (hash-put (resize hmap) k v))))
    (assoc hmap (hash-index hmap k) [k v])))

;; TODO overwrite the default print method to print non nil key-value pairs.
