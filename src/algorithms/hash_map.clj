(ns algorithms.hash_map)

;; A hash map provides the following basic interface:
;; - get O(1)
;; - put O(1)

;; I am going to use a hashing function to map keys to indices.
;; As a backing store, I'm going to use a vector. A hashmap, is
;; precisely these two things.

;; Represent an empty location in the backing array as `[nil nil]`.

(def the-empty-map (into [] (repeat 8 [nil nil])))

(defn- lookup
  [hmap k]
  (hmap (mod (hash k) (count hmap))))

(defn- dump
  [hmap k v]
  (assoc hmap (mod (hash k) (count hmap)) [k v]))

(defn hash-put
  "Open addressing means that to resolve collisions you
  iterate through the backing store until you find an empty spot."
  [hmap k v]
  (let [[curr-key curr-value] (lookup hmap k)]
    (if (= curr-key k)
      (dump hmap k v)
      ;; seek an empty spot
      )
    ))

(def my-hash-map the-empty-map)
;; TODO overwrite the default print method to print non nil key-value pairs.
