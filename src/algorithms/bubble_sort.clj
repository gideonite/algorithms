(ns algorithms.bubble-sort)

;; inspired and largely taken whole sheet and cloth from
;; http://rosettacode.org/wiki/Sorting_algorithms/Bubble_sort#Clojure

(defn bubble-step
  [coll was-changed less?]
  (if (not (second coll))     ;; count > 1 without counting the list
    [coll (not was-changed)]
    (let [[x1 x2 & xs] coll
          first-is-smaller (less? x1 x2)
          is-changed (or was-changed (not first-is-smaller))
          [smaller larger] (if first-is-smaller [x1 x2] [x2 x1])
          [result is-sorted] (bubble-step (cons larger xs) is-changed less?)]
      [(cons smaller result) is-sorted])))

(defn bubble-sort
  [coll less?]
  (let [step #(bubble-step %1 %2 less?)]
    (loop [[coll is-sorted] [coll false]]
      (if is-sorted
        coll
        (recur (step coll is-sorted))))))

(comment
  (bubble-step [40 32 1 300] false <)
  (bubble-sort [40 32 1 300] <)

  (bubble-sort [123123 39 5 4 40 12 32 2 1 300] <))
