(ns algorithms.bubble-sort)

(defn bubble-step [coll is-sorted less?]
  (if (second coll)
    (let [[x1 x2 & xs] coll
          is-less-than (less? x1 x2)
          smaller (if is-less-than x1 x2)
          larger (if is-less-than x2 x1)
          is-changed (or is-sorted (not is-less-than))
          [sorted is-sorted] (bubble-step (cons larger xs) is-changed less?)]
      [(cons smaller sorted) is-sorted])
    [coll is-sorted]))

(defn bubble-sort [coll less?]
  (loop [[coll is-sorted] [coll false]]
    (if is-sorted
      coll
      (recur (bubble-step coll is-sorted less?)))))

(bubble-step [40 32 1 300] false <)
(bubble-step [1 2 3] false <)

;(bubble-sort [40 32 1 300] <)
