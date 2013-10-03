(ns algorithms.core
  (:require [clojure.math.numeric-tower :as math]))

(def maze [[0 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0]])

;; coordinates (coor) are tuples [x y] representing positions in the maze
;;
;; mazes are matrices, represented as vectors of vectors, where the values are
;; either 1 or 0.  0 means you can go there, 1 means you can't.

(defn get-coordinate-value [maze [x y]]
  "maze [x y] -> (value at [x y]) or nil"
  (if
    (or (< x 0) (< y 0)) nil
    (nth (nth maze y) x)))

(get-coordinate-value maze [1 1])

(defn get-neighbors [maze [x y]]
  "maze x-coordinate y-coordinate -> {coordinates (value at tuple)}"
  (let [left   [(dec x) y]
        right  [(inc x) y]
        up     [x (inc y)]
        down   [x (dec y)]]
    {left   (get-coordinate-value maze left)
     right  (get-coordinate-value maze right)
     up     (get-coordinate-value maze up)
     down   (get-coordinate-value maze down)}))

(get-neighbors maze [0 0])

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (math/abs (+ (- x1 x2) (- y1 y2))))

(defn closest [h neighbors-map]

  )

(defn a-star [maze [start1 start2] [end1 end2]]
  (when-let [:g 0
             :neighbors (get-neighbors [start1 start2])
             ])
  (closest (get-neighbors [start1 start2]))
  )
