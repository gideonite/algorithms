(ns algorithms.core
  (:use clojure.data.priority-map)
  (:require [clojure.math.numeric-tower :as math]
            ))

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

(defn get-viable-neighbors [maze coor]
  (->> (get-neighbors maze coor)
    (filter #(second %))            ;; nil (falsey) value when you are off the board
    (filter #(not= 1 (second %))))) ;; 1s are blocked

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (math/abs (+ (- x1 x2) (- y1 y2))))

(def not-in (complement contains?))

(defn a-star [start end maze heuristic]
  (let [closed-set #{}
        open-set #{start}
        came-from {}
        g-score {}
        f-score {}
        f-queue (priority-map start (heuristic start end))]
    (assoc g-score start 0)
    (assoc f-score start (+ (g-score start) (heuristic start end)))

    (while (seq open-set)
      (let [current (peek f-queue)]
        (if (= current end) :horrah)

        (disj open-set current)
        (conj closed-set current)

        (for [neighbor (get-viable-neighbors maze current)
              ;; assuming that the heuristic function *is* the distance metric
              tentative-gscore [(+ (g-score current) (heuristic current neighbor))]
              tentative-fscore [(+ tentative-gscore (heuristic current end))]]
          (if-not (and (contains? closed-set neighbor)
                   (>= tentative-fscore (f-score neighbor)))
            (if (or (not-in open-set neighbor)
                    (< tentative-fscore (f-score neighbor)))
              (do
                (assoc came-from neighbor current)
                (assoc g-score neighbor tentative-gscore)
                (assoc f-score neighbor tentative-fscore)
                (conj open-set neighbor))))))))
  "fail")
