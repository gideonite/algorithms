(ns algorithms.core
  (:use clojure.data.priority-map)    ;; TODO why not :require?
  (:require [clojure.math.numeric-tower :as math]))

(defn get-coordinate-value
  "maze [x y] -> (value at [x y]) or nil"
  [maze [x y]]
  (let [h (count maze)
        w (count (first maze))]
    (when (and (<= 0 x (dec w)) (<= 0 y (dec h)))
      (nth (nth maze y) x))))

(defn get-neighbors
  "maze x-coordinate y-coordinate -> {coordinates (value at tuple)}"
  [maze [x y]]
  (let [left   [(dec x) y]
        right  [(inc x) y]
        up     [x (inc y)]
        down   [x (dec y)]]
    {left   (get-coordinate-value maze left)
     right  (get-coordinate-value maze right)
     up     (get-coordinate-value maze up)
     down   (get-coordinate-value maze down)}))

(defn get-viable-neighbors
  "returns the neighbors of the coordinate that are traversable
  (i.e. not blocked)"
  [maze coor]
  (->> (get-neighbors maze coor)
    (filter #(not= 1 (second %)))   ;; 1s are blocked
    (map first)))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (math/abs (+ (- x1 x2) (- y1 y2))))

(defn construct-path [came-from end]
  (take-while identity                  ; stopping condition, (identity nil) -> nil -> falsy
              (conj                     ; include end in the path
                (iterate came-from end) ; apply came-from like an exponent, (came-from ... (came-from (came-from end)))
                end)))

;; Inspiration: http://clj-me.cgrand.net/2010/09/04/a-in-clojure/
;; Thanks Christophe Grand!

(defn a-star [start end maze metric]
  (let [heuristic #(metric % end)]
    (loop [closed-set #{}
           came-from  {}
           f-queue    (priority-map start (heuristic start))]
      (when-let [[current f-current] (peek f-queue)]
        (if (= current end)
          (construct-path came-from end)
          (let [neighbors (for [n (remove closed-set (get-viable-neighbors maze current))
                                ;; filter the viable neighbors down to the ones that are:
                                ;;    * not in the closed set
                                ;;    * found to have a lower cost (g-score)
                                ;;      than previously found, or infinity if it
                                ;;      is a new node
                                :let [tentative (+ (metric current n) (heuristic n))
                                      f-neighbor (f-queue n Double/POSITIVE_INFINITY)]
                                :when (< tentative f-neighbor)]
                            [n tentative])]
            (recur (conj closed-set current)
                   (into came-from (for [[n] neighbors] [n current]))
                   (into (pop f-queue) neighbors))))))))

(defn place-X
  "places the char \\X at the coordinates [x y]"
  [maze [x y]]
  (assoc maze y (assoc (maze y) x \X)))

(defn visualize
  "print the path overlayed on the maze to stdout"
  [maze path]
  (print " ")
  (apply println
         (interleave
           (map #(clojure.string/join " " %)
                (reduce #(place-X %1 %2) (place-X maze (first path)) path))
           (repeat "\n"))))

(defn run-a-star
  "runs the algorithm and visualizes the output to stdout"
  [start end maze metric]
  (visualize maze (a-star start end maze manhattan-distance)))

(run-a-star [0 0] [3 3]
            [[0 0 0 0]
             [0 1 0 0]
             [0 0 1 0]
             [0 0 1 0]]
            manhattan-distance)

;; --- Trash ---

(def not-in (complement contains?))

(defn a-star-pseudo [start end maze heuristic]
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
