(ns algorithms.naive_bayes
  (:require [clojure.string :only [split] :as s]))

(def house-votes "resources/house-votes-84.data")
(def all-data (map #(s/split % #",") (s/split (slurp house-votes) #"\n")))
(def training-data (take 300 all-data))
(def test-data (drop 300 all-data))

(defn column
  [n data]
  (map #(nth % n) data))

(defn zip
  [l1 l2]
  (map vector l1 l2))

(defn distribution
  "Lists of values -> {value -> frequency of value}.
  What's cool is that a value can be anything, including a seq."
  [col]
  (let [l (partition-by identity (sort col))
        n (count col)]
    (into {}
          (map #(vector (first %1) %2)
               l (map #(/ (count %) n) l)))))

(comment
  (distribution (column 10 training-data))
  (distribution (zip (column 1 training-data) (column 2 training-data)))
  (assert (= 1 (reduce + (map second (distribution (column 10 training-data)))))))

(defn a|b
  "Conditional probability of `a` given `b`. Basically finds the distribution
  of the zipped list of `a` and `b` and then divides it by the distribution of
  `b`."
  [a b]
  (let [b-dist (distribution b)]
    (into {}
          (map (fn [[[a b] k]] [[a b]
                                (/ k (b-dist b))])
               (distribution (zip a b))))))

(comment
  (a|b (column 0 training-data) (column 2 training-data))
  ((a|b (column 0 training-data) (column 2 training-data)) ["democrat" "?"]))

(defn filter-by-a
  [a a|b]
  (into {}
        (map (fn [[[fi c] p]] [c p])
             (filter (fn [[[fi c] p]] (= fi a)) a|b))))

(comment
  (filter-by-a "n" (a|b (column 2 training-data) (column 0 training-data))))

(defn naive-bayes   ;; TODO refactor so that you get a naive bayes model for a particular training set.
  [datum training-data]
  (let [Z (for [i (range 1 (count datum))]
            (filter-by-a
              (nth datum i)
              (a|b (column i data) (column 0 data))))
        P-of-C (distribution (column 0 data))
        foo (cons P-of-C Z)
        p-democrat (reduce * (filter (comp not nil?) (map #(get % "democrat") foo)))
        p-republican (reduce * (filter (comp not nil?) (map #(get % "republican") foo)))]
    (if (< p-democrat p-republican)
      "republican"
      "democrat")))

(comment
  (map #(naive-bayes % training-data) all-data)
  (naive-bayes (nth all-training-data 0) data)

  (let [predictions (map #(naive-bayes % training-data) test-data)
        truth (column 0 test-data)]
    (count
      (filter #(not= (first %) (second %))
              (zip predictions truth)))))
