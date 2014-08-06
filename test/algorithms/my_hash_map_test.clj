(ns algorithms.my_hash_map-test
  (:require [clojure.test :refer :all]
            [algorithms.my_hash_map :refer :all]))

(deftest my-put
  (testing "Put without collisions."
    (assert (= [nil nil ["hello" "world"] nil nil nil nil nil]
             (hash-put the-empty-map "hello" "world"))))
  (testing "Overwrite when keys are equal."
    (let [hello-world (hash-put the-empty-map "hello" "world")
          hello-you (hash-put hello-world "hello" "you")]
      (assert (= [nil nil ["hello" "you"] nil nil nil nil nil]
                 hello-you))))
  (testing "Does not overwrite when keys are not equal."
    (let [hello-world (hash-put the-empty-map "hello" "world")
          hello-b (hash-put hello-world "b" "world")]
      (assert (some #{["hello" "world"]} hello-b))
      (assert (some #{["b" "world"]} hello-b)))))
