(ns algorithms.priority-map)

(defn stack
  "a stack is a {:head :rest}"
  [& xs]
  (let [x1 (first xs)
        xs (rest xs)]
    (if (not x1)
      {:head nil :rest nil}
      {:head x1 :rest xs})))

(defn push-stack
  [s item]
  {:head item
   :rest (cons (:head s) (:rest s))})

(defn pop-stack
  [s]
  (let [r (:rest s)]
    (list (:head s)
          {:head (first r)
           :rest (rest r)})))

(defn empty-stack?
  [s]
  (nil? (:head s)))

(defn queue
  "returns an unbalanced queue (i.e. [stack nil])"
  [& items]
  [(apply stack items) (stack)])

(defn balance-queue
  "flip the stack if reverse is empty, otherwise do nothing."
  [q]
  (let [[f r] q]
    (if (empty-stack? r)
      [(stack)
       (apply stack (reverse (cons (:head f) (:rest f))))]
      [f r])))

(defn pop-queue
  [q]
  (let [[f r] (balance-queue q)]
    (list (:head r) )
    ))

(comment
  (stack 1 2 3)
  (stack)

  (push-stack (stack) 2)
  (push-stack (stack 1 2 3) 1)

  (pop-stack (stack 1 2 3))
  (pop-stack (push-stack (stack 1) 2))

  (queue 1 2 3)
  (balance-queue (queue 1 2 3))
  (balance-queue (balance-queue (queue 1 2 3))) ;nilpotent

  (pop-queue (queue 1 2 3))

  )
