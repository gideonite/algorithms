(ns algorithms.priority-map)

(defn stack
  "a stack is just a cons list"
  [& xs]
  xs)

(defn push-stack
  "just does `cons`"
  [s item]
  (cons item s))

(defn pop-stack
  "stack -> stack"
  [s]
  (pop (into '() s)))

(defn peek-stack
  [s]
  "stack -> item"
  (peek s))

(defn empty-stack?
  "stack -> boolean"
  [s]
  (nil? (:head s)))

 ;; A queue is a map with a pulling stack and a pushing stack:
 ;; {:push stack :pull stack}

(defn queue
  "returns an unbalanced queue"
  [& items]
  {:push (apply stack items) :pull (stack)})

(defn balance-queue
  "queue -> queue with nil pushing stack
  lazily attaches the pushing stack to the end of the pulling stack"
  [q]
  {:push nil
   :pull (reverse (concat (:push q) (:pull q)))})

(defn balanced?
  [q]
  (not (nil? (:pull q))))

(defn pop-queue
  "queue -> queue"
  [q]
  (let [b? (balanced? q)
        q (if b? q (balance-queue q))]
    {:push (:push q)
     :pull (pop (:pull q))}))

(defn push-queue
  "queue item -> queue"
  [q item]
  {:push (cons item (:push q))
   :pull (:pull q)})

;; TODO peek queue

(comment
  (stack 1 2 3)
  (stack)

  (push-stack (stack) 2)
  (push-stack (stack 1 2 3) 1)

  (pop-stack (stack 1 2 3))
  (pop-stack (push-stack (stack 1) 2))


  (queue 1 2 3)
  (:pull (balance-queue (queue 1 2 3))) 
  (balance-queue (balance-queue (queue 1 2 3))) ;nilpotent

  (pop-queue (push-queue (queue 1) 5))

  )
