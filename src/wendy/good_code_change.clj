(ns wendy.good-code-change)


; inc a value in a hash map

(defn inc-me
  [m k v]
  (assoc m k (inc v)))

; write a loop recur that processes a collection

(defn walk-the-coll
  [coll]
  (loop [remaining coll
         result []]
    (let [primero (first remaining)
          new-coll (conj result primero)]
      (if (seq remaining)
        (recur (rest remaining) new-coll)
        result))))

