(ns wendy.good-code-change)


; make a starter map from a coinset

(defn seed-map
  [coinset]
  (->> coinset
       (map (fn [coin] [coin 0]))
       (into {})))

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


; make a loop that can create a collection of maps where each of a specific key is incremented until it exceeds the target value

(defn inc-range-for-key
  [m r k]
  (loop [remaining (range r)
         result ()]
    (let [count (first remaining)
          new-map (assoc m k count)
          new-coll (conj result new-map)]
      (if (seq remaining)
        (recur (rest remaining) new-coll)
        result))))
