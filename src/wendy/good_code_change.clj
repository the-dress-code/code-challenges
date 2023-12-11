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


;; create collection of all possible coin counts
;; given any coin and any target

(defn coin-counts
  [target coin]
  (range (inc (quot target coin))))


; write a fn that creates a collection of maps, where each of a given key is incremented until it exceeds the target value. works on one provided key.

(defn update-key-for-range
  [m k r] 
   (map (fn [x] 
          (assoc m k x)) 
        (range r)))


; write a loop recur that walks thru a collection.

(defn walk-the-coll
  [coll]
  (loop [remaining coll
         result []]
    (let [primero (first remaining)
          new-coll (conj result primero)]
      (if (seq remaining)
        (recur (rest remaining) new-coll)
        result))))


;; write a loop that creates a collection of maps, where each of a given key is incremented until it exceeds the target value. uses one key.

(defn maps-for-range
  [m r k]
  (loop [remaining (range r)
         result ()]
    (let [count (first remaining)
          new-map (assoc m k count)
          new-coll (conj result new-map)]
      (if (seq remaining)
        (recur (rest remaining) new-coll)
        result))))


;; write a fn that walks thru ALL of your keys. 

(defn walk-thru-all-keys
  [m]
  (map (fn [x] 
         (assoc m x 0)) 
       (keys m)))


;; write a fn that walks thru ALL of your keys, makes collections of maps, incrementing a key val based on a range. takes a map and range val.


(defn make-maps-inc-all-keys
  [m r]
  (map (fn [k] 
         (maps-for-range m r k)) 
       (keys m)))




