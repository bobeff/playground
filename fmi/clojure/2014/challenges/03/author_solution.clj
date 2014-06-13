(defn chase
  [n coll]
  (->> coll
       cycle
       (drop (dec n))
       (take-nth n)))
