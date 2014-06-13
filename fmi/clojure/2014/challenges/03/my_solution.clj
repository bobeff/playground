(defn chase [n coll]
  (let [len (count coll)]
    (map #(nth coll (rem % len))
         (iterate (partial + n) (dec n)))))
