(defn from-digits [sequence]
  {:pre [(every? #(<= 0 % 9) sequence)]}
  (reduce (fn [accum elem] (+ (* accum 10) elem)) 0 sequence))
