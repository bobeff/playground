(defn from-digits [digits]
  (reduce #(+ (* %1 10) %2)
          0
          digits))
