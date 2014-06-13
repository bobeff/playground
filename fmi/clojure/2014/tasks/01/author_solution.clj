(defn digits [n]
  (loop [n n
         digits []]
    (let [digit (rem n 10)
          remaining (quot n 10)
          new-digits (conj digits digit)]
      (if (zero? remaining)
        (reverse new-digits)
        (recur remaining new-digits)))))

(defn prime-factors [n]
  (loop [n n
         i 2
         factors []]
    (cond (= n 1) factors
          (zero? (rem n i)) (recur (/ n i) i (conj factors i))
          :else (recur n (inc i) factors))))

(defn divides? [a b]
  (zero? (rem a b)))

(defn fizzbuzz [n]
  (for [i (range 1 (inc n))]
    (cond (divides? i 15) "fizzbuzz"
          (divides? i 5) "buzz"
          (divides? i 3) "fizz"
          :else i)))

(defn densities [xs]
  (map (frequencies xs) xs))

(defn index-by [f xs]
  (reduce #(assoc %1 (f %2) %2)
          {}
          xs))

(defn harmonic [n]
  (->> (inc n)
       (range 1)
       (map #(/ 1 %))
       (reduce +)))

(defn uniquify [coll]
  (reduce
   (fn [xs s]
     (loop [n 1
            c s]
       (if (some #{c} xs)
         (recur (inc n) (str s "-" n))
         (conj xs c))))
   []
   coll))
