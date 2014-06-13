(defn digits [n]
  (loop [result '() x n]
    (if (= x 0)
      (if (empty? result) '(0) result)
      (recur (conj result (rem x 10)) (quot x 10)))))

;-----------------------------------------------------------------------------

(defn prime-factors [n]
  (loop [result [] x n d 2]
    (if (<= x 1)
      result
      (if (= 0 (rem x d))
        (recur (conj result d) (quot x d) d)
        (recur result x (inc d))))))

;-----------------------------------------------------------------------------

(defn fizzbuzz [n]
  (letfn [(fzbz [x]
            (cond
              (= 0 (rem x 3) (rem x 5)) "fizzbuzz"
              (= 0 (rem x 3)) "fizz"
              (= 0 (rem x 5)) "buzz"
              :else x))]
    (map fzbz (range 1 (inc n)))))

;-----------------------------------------------------------------------------

(defn update-map [m x]
  (if (m x)
    (update-in m [x] inc)
    (assoc m x 1)))

(defn densities [xs]
  (let [unique-elems-count (reduce update-map {} xs)]
    (map #(unique-elems-count %) xs)))

;-----------------------------------------------------------------------------

(defn index-by [f xs]
  (reduce (fn [m x] (assoc m (f x) x)) {} xs))

;-----------------------------------------------------------------------------

(defn harmonic [n]
  (reduce (fn [accum x] (+ accum (/ 1 x))) 0 (range 1 (inc n))))

;-----------------------------------------------------------------------------

(defn uniquify [in]
  (letfn [(get-elem [m x] (if (m x) (str x "-" (m x)) x))]
    (loop [m {} l in result []]
      (if (empty? l)
        result
        (recur (update-map m (first l))
               (rest l)
               (conj result (get-elem m (first l))))))))
