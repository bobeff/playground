(require '[clojure.set :refer [difference]])

(defn add-prize
  "Creates a prize in random point on the field."
  [{:keys [width height location prizes] :as snake}]
  (-> (for [i (range width)
            j (range height)]
        [i j])
      (set)
      (difference location prizes)
      (seq)
      (rand-nth)
      (->> (update-in snake [:prizes] conj))))

(defn move
  "Moves the snake in the current direction."
  [{:keys [location dir width height prizes] :as snake}]
  (let [pos (mapv + (last location) dir)]
    (cond
     (some (partial = pos) (rest location))
     false ;; bite

     (or (>= (first pos) width)
         (>= (second pos) height))
     false ;; wall

     (prizes pos)
     (-> snake
         (update-in [:prizes] disj pos)
         (update-in [:location] conj pos)
         (add-prize))

     :else
     (-> snake
         (update-in [:location] subvec 1)
         (update-in [:location] conj pos)))))

(defn danger?
  "Are we going to die in two moves?"
  [snake]
  (->> (iterate move snake)
       (take 3)
       (some false?)))

(defn turn
  "Changes snake direction."
  [snake dir]
  (assoc snake :dir (case dir
                      :top    [ 0 -1]
                      :right  [ 1  0]
                      :left   [-1  0]
                      :bottom [ 0  1])))
