(defn new-prize [prizes location width height]
  (->> (fn [] [(rand-int width) (rand-int height)])
       repeatedly
       (filter #(not (or (prizes %) ((set location) %))))
       first))

(defn self-intersection? [location head]
  (-> (set location)
      (disj (first location))
      (contains? head)
      not))

(defn move [snake]
  (let [{:keys [dir width height prizes location]} snake
        head (last location)
        next-x (+ (first dir) (first head))
        next-y (+ (second dir)  (second head))
        new-head [next-x next-y]
        new-location (conj location new-head)]
    (if (and 
          ; is new position in field ?
          (<= 0 next-x (dec width))
          (<= 0 next-y (dec height))
          ; check for self intersection
          (self-intersection? location new-head))
      (if-let [prize (prizes new-head)]
        ; eat prize
        (-> snake
            ; extend snake
            (assoc-in [:location] new-location)
            ; remove consumed prize
            (update-in [:prizes] #(disj % prize))
            ; add new prize at random position
            (update-in [:prizes]
                       #(conj % (new-prize % new-location width height))))
        ; move snake without eating prize
        (assoc-in snake [:location] (subvec new-location 1)))
      ; the snake dies
      false)))

(defn turn [snake direction]
  (assoc-in snake
            [:dir]
            (case direction
              :left   [-1  0]
              :right  [ 1  0]
              :top    [ 0 -1]
              :bottom [ 0  1])))

(defn danger? [snake]
  (if-let [snake (move snake)]
    (if-let [snake (move snake)]
      false
      true)
    true))
