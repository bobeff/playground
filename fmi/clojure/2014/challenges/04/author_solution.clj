(defn subseq?
  "Check if coll1 is subseq of coll2."
  [coll1 coll2]
  (some #{coll1} (partition (count coll1) 1 coll2)))

(defn ->str
  "Transform parsed range data into string."
  [rng]
  (if (> (count rng) 2)
    (str (first rng) ".." (last rng))
    (apply str rng)))

(defn shorten-ranges
  "Walk the string from left to right and shorten
   the best possible ranges found this way."
  [s rng]
  (loop [[item :as input] s
         result  ""
         current []]
    (let [nxt (conj current item)]
      (cond
       (empty? input)
       (str result (->str current))

       (subseq? nxt rng)
       (recur (next input)
              result
              nxt)

       :else
       (recur (next input)
              (str result (->str current))
              [item])))))
