(require '[clojure.string :refer [split-lines split trim lower-case]]
         '[clojure.set :refer [intersection union difference]])

(defn get-words [s regex]
  (map trim (split s regex)))

(defn create-tags-set [tags genre subgenre]
  (->> (conj (get-words tags #",") genre subgenre)
       (remove empty?)
       (map (comp keyword lower-case))
       set))

(defn make-collection [input]
  (set (for [line (split-lines input)
             :let [[title artist genres tags] (get-words line #"\.")
                   [genre subgenre] (get-words genres #",")
                   tags (if (seq tags) tags "")
                   tags-set (create-tags-set tags genre subgenre)]]
    {:title title
     :artist artist
     :genre genre
     :subgenre subgenre
     :tags tags-set})))

(defn match-record [k v record]
  (or (and (= k :tag) (v (:tags record)))
      (= (k record) v)))

(defn process-criteria [collection criteria]
  (apply intersection
         (for [kv criteria
               :let [k (key kv)
                     v (val kv)]]
           (set (filter (partial match-record k v) collection)))))

(declare get-target-set)

(defn process-compound-criteria [collection criteria]
  (letfn [(process-rest-criteria []
            (map (partial get-target-set collection)
                 (rest criteria)))]
    (condp = (first criteria)
      :and (apply intersection (process-rest-criteria))
      :or  (apply union (process-rest-criteria))
      :not (->> (second criteria)
                (get-target-set collection)
                (difference collection)))))

(defn get-target-set [collection criteria]
  (cond (empty? criteria) collection
        (map? criteria) (process-criteria collection criteria)
        (vector? criteria) (process-compound-criteria collection criteria)))

(defn search [collection field criteria]
  (set (flatten (for [m (get-target-set collection criteria)
                      :let [entry (field m)
                            entry (if (set? entry) (seq entry) entry)]
                      :when (seq entry)]
                  entry))))
