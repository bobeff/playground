(require ['clojure.string :as 'str]
         ['clojure.set :as 'set])

(defn make-tags
  [input]
  (->> (str/split input #",\s*")
       (map (comp keyword str/lower-case))
       (set)))

(defn make-song
  [input]
  (let [[title artist genre-subgenre tags] (str/split input #"\.\s*")
        [genre subgenre] (str/split genre-subgenre #",\s*")
        tags (make-tags (str genre-subgenre ", " tags))]
    {:title    title
     :artist   artist
     :genre    genre
     :subgenre subgenre
     :tags     tags}))

(defn make-collection
  [input]
  (map make-song (str/split-lines input)))

(defn attribute-met?
  [song [attribute value]]
  (if (= attribute :tag)
    ((:tags song) value)
    (= (attribute song) value)))

(defn criteria-type
  [criteria]
  (cond (map? criteria)
        :attributes

        (vector? criteria)
        (first criteria)))

(defn criteria-met?
  [song criteria]
  (case (criteria-type criteria)
    :attributes (every? #(attribute-met? song %) criteria)
    :and        (every? #(criteria-met? song %) (rest criteria))
    :or         (some #(criteria-met? song %) (rest criteria))
    :not        (not (criteria-met? song (second criteria)))
    (throw RuntimeException (str "Unrecognized criteria: " (prn-str criteria)))))

(defn extract
  [song attribute]
  (let [value (attribute song)]
    (cond (set? value) value
          (nil? value) #{}
          :else #{value})))

(defn search
  [collection attribute criteria]
  (->> collection
       (filter #(criteria-met? % criteria))
       (map #(extract % attribute))
       (apply set/union)))
