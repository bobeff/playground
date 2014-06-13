(require '[clojure.walk :refer [postwalk]])


(def ^:private underscore (gensym))


(declare match-sequences?)
(declare match-or-clause?)


(defn- match-items? [pattern-item expression-item]
  (cond
    (= pattern-item underscore) true
    (= pattern-item :fn)  (fn? expression-item)
    (= pattern-item :int) (integer? expression-item)
    (= pattern-item :seq) (sequential? expression-item)
    (= pattern-item :str) (string? expression-item)  

    (= pattern-item :odd)
    (and (integer? expression-item) (odd? expression-item))
    
    (= pattern-item :even)
    (and (integer? expression-item) (even? expression-item))

    (and (sequential? pattern-item) (= (first pattern-item) :or))
    (match-or-clause? (rest pattern-item) expression-item)

    :else (= pattern-item expression-item)))


(defn- match-or-clause? [[pattern-head & pattern-tail], expression]
  (if (nil? pattern-head)
    false
    (or (match-items? pattern-head expression)
        (match-or-clause? pattern-tail expression))))


(defn- match-sequences?
  [[pattern-head & pattern-tail], [expression-head & expression-tail]]
  (cond
    (and (nil? pattern-head) (nil? expression-head)) true
    (or  (nil? pattern-head) (nil? expression-head)) false
    :else (and (match-items? pattern-head expression-head)
               (match-sequences? pattern-tail expression-tail))))


(defn- vectorize [clause]
  (if (and (sequential? clause)
           (not= (first clause) :or))
    clause
    (vector clause)))


(defn- match? [pattern expression]
  (let [pattern-sequence (vectorize pattern)
        expression-sequence (vectorize expression)]
    (match-sequences? pattern-sequence expression-sequence)))


(defn- vectorize-or-clause [clause]
  (if (and (sequential? clause)
           (= (first clause) :or))
    (vec clause)
    clause))


(defmacro match [expression & pairs]
  `(let [~'_ underscore]
    (condp match? ~expression
      ~@(map (partial postwalk vectorize-or-clause) pairs)
      nil)))
