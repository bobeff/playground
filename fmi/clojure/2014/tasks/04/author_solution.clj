(require '[clojure.walk :as w])

(defn ->seq
  "It x is sequential?, just return it. If x is not sequential?
  return list containing x."
  [x]
  (if (sequential? x)
    x
    (list x)))

(defn alternates
  "Split coll into 'threads' subsequences, feeding each alternately
   from the input sequence."
  [threads coll]
  (lazy-seq
    (when (seq coll)
      (apply map list (partition threads coll)))))

(defn delay-all
  "Wrap all expressions with (delay)."
  [exprs]
  (map #(list 'delay %) exprs))

(defn prepare
  "Prepare patterns for matching by replacing some of the syntax
   with things that match? can work with."
  [patterns]
  (-> {'_ '(quote _)
       :fn   fn?
       :int  integer?
       :seq  sequential?
       :odd  odd?
       :str  string?
       :even even?
       :or   (fn [& args]
               #((set args) %))}
      (w/postwalk-replace patterns)))

(defn match?
  "Perform match of two things. There are two simple rules:

    - underscore ('_) matches everything
    - f matches x if (f x) is logical true"
  [x y]
  (let [x (->seq x)
        y (->seq y)]
    (when (= (count x)
             (count y))
      (->> (map vector x y)
           (every?
            (fn [[x y :as pair]]
              (or (= x '_)

                  (and (fn? x)
                       (x y))

                  (= x y))))))))

(defmacro match
  "Defines convenient syntax for pattern matching. `vals` are actual
   values that will be matched. `clauses` are pattern-expr pairs.
   The macro tries to match each pattern sequentialy and stops when
   finds a match. Not-reached patterns shouldn't be evaluated. Only
   the body of matched pattern should be evaluated."
  [vals & clauses]
  (let [[tests bodies] (alternates 2 clauses)
        tests (->> tests
                   (prepare)
                   (delay-all)
                   (map-indexed vector)
                   (cons 'list))
        bodies (->> bodies
                    (delay-all)
                    (vec))]
    `(let [evals# ~vals]
       (let [res# (->> ~tests
                       (filter #(match? (force (second %)) evals#))
                       (first))]
         (when res#
           (->> (first res#)
                (nth ~bodies)
                (force)))))))
