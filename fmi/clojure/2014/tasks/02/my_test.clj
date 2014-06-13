(use 'clojure.test)

(load-file "my_solution.clj")

(def input-string
  "My Favourite Things.          John Coltrane.      Jazz, Bebop.        popular, cover
Alabama.                      John Coltrane.      Jazz, Avantgarde.
Boplicity.                    Miles Davis.        Jazz, Bebop
Autumn Leaves.                Bill Evans.         Jazz.               popular
Waltz for Debbie.             Bill Evans.         Jazz
Pathetique.                   Beethoven.          Classical
Fur Elise.                    Beethoven.          Classical.          popular
Toccata e Fuga.               Bach.               Classical, Baroque.
Eine Kleine Nachtmusik.       Mozart.             Classical.          violin, fancy")

(def expected-collection
  #{{:title "My Favourite Things"
     :artist "John Coltrane"
     :genre "Jazz"
     :subgenre "Bebop"
     :tags #{:jazz :bebop :popular :cover}}

    {:title "Alabama"
     :artist "John Coltrane"
     :genre "Jazz"
     :subgenre "Avantgarde"
     :tags #{:jazz :avantgarde}}

    {:title "Boplicity"
     :artist "Miles Davis"
     :genre "Jazz"
     :subgenre "Bebop"
     :tags #{:jazz :bebop}}

    {:title "Autumn Leaves"
     :artist "Bill Evans"
     :genre "Jazz"
     :subgenre nil
     :tags #{:jazz :popular}}

    {:title "Waltz for Debbie"
     :artist "Bill Evans"
     :genre "Jazz"
     :subgenre nil
     :tags #{:jazz}}

    {:title "Pathetique"
     :artist "Beethoven"
     :genre "Classical"
     :subgenre nil
     :tags #{:classical}}

    {:title "Fur Elise"
     :artist "Beethoven"
     :genre "Classical"
     :subgenre nil
     :tags #{:classical :popular}}

    {:title "Toccata e Fuga"
     :artist "Bach"
     :genre "Classical"
     :subgenre "Baroque"
     :tags #{:classical :baroque}}

    {:title "Eine Kleine Nachtmusik"
     :artist "Mozart"
     :genre "Classical"
     :subgenre nil
     :tags #{:classical :violin :fancy}}})

(def actual-collection (make-collection input-string))

(deftest test-make-collection
  (is (= actual-collection expected-collection)))

(deftest test-search
  (is (= (search actual-collection :artist {:genre "Classical"})
         #{"Beethoven" "Bach" "Mozart"}))

  (is (= (search actual-collection :title {:genre "Jazz" :subgenre "Bebop"})
         #{"My Favourite Things" "Boplicity"}))

  (is (= (search actual-collection :genre {:tag :popular})
         #{"Jazz" "Classical"}))

  (is (= (search actual-collection :subgenre {:tag :classical})
         #{"Baroque"}))

  (is (= (search actual-collection :tags {:genre "Classical"})
         #{:classical :popular :baroque :violin :fancy}))

  (is (= (search actual-collection :subgenre [:or {:tag :jazz}
                                                  {:tag :classical}])
         #{"Bebop" "Avantgarde" "Baroque"}))

  (is (= (search actual-collection :subgenre [:or {:tag :bebop}
                                                  {:tag :avantgarde}
                                                  {:tag :baroque}])
          #{"Bebop" "Avantgarde" "Baroque"}))

  (is (= (search actual-collection :subgenre [:or {:tags #{:jazz :bebop}}
                                                  {:tags #{:jazz :avantgarde}}
                                                  {:tags #{:classical :baroque}}])
         #{"Bebop" "Avantgarde" "Baroque"}))

  (is (= (search actual-collection :title {:tags #{:jazz :bebop :popular :cover}})
      #{"My Favourite Things"}))

  (is (= (search actual-collection :title nil)
      #{"My Favourite Things" "Alabama" "Boplicity" "Autumn Leaves"
        "Waltz for Debbie" "Pathetique" "Fur Elise" "Toccata e Fuga"
        "Eine Kleine Nachtmusik"}))

  (is (= (search actual-collection :title {})
      #{"My Favourite Things" "Alabama" "Boplicity" "Autumn Leaves"
        "Waltz for Debbie" "Pathetique" "Fur Elise" "Toccata e Fuga"
        "Eine Kleine Nachtmusik"}))

  (is (= (search actual-collection :title [:and {:genre "Jazz"}
                                                [:not {:subgenre nil}]])
         #{"My Favourite Things" "Alabama" "Boplicity"}))

  (is (= (search actual-collection :title {:subgenre nil})
         #{"Autumn Leaves" "Waltz for Debbie" "Pathetique" "Fur Elise"
           "Eine Kleine Nachtmusik"}))

  (is (= (search actual-collection
                 :title
                 [:and [:or {:tag :bebop}
                            {:tag :avantgarde}
                            {:tag :baroque}]
                       [:not {:tags #{:jazz :bebop :popular :cover}}]])
          #{"Boplicity" "Alabama" "Toccata e Fuga"})))
  
(run-tests)
