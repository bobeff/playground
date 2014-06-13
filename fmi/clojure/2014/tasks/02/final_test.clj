(use 'clojure.test)

(load-file "my_solution.clj")

(def songs-text
"My Favourite Things.          John Coltrane.      Jazz, Bebop.        popular, cover
Greensleves.                  John Coltrane.      Jazz, Bebop.        popular, cover
Alabama.                      John Coltrane.      Jazz, Avantgarde.   melancholic
Acknowledgement.              John Coltrane.      Jazz, Avantgarde
Afro Blue.                    John Coltrane.      Jazz.               melancholic
'Round Midnight.              John Coltrane.      Jazz
My Funny Valentine.           Miles Davis.        Jazz.               popular
Tutu.                         Miles Davis.        Jazz, Fusion.       weird, cool
Miles Runs the Voodoo Down.   Miles Davis.        Jazz, Fusion.       weird
Boplicity.                    Miles Davis.        Jazz, Bebop
Autumn Leaves.                Bill Evans.         Jazz.               popular
Waltz for Debbie.             Bill Evans.         Jazz
'Round Midnight.              Thelonious Monk.    Jazz, Bebop
Ruby, My Dear.                Thelonious Monk.    Jazz.               saxophone
Fur Elise.                    Beethoven.          Classical.          popular
Moonlight Sonata.             Beethoven.          Classical.          popular
Pathetique.                   Beethoven.          Classical
Toccata e Fuga.               Bach.               Classical, Baroque. popular
Goldberg Variations.          Bach.               Classical, Baroque
Eine Kleine Nachtmusik.       Mozart.             Classical.          popular, violin")

(def collection (make-collection songs-text))

(deftest task-02-test
  (is (= (search collection :artist {:genre "Jazz"})
         #{"John Coltrane" "Miles Davis" "Bill Evans" "Thelonious Monk"})
      "Map criteria: genre.")

  (is (= (search collection :subgenre {:artist "John Coltrane"})
         #{"Bebop" "Avantgarde"})
      "Map criteria: artist.")

  (is (= (search collection :title {:tag :popular})
         #{"My Favourite Things" "Greensleves" "My Funny Valentine" "Autumn Leaves"
           "Fur Elise" "Moonlight Sonata" "Toccata e Fuga" "Eine Kleine Nachtmusik"})
      "Map criteria: tag.")

  (is (= (search collection :tags {:artist "John Coltrane"})
         #{:jazz :bebop :popular :cover :avantgarde :melancholic})
      "Tags search.")

  (is (= (search collection :title {:genre "Classical", :tag :popular})
         #{"Fur Elise" "Moonlight Sonata" "Toccata e Fuga" "Eine Kleine Nachtmusik"})
      "Map criteria: two attributes.")

  (is (= (search collection :title [:and {:genre "Classical"} {:tag :popular}])
         #{"Fur Elise" "Moonlight Sonata" "Toccata e Fuga" "Eine Kleine Nachtmusik"})
      "Conjunction.")

  (is (= (search collection :title [:or {:subgenre "Avantgarde"} {:tag :baroque}])
         #{"Alabama" "Acknowledgement" "Toccata e Fuga" "Goldberg Variations"})
      "Disjunction.")

  (is (= (search collection :artist [:not {:genre "Jazz"}])
         #{"Beethoven" "Bach" "Mozart"})
      "Negation.")

  (is (= (search collection :title [:and {:subgenre "Bebop"}
                                         [:not {:title "Greensleves"}]
                                         [:or {:artist "Thelonious Monk"} {:artist "Miles Davis"}]])
         #{"Boplicity" "'Round Midnight"})
      "Nested criteria (I).")

  (is (= (search collection :title [:and {:tag :popular}
                                         [:not {:genre "Classical"}]
                                         [:not {:tag :cover}]])
         #{"My Funny Valentine" "Autumn Leaves"})
      "Nested criteria (II).")

  (is (= (search collection :title [:and {:tag :popular}
                                         [:not {:genre "Classical"}]
                                         {:tag :cover}])
         #{"My Favourite Things" "Greensleves"})
      "Nested criteria (III).")

  (is (= (search collection :title [:or [:and {:tag :baroque}
                                              {:tag :popular}]
                                        [:and {:tag :jazz}
                                              {:tag :melancholic}
                                              [:not {:tag :avantgarde}]]])
         #{"Toccata e Fuga" "Afro Blue"})
      "Nested criteria (IV)."))

(run-tests)
