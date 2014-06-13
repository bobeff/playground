(use 'clojure.test)

(load-file "my_solution.clj")

(def songs-text2
"My Favourite Things.          John Coltrane.      Jazz, Bebop.        popular, cover
Alabama.                      John Coltrane.      Jazz, Avantgarde.
Boplicity.                    Miles Davis.        Jazz, Bebop
Autumn Leaves.                Bill Evans.         Jazz.               popular
Waltz for Debbie.             Bill Evans.         Jazz
Pathetique.                   Beethoven.          Classical
Fur Elise.                    Beethoven.          Classical.          popular
Toccata e Fuga.               Bach.               Classical, Baroque.
Eine Kleine Nachtmusik.       Mozart.             Classical.          violin, fancy")

(deftest task-02-sample-test
  (def collection (make-collection songs-text2))
  (is (= (search collection :artist {:genre "Jazz"})
         #{"John Coltrane" "Miles Davis" "Bill Evans"}))
  (is (= (search collection :subgenre {:artist "John Coltrane"})
         #{"Bebop" "Avantgarde"}))
  (is (= (search collection :tags {:artist "John Coltrane"})
         #{:jazz :bebop :popular :cover :avantgarde}))
  (is (= (search collection :title [:and {:genre "Classical"} {:tag :violin}])
         #{"Eine Kleine Nachtmusik"}))
  (is (= (search collection :title [:and {:genre "Jazz"}
                                         [:not {:title "My Favourite Things"}]
                                         [:or {:tag :popular} {:tag :bebop}]])
         #{"Autumn Leaves" "Boplicity"}))) 

(run-tests)
