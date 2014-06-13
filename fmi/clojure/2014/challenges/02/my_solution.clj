(def rules {
  [:scissors :paper   ] :cut
  [:paper    :rock    ] :covers
  [:rock     :lizard  ] :crushes
  [:lizard   :spock   ] :poisons
  [:spock    :scissors] :smashes
  [:scissors :lizard  ] :decapitate
  [:lizard   :paper   ] :eats
  [:paper    :spock   ] :disproves
  [:spock    :rock    ] :vaporizes
  [:rock     :scissors] :crushes})

(defn fight [weapon1 weapon2]
  (let [frst (rules [weapon1 weapon2])
        scnd (rules [weapon2 weapon1])]
    (cond
      frst [weapon1 frst weapon2]
      scnd [weapon2 scnd weapon1])))
