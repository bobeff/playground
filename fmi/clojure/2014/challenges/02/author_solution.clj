(defn fight [x y]
  (let [solutions #{[x y] [y x]}]
    (->> [[:scissors :cut        :paper   ]
          [:paper    :covers     :rock    ]
          [:rock     :crushes    :lizard  ]
          [:lizard   :poisons    :spock   ]
          [:spock    :smashes    :scissors]
          [:scissors :decapitate :lizard  ]
          [:lizard   :eats       :paper   ]
          [:paper    :disproves  :spock   ]
          [:spock    :vaporizes  :rock    ]
          [:rock     :crushes    :scissors]]
         (filter (fn [[a _ b]] (solutions [a b])))
         (first))))
