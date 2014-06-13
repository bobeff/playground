# Камък-ножица-хартия-гущер-Спок

[Камък-ножица-хартия-гущер-Спок](http://www.youtube.com/watch?v=iapcKVn7DdY) е
разширение на класическата игра Камък-ножица-хартия. Правилата са следните:

|               |               |          |
| ------------- |:-------------:| --------:|
| scissors      | cut           | paper    |
| paper         | covers        | rock     |
| rock          | crushes       | lizard   |
| lizard        | poisons       | spock    |
| spock         | smashes       | scissors |
| scissors      | decapitate    | lizard   |
| lizard        | eats          | paper    |
| paper         | disproves     | spock    |
| spock         | vaporizes     | rock     |
| rock          | crushes       | scissors |

Напишете функция `fight`, която приема две от оръжията и връща резултата от
борбата между тях:

    (fight :scissors :spock) ; [:spock :smashes :scissors]
    (fight :lizard :paper)   ; [:lizard :eats :paper]

Резултатът трябва да е вектор от 3 ключови думи или nil в случай, че няма победител. Примерните тестове са [тук](https://github.com/fmi/clojure-homework/blob/master/challenges/02/sample_test.clj).
