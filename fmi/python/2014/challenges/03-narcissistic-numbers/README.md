# Нарцисистки числа

## Леко встъпление ##

[Нарцисизъм](http://en.wikipedia.org/wiki/Narcissism) означава любов към себе
си и се свързва с преследването на удовлетворение от суетата или егоистично
възхищение на собстевните физически или умствени качества. Произлиза във връзка
с [Нарцис](http://en.wikipedia.org/wiki/Narcissus_(mythology)) от
древногръцката митология, който се влюбил във собственото си отражение.

## И сега [Narcissistic числата](http://en.wikipedia.org/wiki/Narcissistic_number) ... ##

### Неформално ###

Числа влюбени в себе си(очевидно).

### Формално ###

Естественото (в рамките на тази задача ще считаме и 0 за естествено) число ```n``` се нарича нарцистично, ако
изпълнява следното условие:
![](https://raw.github.com/fmi/python-homework/master/2014/challenges/narcissistic-numbers/images/condition.png)

Тоест сумата от цифрите на чилото, повдигнати на степен броя на цифрите, да е
самото число.

Аналогично стоят нещата и при числа в произволна бройна система.

#### Примери ####

![](https://raw.github.com/fmi/python-homework/master/2014/challenges/narcissistic-numbers/images/example0.png)

![](https://raw.github.com/fmi/python-homework/master/2014/challenges/narcissistic-numbers/images/example1.png)

## След всичките дефиниции ето и самото условие ##

Като видни нарцисисти напишете функция предикат ```is_narcissistic```, която,
по дадено число и бройна система(2-36), ви помага да разберете дали числото е
*нарцисистко*, или не.

```base``` параметърът указва бройната система, в която е ```number``` и по
подразбиране е ```10```.

#### Сигнатура ####

```python
def is_narcissistic(number, base=10)
    ...
```

#### Примери ####

```
>> is_narcissistic(10)
False
>> is_narcissistic(223, 4)
True
>> is_narcissistic(115132219018763992565095597973971522401)
True
```
