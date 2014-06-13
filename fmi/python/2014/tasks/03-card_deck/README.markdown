# Тесте карти #

Задачата ви е да моделирате стандартното тесте от 52 карти за покер (жокери не са нужни). За целта трябва да използвате принципите на ООП.

Ето класовете които очакваме да видим:

### Rank ###

    class Rank # вид на картата
      symbol   # символът на картата (A, 2, Q, K)

### Suit ###

    class Suit # боя на картата
      color    # цветът на боята ('red', 'black')

### RANKS ###
това трябва да е речник с класове, наследяващи `Rank` и представляващи всеки вид карта. Например:

    >>> RANKS
    {'Six': <class '__main__.Six'>, 'Four': <class '__main__.Four'>, ... }

За пълен списък с ключовете на масива вж. по-долу.

### SUITS ###
речник с класове, наследяващи `Suit` и представляващи всеки вид боя. Например:

    >>> SUITS
    {'Hearts': <class '__main__.Hearts'>, 'Clubs': <class '__main__.Clubs'>, 'Spades': <class '__main__.Spades'>, 'Diamonds': <class '__main__.Diamonds'>}

Видовете карти и боите трябва да са сравними с оператора '=='. Например:

    >>> RANKS["Ace"]() == RANKS["Ace"]()
    True
    >>> SUITS["Spades"]() == SUITS["Spades"]()
    True

Видовете карти и боите трябва да имат репрезентация като низ:

     >>> str(RANKS["Ace"]())
     Ace
     >>> str(SUITS["Spades"]())
     'Spades'


### Card ###

    class Card # Клас който представлява една карта за игра
        rank
        suit
        __init__(self, rank, suit)

Конструктура на `Card` приема клас `rank` и `suit` (не тяхна инстанция). Класът е непроменяем (immutable). Например:
  
    >>> aos = Card(RANKS["Ace"], SUITS["Spades"])
    >>> aos.rank
    <__main__.Ace object at 0x7f2632ed5890>
    >>> aos.suit
    <__main__.Spades object at 0x7f2632ed5950>
    >>> aos.suit = SUITS["Hearts"]()
    Traceback (most recent call last):
      File "<stdin>", line 1, in <module>
      AttributeError: can't set attribute
      
Инстанциите на `Card` трябва да са сравними с `==` (като `Rank` и `Suit`).
`Card` трябва да има репрезентация като низ под тази форма:

    >>> str(aos)
    'Ace of Spades'

### CardCollection ###

    class CardCollection # Представлява колекция (тесте) от карти
        __init__(self, collection)
        draw(self, index)
        draw_from_top(self)
        draw_from_bottom(self)
        top_card(self)
        bottom_card(self)
        add(self, card)
        index(self, card)

Конструктора на `Card` пиема каквато и да е колекция от инстанции на `Card` като начални данни. Още карти могат да се добавят с метода add. Първата карта в колекцията се счита за тази най-долу в тестето. Тоест индекс 0 е най-долната карта. Метода `add` добавя карти отгоре. Класът трябва да може да се индексира с числа, и трябва да може да се итерира с `for` цикъл.

    >>> deck[13]
    <Card King of Clubs>
    >>> deck[1337]
    Traceback (most recent call last):
      File "<stdin>", line 1, in <module>
    IndexError: list index out of range

Метода `draw` връща картата на определен индекс и я премахва от колекцията, `draw_from_top` и `draw_from_bottom` са аналогични. Методите `top_card и bottom_card` връщат съответните карти без да ги махат от колекцията.

Метода `index` търси за определена карта в тестето и връща индекса ѝ. Ако има няколко еднакви карти се връща индекса на първата намерена. Търсенето става чрез сравнение, т.е. не е задължително инстанцията на Асо Пика което подавате на `index` да е в колекцията, просто трябва да има Асо Пика в нея. Ако няма такава карта метода хвърля `ValueError`:

    >>> deck.index(Card(RANKS["Two"], SUITS["Hearts"]))
    Traceback (most recent call last):
      File "<stdin>", line 1, in <module>
    ValueError: <Card Two of Hearts> is not in list

### Стандартни тестета ###
Очакваме да видим и 3 функции които генерират стандартните тестета за Покер, Белот и Сантасе:

    StandardDeck()
    BeloteDeck()
    SixtySixDeck()

### Карти и подредба ###
Ключовете в `dict`-овете ви трябва да са именовани точно по този начин. Тестовете ни разчитат на това.

    >>> RANKS.keys()
    dict_keys(['King', 'Six', 'Jack', 'Five', 'Queen', 'Ten', 'Ace', 'Three', 'Eight', 'Four', 'Two', 'Seven', 'Nine'])
    >>> SUITS.keys()
    dict_keys(['Diamonds', 'Hearts', 'Spades', 'Clubs'])

Стандартната подредба на картите е по боя и след това по вид. Ето стандартната подредба на пълното тесте. Тестетата     които `StandardDeck`, `BeloteDeck` и `SixtySixDeck` генерират трябва да са подредени по този начин.

    >>> StandardDeck()
    [<Card King of Diamonds>, <Card Queen of Diamonds>, <Card Jack of Diamonds>, <Card Ten of Diamonds>, <Card Nine of Diamonds>, <Card Eight of Diamonds>, <Card Seven of Diamonds>, <Card Six of Diamonds>, <Card Five of Diamonds>, <Card Four of Diamonds>, <Card Three of Diamonds>, <Card Two of Diamonds>, <Card Ace of Diamonds>, <Card King of Clubs>, <Card Queen of Clubs>, <Card Jack of Clubs>, <Card Ten of Clubs>, <Card Nine of Clubs>, <Card Eight of Clubs>, <Card Seven of Clubs>, <Card Six of Clubs>, <Card Five of Clubs>, <Card Four of Clubs>, <Card Three of Clubs>, <Card Two of Clubs>, <Card Ace of Clubs>, <Card King of Hearts>, <Card Queen of Hearts>, <Card Jack of Hearts>, <Card Ten of Hearts>, <Card Nine of Hearts>, <Card Eight of Hearts>, <Card Seven of Hearts>, <Card Six of Hearts>, <Card Five of Hearts>, <Card Four of Hearts>, <Card Three of Hearts>, <Card Two of Hearts>, <Card Ace of Hearts>, <Card King of Spades>, <Card Queen of Spades>, <Card Jack of Spades>, <Card Ten of Spades>, <Card Nine of Spades>, <Card Eight of Spades>, <Card Seven of Spades>, <Card Six of Spades>, <Card Five of Spades>, <Card Four of Spades>, <Card Three of Spades>, <Card Two of Spades>, <Card Ace of Spades>]

`BeloteDeck` и `SixtySixDeck` запазват подредбата на стандартното тесте, но Белот се играе без картите 2-6, а Сантасе(66) се играе без картите 2-8.

Ако имате още въпроси не се колебайте да попитате във форума.
