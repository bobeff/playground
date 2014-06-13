from collections import OrderedDict


class Rank:
    def __init__(self, symbol):
        self.symbol = symbol

    def __eq__(self, other):
        return type(self) == type(other)

    def __str__(self):
        return self.__class__.__name__


class Suit:
    def __init__(self, color):
        self.color = color

    def __eq__(self, other):
        return type(self) == type(other)

    def __str__(self):
        return self.__class__.__name__


RANK_NAMES = (('King', 'K'), ('Queen', 'Q'), ('Jack',  'J'), ('Ten', '10'),
              ('Nine', '9'), ('Eight', '8'), ('Seven', '7'), ('Six', '6'),
              ('Five', '5'), ('Four',  '4'), ('Three',  '3'), ('Two', '2'),
              ('Ace',  'A'))


SUIT_NAMES = (('Diamonds', 'red'), ('Clubs',  'black'),
              ('Hearts',   'red'), ('Spades', 'black'))


def create_rank_class(name, symbol):
    return type(
        name, (Rank,), {'__init__': lambda self: Rank.__init__(self, symbol)})


def create_suit_class(name, color):
    return type(
        name, (Suit,), {'__init__': lambda self: Suit.__init__(self, color)})


RANKS = OrderedDict(
    (rank, create_rank_class(rank, symbol)) for rank, symbol in RANK_NAMES)


SUITS = OrderedDict(
    (suit, create_suit_class(suit, color)) for suit, color in SUIT_NAMES)


class Card:
    def __init__(self, rank, suit):
        self.__rank = rank()
        self.__suit = suit()

    @property
    def rank(self):
        return self.__rank

    @property
    def suit(self):
        return self.__suit

    def __eq__(self, other):
        return self.rank == other.rank and self.suit == other.suit

    def __str__(self):
        return '{0} of {1}'.format(self.rank, self.suit)

    def __repr__(self):
        return '<Card {0} of {1}>'.format(self.rank, self.suit)


class CardCollection:
    def __init__(self, collection=[]):
        self.__cards = list(collection)

    def draw(self, index):
        return self.__cards.pop(index)

    def draw_from_top(self):
        return self.__cards.pop()

    def draw_from_bottom(self):
        return self.__cards.pop(0)

    def top_card(self):
        return self.__cards[-1]

    def bottom_card(self):
        return self.__cards[0]

    def add(self, card):
        self.__cards.append(card)

    def index(self, card):
        return self.__cards.index(card)

    def __getitem__(self, index):
        return self.__cards[index]

    def __setitem__(self, index, card):
        seld.__cards[index] = card

    def __iter__(self):
        return iter(self.__cards)

    def __repr__(self):
        return repr(self.__cards)

    def __len__(self):
        return len(self.__cards)


def StandardDeck():
    return CardCollection(
        Card(rank, suit) for suit in SUITS.values() for rank in RANKS.values())


def BeloteDeck():
    return CardCollection(card for card in StandardDeck()
                          if card.rank.symbol not in map(str, range(2, 7)))


def SixtySixDeck():
    return CardCollection(card for card in StandardDeck()
                          if card.rank.symbol not in map(str, range(2, 9)))
