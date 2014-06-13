from collections import OrderedDict


class Card:
    def __init__(self, rank, suit):
        self._rank = rank()
        self._suit = suit()

    def __eq__(self, other):
        return self.rank == other.rank and self.suit == other.suit

    @property
    def rank(self):
        return self._rank

    @property
    def suit(self):
        return self._suit

    def __repr__(self):
        return "<Card " + self.__str__() + ">"

    def __str__(self):
        return str(self.rank) + " of " + str(self.suit)


class Suit:
    color = "blank"

    def __str__(self):
        return self.__class__.__name__

    def __eq__(self, other):
        return self.__class__ == other.__class__


class Rank:
    symbol = ""

    def __str__(self):
        return self.__class__.__name__

    def __eq__(self, other):
        return self.__class__ == other.__class__


ORDERED_RANKS = OrderedDict([("Ace", "A"), ("Two", "2"), ("Three", "3"),
                             ("Four", "4"), ("Five", "5"), ("Six", "6"),
                             ("Seven", "7"), ("Eight", "8"), ("Nine", "9"),
                             ("Ten", "10"), ("Jack", "J"), ("Queen", "Q"),
                             ("King", "K")])
ORDERED_SUITS = OrderedDict([("Spades", "black"), ("Hearts", "red"),
                             ("Clubs", "black"), ("Diamonds", "red")])

RANKS = {rank: type(rank, (Rank,), {"symbol": symbol})
         for rank, symbol in ORDERED_RANKS.items()}

SUITS = {suit: type(suit, (Suit,), {"color": color})
         for suit, color in ORDERED_SUITS.items()}


class CardCollection(list):

    def draw(self, index):
        return self.pop(index)

    def draw_from_top(self):
        return self.draw(len(self) - 1)

    def draw_from_bottom(self):
        return self.draw(0)

    def top_card(self):
        return self[len(self) - 1]

    def bottom_card(self):
        return self[0]

    def add(self, card):
        self.append(card)


def StandardDeck():
    cards = [Card(RANKS[rank], SUITS[suit])
             for suit in ORDERED_SUITS.keys() for rank in ORDERED_RANKS.keys()]
    cards.reverse()
    return CardCollection(cards)


def BeloteDeck():
    return CardCollection(filter(lambda x: x.rank.symbol >= '7'
                                 or x.rank.symbol == '10', StandardDeck()))


def SixtySixDeck():
    return CardCollection(filter(lambda x: x.rank.symbol >= '9'
                                 or x.rank.symbol == '10', BeloteDeck()))
