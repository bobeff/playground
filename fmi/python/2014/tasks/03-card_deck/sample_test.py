import unittest
import random

from my_solution import Card, RANKS, SUITS, CardCollection


Card.__hash__ = lambda self: 1


def random_cards(count=1):
    if count == 1:
        return Card(random.choice(list(RANKS.values())),
                    random.choice(list(SUITS.values())))

    cards = set()
    while len(cards) < count:
        cards.add(Card(random.choice(list(RANKS.values())),
                       random.choice(list(SUITS.values()))))

    return cards


class CardTest(unittest.TestCase):
    def test_card_instance(self):
        aos = Card(RANKS["Ace"], SUITS["Spades"])
        self.assertIsInstance(aos.rank, RANKS["Ace"])
        self.assertIsInstance(aos.suit, SUITS["Spades"])

    def test_card_equals(self):
        aos1 = Card(RANKS["Ace"], SUITS["Spades"])
        aos2 = Card(RANKS["Ace"], SUITS["Spades"])
        self.assertEqual(aos1, aos2)

    def test_suit_rank_equals(self):
        aos = Card(RANKS["Ace"], SUITS["Spades"])
        self.assertEqual(aos.rank, RANKS["Ace"]())
        self.assertEqual(aos.suit, SUITS["Spades"]())

    def test_to_string(self):
        aos = Card(RANKS["Ace"], SUITS["Spades"])
        self.assertEqual(str(aos), "Ace of Spades")


class CardCollectionTest(unittest.TestCase):
    def setUp(self):
        self.deck = [Card(rank, suit) for rank in RANKS.values()
                     for suit in SUITS.values()]

    def test_standard_deck(self):
        deck = CardCollection(self.deck)
        self.assertEqual(len(deck), 52)

    def test_deck_add(self):
        deck = CardCollection()
        card1, card2 = random_cards(2)

        deck.add(card1)
        self.assertEqual(deck[0], card1)

        deck.add(card2)
        self.assertEqual(deck[1], card2)

        self.assertEqual(len(deck), 2)

    def test_deck_draw(self):
        deck = CardCollection(self.deck)
        card = deck.top_card()

        self.assertEqual(card, deck.draw_from_top())
        self.assertEqual(len(deck), 51)

    def test_deck_iteration(self):
        deck = CardCollection(self.deck)
        for card in deck:
            self.assertIsInstance(card, Card)

if __name__ == '__main__':
    unittest.main()
