import unittest
import random

import my_solution as solution

STANDARD_DECK = ['King of Diamonds', 'Queen of Diamonds', 'Jack of Diamonds',
                 'Ten of Diamonds', 'Nine of Diamonds', 'Eight of Diamonds',
                 'Seven of Diamonds', 'Six of Diamonds', 'Five of Diamonds',
                 'Four of Diamonds', 'Three of Diamonds', 'Two of Diamonds',
                 'Ace of Diamonds', 'King of Clubs', 'Queen of Clubs', 'Jack of Clubs',
                 'Ten of Clubs', 'Nine of Clubs', 'Eight of Clubs', 'Seven of Clubs',
                 'Six of Clubs', 'Five of Clubs', 'Four of Clubs', 'Three of Clubs',
                 'Two of Clubs', 'Ace of Clubs', 'King of Hearts', 'Queen of Hearts',
                 'Jack of Hearts', 'Ten of Hearts', 'Nine of Hearts', 'Eight of Hearts',
                 'Seven of Hearts', 'Six of Hearts', 'Five of Hearts', 'Four of Hearts',
                 'Three of Hearts', 'Two of Hearts', 'Ace of Hearts', 'King of Spades',
                 'Queen of Spades', 'Jack of Spades', 'Ten of Spades', 'Nine of Spades',
                 'Eight of Spades', 'Seven of Spades', 'Six of Spades', 'Five of Spades',
                 'Four of Spades', 'Three of Spades', 'Two of Spades', 'Ace of Spades']

BELOTE_DECK = ['King of Diamonds', 'Queen of Diamonds', 'Jack of Diamonds',
               'Ten of Diamonds', 'Nine of Diamonds', 'Eight of Diamonds',
               'Seven of Diamonds', 'Ace of Diamonds', 'King of Clubs',
               'Queen of Clubs', 'Jack of Clubs', 'Ten of Clubs',
               'Nine of Clubs', 'Eight of Clubs', 'Seven of Clubs',
               'Ace of Clubs', 'King of Hearts', 'Queen of Hearts',
               'Jack of Hearts', 'Ten of Hearts', 'Nine of Hearts',
               'Eight of Hearts', 'Seven of Hearts', 'Ace of Hearts',
               'King of Spades', 'Queen of Spades', 'Jack of Spades',
               'Ten of Spades', 'Nine of Spades', 'Eight of Spades',
               'Seven of Spades', 'Ace of Spades']

SIXTY_SIX_DECK = ['King of Diamonds', 'Queen of Diamonds', 'Jack of Diamonds',
                  'Ten of Diamonds', 'Nine of Diamonds', 'Ace of Diamonds',
                  'King of Clubs', 'Queen of Clubs', 'Jack of Clubs', 'Ten of Clubs',
                  'Nine of Clubs', 'Ace of Clubs', 'King of Hearts', 'Queen of Hearts',
                  'Jack of Hearts', 'Ten of Hearts', 'Nine of Hearts', 'Ace of Hearts',
                  'King of Spades', 'Queen of Spades', 'Jack of Spades', 'Ten of Spades',
                  'Nine of Spades', 'Ace of Spades']

solution.Card.__hash__ = lambda self: 1


def random_cards(count=1):
    if count == 1:
        return solution.Card(random.choice(list(solution.RANKS.values())),
                    random.choice(list(solution.SUITS.values())))

    cards = set()
    while len(cards) < count:
        cards.add(solution.Card(random.choice(list(solution.RANKS.values())),
                       random.choice(list(solution.SUITS.values()))))

    return cards


class CardTest(unittest.TestCase):
    def test_card_instance(self):
        aos = solution.Card(solution.RANKS["Ace"], solution.SUITS["Spades"])
        self.assertIsInstance(aos.rank, solution.RANKS["Ace"])
        self.assertIsInstance(aos.suit, solution.SUITS["Spades"])

    def test_all_card_instances(self):
        for rank in solution.RANKS.values():
            for suit in solution.SUITS.values():
                card = solution.Card(rank, suit)
                self.assertIsInstance(card.rank, rank)
                self.assertIsInstance(card.suit, suit)

    def test_card_equals(self):
        aos1 = solution.Card(solution.RANKS["Ace"], solution.SUITS["Spades"])
        aos2 = solution.Card(solution.RANKS["Ace"], solution.SUITS["Spades"])
        self.assertEqual(aos1, aos2)
        self.assertNotEqual(aos1, solution.Card(solution.RANKS["Ace"], solution.SUITS["Hearts"]))

    def test_all_cards_equal(self):
        for rank in solution.RANKS.values():
            for suit in solution.SUITS.values():
                card1 = solution.Card(rank, suit)
                card2 = solution.Card(rank, suit)
                self.assertEqual(card1, card2)

    def test_suit_rank_equals(self):
        aos = solution.Card(solution.RANKS["Ace"], solution.SUITS["Spades"])
        self.assertEqual(aos.rank, solution.RANKS["Ace"]())
        self.assertEqual(aos.suit, solution.SUITS["Spades"]())

    def test_all_suits_ranks_equal(self):
        for rank in solution.RANKS.values():
            for suit in solution.SUITS.values():
                card = solution.Card(rank, suit)
                self.assertEqual(card.rank, rank())
                self.assertEqual(card.suit, suit())

    def test_to_string(self):
        aos = solution.Card(solution.RANKS["Ace"], solution.SUITS["Spades"])
        self.assertEqual(str(aos), "Ace of Spades")

    def test_all_to_string(self):
        for rank in solution.RANKS.values():
            for suit in solution.SUITS.values():
                card = solution.Card(rank, suit)
                self.assertEqual(str(card), str(card.rank)
                                 + " of " + str(card.suit))


class CardCollectionTest(unittest.TestCase):
    def setUp(self):
        self.deck = [solution.Card(rank, suit) for rank in solution.RANKS.values()
                     for suit in solution.SUITS.values()]

    def test_standard_deck(self):
        deck = solution.CardCollection(self.deck)
        self.assertEqual(len(deck), 52)

    def test_deck_add(self):
        deck = solution.CardCollection()
        card1, card2 = random_cards(2)

        deck.add(card1)
        self.assertEqual(deck[0], card1)

        deck.add(card2)
        self.assertEqual(deck[1], card2)

        self.assertEqual(len(deck), 2)

    def test_deck_order(self):
        deck = solution.CardCollection()
        card1, card2, card3 = random_cards(3)

        deck.add(card1), deck.add(card2), deck.add(card3)
        self.assertEqual(deck.top_card(), card3)
        self.assertEqual(deck.bottom_card(), card1)
        self.assertEqual(deck[1], card2)

    def test_deck_draw(self):
        deck = solution.CardCollection(self.deck)
        card = deck.top_card()

        self.assertEqual(card, deck.draw_from_top())
        self.assertEqual(len(deck), 51)

        card = deck.bottom_card()
        self.assertEqual(card, deck.draw_from_bottom())
        self.assertEqual(len(deck), 50)

        i = random.randint(0, 49)
        card = deck[i]
        self.assertEqual(card, deck.draw(i))
        self.assertEqual(len(deck), 49)

    def test_deck_index(self):
        deck = solution.CardCollection()
        card1, card2, card3 = random_cards(3)

        deck.add(card1)
        deck.add(card2)

        self.assertEqual(0, deck.index(card1))
        self.assertEqual(1, deck.index(card2))
        self.assertRaises(ValueError, lambda: deck.index(card3))

    def test_deck_iteration(self):
        deck = solution.CardCollection(self.deck)
        for card in deck:
            self.assertIsInstance(card, solution.Card)

    def test_standard_deck(self):
        cards = [str(card) for card in solution.StandardDeck()]
        self.assertEqual(STANDARD_DECK, cards)

    def test_belote_deck(self):
        cards = [str(card) for card in solution.BeloteDeck()]
        self.assertEqual(BELOTE_DECK, cards)

    def test_sixtysix_deck(self):
        cards = [str(card) for card in solution.SixtySixDeck()]
        self.assertEqual(SIXTY_SIX_DECK, cards)

if __name__ == '__main__':
    unittest.main()
