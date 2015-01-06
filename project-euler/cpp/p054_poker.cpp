#include <iostream>
#include <fstream>
#include <vector>
#include <cassert>
#include <algorithm>
#include <type_traits>

using namespace std;

template <typename E>
constexpr typename underlying_type<E>::type toInt(E e)
{
  return static_cast<typename underlying_type<E>::type>(e);
}

enum class CardRank : uint8_t
{
  TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE,
  TEN, JACK, QUEEN, KING, ACE, COUNT
};

enum class CardSuit : uint8_t
{
  SPADES, HEARTS, DIAMONDS, CLUBS, COUNT
};

enum class HandRank : uint8_t
{
  HIGH_CARD,
  ONE_PAIR,
  TWO_PAIRS,
  THREE_OF_A_KIND,
  STREIGHT,
  FLUSH,
  FULL_HOUSE,
  FOUR_OF_A_KIND,
  STREIGHT_FLUSH,
  COUNT
};

CardRank getCardRank(char rank)
{
  switch (rank)
  {
    case '2': return CardRank::TWO;
    case '3': return CardRank::THREE;
    case '4': return CardRank::FOUR;
    case '5': return CardRank::FIVE;
    case '6': return CardRank::SIX;
    case '7': return CardRank::SEVEN;
    case '8': return CardRank::EIGHT;
    case '9': return CardRank::NINE;
    case 'T': return CardRank::TEN;
    case 'J': return CardRank::JACK;
    case 'Q': return CardRank::QUEEN;
    case 'K': return CardRank::KING;
    case 'A': return CardRank::ACE;
    default: throw runtime_error("Invalid card rank: " + rank);
  }
}

CardSuit getCardSuit(char suit)
{
  switch (suit)
  {
    case 'C': return CardSuit::CLUBS;
    case 'D': return CardSuit::DIAMONDS;
    case 'H': return CardSuit::HEARTS;
    case 'S': return CardSuit::SPADES;
    default: throw runtime_error("Invalid card suit: " + suit);
  }
}

class Card
{
public:

  Card(CardRank rank, CardSuit suit) : m_Rank(rank), m_Suit(suit)
  {
    assert(rank < CardRank::COUNT);
    assert(suit < CardSuit::COUNT);
  }

  explicit Card(const string& card)
  {
    assert(card.length() == 2);
    m_Rank = getCardRank(card[0]);
    m_Suit = getCardSuit(card[1]);
  }

  CardRank rank() const { return m_Rank; }
  CardSuit suit() const { return m_Suit; }

private:
  CardRank m_Rank;
  CardSuit m_Suit;

}; // Card class

bool operator==(const Card& lhs, const Card& rhs)
{
  return lhs.rank() == rhs.rank() && lhs.suit() == rhs.suit();
}

bool operator>(const Card& lhs, const Card& rhs)
{
  return lhs.rank() > rhs.rank();
}

bool hasCount(uint8_t cardRankCounts[], uint8_t n, CardRank& tb)
{
  for (auto i = 0u; i < toInt(CardRank::COUNT); ++i)
    if (cardRankCounts[i] == n)
    {
      tb = CardRank(i);
      return true;
    }
  return false;
}

class Hand
{
  friend bool operator>(const Hand& lhs, const Hand& rhs);

public:

  explicit Hand(const string& cards)
  {
    assert(cards.length() == 14); // 5 cards * 2 chars + 4 separator
    auto i = 0u;
    while (i < cards.length())
    {
      auto rank = getCardRank(cards[i++]);
      auto suit = getCardSuit(cards[i++]);
      m_Cards.emplace_back(rank, suit);
      ++i;
    }
    stable_sort(m_Cards.begin(), m_Cards.end(), greater<Card>());
    determineHandRank();
    assert(m_Cards.size() == 5);
  }

  size_t   size() const { return m_Cards.size(); }
  HandRank rank() const { return m_Rank; }

  const Card& operator[](size_t index) const
  {
    assert(index < size());
    return m_Cards[index];
  }

private: // methods

  void copyTiebreakersDifferentOf(CardRank rank1, CardRank rank2)
  {
    for (auto i = 0u; i < size(); ++i)
    {
      auto rank = m_Cards[i].rank();
      if (rank != rank1 && rank != rank2)
        m_Tiebreakers.emplace_back(rank);
    }
  }

  void determineHandRank()
  {
    // determine hand cards with equal ranks
    uint8_t cardRankCounts[toInt(CardRank::COUNT)] = {0};
    for (auto i = 0u; i < size(); ++i)
      ++cardRankCounts[toInt(m_Cards[i].rank())];

    // tiebreakers
    CardRank tb1(CardRank::COUNT);
    CardRank tb2(CardRank::COUNT);

    // determine hand rank
    if (isStreightFlush())
      m_Rank = HandRank::STREIGHT_FLUSH;
    else if (isFourOfAKind(cardRankCounts, tb1))
      m_Rank = HandRank::FOUR_OF_A_KIND;
    else if (isFullHouse(cardRankCounts, tb1, tb2))
      m_Rank = HandRank::FULL_HOUSE;
    else if (isFlush())
      m_Rank = HandRank::FLUSH;
    else if (isStreight())
      m_Rank = HandRank::STREIGHT;
    else if (isThreeOfAKind(cardRankCounts, tb1))
      m_Rank = HandRank::THREE_OF_A_KIND;
    else if (isTwoPairs(cardRankCounts, tb1, tb2))
      m_Rank = HandRank::TWO_PAIRS;
    else if (isOnePair(cardRankCounts, tb1))
      m_Rank = HandRank::ONE_PAIR;
    else
      m_Rank = HandRank::HIGH_CARD;

    // determine tiebrekaers
    if (tb1 != CardRank::COUNT) m_Tiebreakers.emplace_back(tb1);
    if (tb2 != CardRank::COUNT) m_Tiebreakers.emplace_back(tb2);
    copyTiebreakersDifferentOf(tb1, tb2);
  }

  bool isStreightFlush() const
  {
    return isStreight() && isFlush();
  }

  bool isFourOfAKind(uint8_t cardRankCounts[], CardRank& tb) const
  {
    return hasCount(cardRankCounts, 4, tb);
  }

  bool isFullHouse(uint8_t cardRankCounts[],
                   CardRank& tb1, CardRank& tb2) const
  {
    return hasCount(cardRankCounts, 3, tb1) &&
           hasCount(cardRankCounts, 2, tb2);
  }

  bool isFlush() const
  {
    for (auto i = 0u; i < size() - 1; ++i)
      if (m_Cards[i].suit() != m_Cards[i + 1].suit())
        return false;
    return true;
  }

  bool isStreight() const
  {
    for (auto i = 0u; i < size() - 1; ++i)
      if (toInt(m_Cards[i].rank()) != toInt(m_Cards[i + 1].rank()) + 1)
        return false;
    return true;
  }

  bool isThreeOfAKind(uint8_t cardRankCounts[], CardRank& tb) const
  {
    return hasCount(cardRankCounts, 3, tb);
  }

  bool isTwoPairs(uint8_t cardRankCounts[],
                  CardRank& tb1, CardRank& tb2) const
  {
    auto pairsCount = 0u;
    for (auto i = 0u; i < toInt(CardRank::COUNT); ++i)
      if (2 == cardRankCounts[i])
      {
        if (CardRank::COUNT == tb1)
          tb1 = CardRank(i);
        else
          tb2 = CardRank(i);
        ++pairsCount;
      }
    return 2 == pairsCount;
  }

  bool isOnePair(uint8_t cardRankCounts[], CardRank& tb) const
  {
    return hasCount(cardRankCounts, 2, tb);
  }

private: // data

  vector<Card> m_Cards;
  vector<CardRank> m_Tiebreakers;
  HandRank m_Rank;

}; // Hand class

bool operator>(const Hand& lhs, const Hand& rhs)
{
  if (lhs.rank() > rhs.rank()) return true;
  if (lhs.rank() < rhs.rank()) return false;

  assert(lhs.m_Tiebreakers.size() == rhs.m_Tiebreakers.size());

  for (auto i = 0u; i < lhs.m_Tiebreakers.size(); ++i)
  {
    if (lhs.m_Tiebreakers[i] > rhs.m_Tiebreakers[i])
      return true;
    if (lhs.m_Tiebreakers[i] < rhs.m_Tiebreakers[i])
      return false;
  }

  return false;
}

void runTests()
{
  Card card("5H");
  assert(card.rank() == CardRank::FIVE);
  assert(card.suit() == CardSuit::HEARTS);

  Hand hand("4D 6S 9H QH QC");
  assert(hand[0] == Card(CardRank::QUEEN, CardSuit::HEARTS));
  assert(hand[1] == Card(CardRank::QUEEN, CardSuit::CLUBS));
  assert(hand[2] == Card(CardRank::NINE,  CardSuit::HEARTS));
  assert(hand[3] == Card(CardRank::SIX,   CardSuit::SPADES));
  assert(hand[4] == Card(CardRank::FOUR,  CardSuit::DIAMONDS));

  assert(Hand("TD JD KD QD AD").rank() == HandRank::STREIGHT_FLUSH);
  assert(Hand("2S 2D 3D 2C 2H").rank() == HandRank::FOUR_OF_A_KIND);
  assert(Hand("2H 2D 4C 4D 4S").rank() == HandRank::FULL_HOUSE);
  assert(Hand("3D 6D 7D TD QD").rank() == HandRank::FLUSH);
  assert(Hand("7D 6C TC 9C 8C").rank() == HandRank::STREIGHT);
  assert(Hand("JD 6D JS 9S JH").rank() == HandRank::THREE_OF_A_KIND);
  assert(Hand("QH 2D QS TS TD").rank() == HandRank::TWO_PAIRS);
  assert(Hand("AS 5D JH QD 5C").rank() == HandRank::ONE_PAIR);
  assert(Hand("AS QD JD TD 9D").rank() == HandRank::HIGH_CARD);

  // not a ties: Flush with Diamonds vs. Three Aces
  assert(Hand("3D 6D 7D TD QD") > Hand("2D 9C AS AH AC"));
  // high card ties
  assert(Hand("5D 8C 9S JS AC") > Hand("2C 5C 7D 8S QH"));
  // one pair ties
  assert(Hand("7D 7S 2S 3S 4D") > Hand("2D 3D 4S 5S 5D"));
  assert(Hand("5D 5S 6S 8S 9S") > Hand("5H 5C 6C 7C 9C"));
  // two pair ties
  assert(Hand("9D 9S KD KS AS") > Hand("TC TS 9C 9H AH"));
  assert(Hand("QH QS JH JS AH") > Hand("QC QD TD TC AS"));
  assert(Hand("2S 2H 3S 3H JC") > Hand("2C 2D 3C 3D TS"));
  // three of a kind ties
  assert(Hand("JS JC JH TH 9S") > Hand("8S 8C 8H TC 9C"));
  assert(Hand("JS JC JH TH 8S") > Hand("JS JC JH 9H 8C"));
  assert(Hand("JS JC JH TH 7S") > Hand("JS JC JH TC 6C"));
  // streight ties
  assert(Hand("JS TC 9S 8C 7S") > Hand("TS 9C 8S 7H 6H"));
  // flush ties
  assert(Hand("AS QS 7S 2S 3S") > Hand("KH 9H 8H 3H 2H"));
  // full house ties
  assert(Hand("TH TS TC QS QH") > Hand("9H 9S 9C QC QD"));
  assert(Hand("9H 9S 9C KS KH") > Hand("9H 9S 9C QC QD"));
  // four of a kind ties
  assert(Hand("KC KS KD KH 2D") > Hand("JC JS JD JH 2S"));
  assert(Hand("AC AS AD AH 3C") > Hand("AC AS AD AH 2S"));
  // streight flush ties
  assert(Hand("AS KS QS JS TS") > Hand("KH QH JH TH 9H"));
}

int main()
{
  runTests();

  ifstream f("p054_poker.txt");

  size_t playerOneWinsCount = 0;

  static const uint8_t MAX_LINE_LENGTH = 32;
  char line[MAX_LINE_LENGTH];

  while(f.getline(line, MAX_LINE_LENGTH))
  {
    string playerOneCards(line, line + 14);
    string playerTwoCards(line + 15, line + 29);

    bool isPlayerOneWins = Hand(playerOneCards) > Hand(playerTwoCards);

    cout << "Player 1: " << playerOneCards << " vs. "
         << "Player 2: " << playerTwoCards << " Wins: "
         << (isPlayerOneWins ? "1" : "2") << endl;

    playerOneWinsCount += int(isPlayerOneWins);
  }

  cout << playerOneWinsCount << endl;

  return 0;
}
