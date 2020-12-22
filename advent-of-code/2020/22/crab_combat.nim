import strutils, sequtils, deques, sets, hashes

type
  Deck = Deque[int]

proc parseDeck(s: string): Deck =
  result = s.split('\n')[1 .. ^1].mapIt(it.parseInt).toDeque

proc readInput(fileName: string): tuple[deck1, deck2: Deck] =
  let players = fileName.readFile.split("\n\n")
  result.deck1 = parseDeck(players[0])
  result.deck2 = parseDeck(players[1])

proc evaluate(deck: Deck): int =
  for i, card in deck:
    result += card * (deck.len - i)

proc playGame(deck1, deck2: var Deck): int =
  while deck1.len > 0 and deck2.len > 0:
    let card1 = deck1.popFirst
    let card2 = deck2.popFirst
    if card1 > card2:
      deck1.addLast card1
      deck1.addLast card2
    else:
      deck2.addLast card2
      deck2.addLast card1

  return evaluate(if deck1.len > 0: deck1 else: deck2)

proc hash(deck: Deck): Hash =
  var h: Hash = 0
  for elem in deck:
    h = h !& hash(elem)
  result = !$h

proc copyCards(deck: Deck, count: int): Deck =
  for i in 0 ..< count:
    result.addLast deck[i]

proc playRecursiveGame(deck1, deck2: var Deck, game: int): int =
  var deck1History, deck2History: HashSet[Deck]
  while deck1.len > 0 and deck2.len > 0:
    if deck1 in deck1History or deck2 in deck2History:
      return 1
    else:
      deck1History.incl deck1
      deck2History.incl deck2
      let card1 = deck1.popFirst
      let card2 = deck2.popFirst

      let roundWinner =
        if card1 <= deck1.len and card2 <= deck2.len:
          var
            newDeck1 = copyCards(deck1, card1)
            newDeck2 = copyCards(deck2, card2)
          playRecursiveGame(newDeck1, newDeck2, game + 1)
        else:
          if card1 > card2: 1 else: 2

      if roundWinner == 1:
        deck1.addLast card1
        deck1.addLast card2
      else:
        deck2.addLast card2
        deck2.addLast card1

  return if deck2.len == 0: 1 else: 2

let decks = readInput("input.txt")
var (deck1, deck2) = decks
echo playGame(deck1, deck2)
(deck1, deck2) = decks
let winner = playRecursiveGame(deck1, deck2, 1)
echo evaluate(if winner == 1: deck1 else: deck2)
