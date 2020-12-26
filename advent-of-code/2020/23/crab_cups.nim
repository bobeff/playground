import lists

const
  #input = "389125467" # sample input
  input = "476138259"
  partOneMaxValue = 9
  partTwoMaxValue = 1_000_000

type
  Ring = object
    ring: DoublyLinkedRing[int]
    nodes: seq[DoublyLinkedNode[int]]

  Triple = array[3, int]

proc parseInput(input: string): Ring =
  result.ring = initDoublyLinkedRing[int]()
  for c in input:
    result.ring.append(ord(c) - ord('0'))

proc initNodes(ring: var Ring, maxValue: int) =
  ring.nodes = newSeq[DoublyLinkedNode[int]](maxValue)
  ring.nodes[ring.ring.head.value - 1] = ring.ring.head
  var node = ring.ring.head.next
  while node != ring.ring.head:
    ring.nodes[node.value - 1] = node
    node = node.next

proc initRingPartOne(input: string): Ring =
  result = input.parseInput
  result.initNodes(partOneMaxValue)

proc initRingPartTwo(input: string): Ring =
  result = input.parseInput
  for i in 10 .. 1_000_000:
    result.ring.append(i)
  result.initNodes(partTwoMaxValue)

proc remove(ring: var Ring): Triple =
  ring.ring.head = ring.ring.head.next
  for i in 0 ..< 3:
    result[i] = ring.ring.head.value
    ring.nodes[ring.ring.head.value - 1] = nil
    ring.ring.remove(ring.ring.head)
    ring.ring.head = ring.ring.head.next

proc getNextCupNumber(currentValue, maxValue: int): int =
  if currentValue > 1: currentValue - 1 else: maxValue

proc getNextCupNumber(currentValue, maxValue: int, triple: Triple): int =
  var nextValue = getNextCupNumber(currentValue, maxValue)
  while nextValue == triple[0] or
        nextValue == triple[1] or
        nextValue == triple[2]:
    nextValue = getNextCupNumber(nextValue, maxValue)
  return nextValue

proc advanceTo(ring: var Ring, value: int) =
  ring.ring.head = ring.nodes[value - 1]

proc insert(ring: var Ring, triple: Triple) =
  ring.ring.head = ring.ring.head.next
  for i in countdown(2, 0):
    ring.ring.prepend(triple[i])
    ring.nodes[ring.ring.head.value - 1] = ring.ring.head

proc transform(ring: var Ring, steps, maxValue: int) =
  for i in 1 .. steps:
    let currentElement = ring.ring.head
    let triple = ring.remove
    let nextValue = getNextCupNumber(currentElement.value, maxValue, triple)
    ring.advanceTo(nextValue)
    ring.insert(triple)
    ring.ring.head = currentElement.next

proc getAnswerPartOne(ring: var Ring): string =
  ring.advanceTo(1)
  ring.ring.head = ring.ring.head.next
  while ring.ring.head.value != 1:
    result &= chr(ord('0') + ring.ring.head.value)
    ring.ring.head = ring.ring.head.next

proc getAnswerPartTwo(ring: var Ring): int =
  ring.advanceTo(1)
  ring.ring.head = ring.ring.head.next
  return ring.ring.head.value * ring.ring.head.next.value

var ringPartOne = initRingPartOne(input)
ringPartOne.transform(100, partOneMaxValue)
echo getAnswerPartOne(ringPartOne)

var ringPartTwo = initRingPartTwo(input)
ringPartTwo.transform(10_000_000, partTwoMaxValue)
echo getAnswerPartTwo(ringPartTwo)
