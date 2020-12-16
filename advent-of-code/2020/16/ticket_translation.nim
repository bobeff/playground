import tables, sets, strutils, sequtils, algorithm

type
  ValueRange = tuple[l, r: uint]
  TicketField = tuple[f, s: ValueRange]
  Ticket = seq[uint]
  PositionToFields = tuple[pos: uint, fields: HashSet[string]]

var
  myTicket: Ticket
  nearbyTickets: seq[Ticket]
  validTickets: seq[Ticket]
  ticketFields: Table[string, TicketField]

proc parseTicket(data: string): Ticket =
  data.split(',').mapIt(it.parseUint)

proc readInput(fileName: string) =
  let fileParts = fileName.readFile.split("\n\n")
  for line in fileParts[0].split('\n'):
    let parts = line.split(':')
    let numbers = parts[1].split({' ', '-', 'o', 'r'}).filterIt(
      it != "").mapIt(it.parseUInt)
    ticketFields[parts[0]] = ((numbers[0], numbers[1]),
                              (numbers[2], numbers[3]))
  myTicket = parseTicket(fileParts[1].split('\n')[1])
  let parts = fileParts[2].split('\n')
  for i in 1 ..< parts.len:
    nearbyTickets.add parseTicket(parts[i])

proc solvePartOne(): uint =
  for ticket in nearbyTickets:
    var isValid = true
    for field in ticket:
      var inRange = false
      for _, tf in ticketFields:
        if (tf[0][0] <= field and field <= tf[0][1]) or
           (tf[1][0] <= field and field <= tf[1][1]):
          inRange = true
      if not inRange:
        result += field
        isValid = false
    if isValid:
      validTickets.add ticket

proc solvePartTwo(): uint =
  var allFields: HashSet[string]
  for tfn, _ in ticketFields:
    allFields.incl tfn

  var positionToFields = newSeqOfCap[PositionToFields](myTicket.len)
  for tp in 0 ..< myTicket.len:
    positionToFields.add (tp.uint, allFields)

  for tp in 0 ..< myTicket.len:
    for ticket in validTickets:
      for name, tf in ticketFields:
        if (tf[0][0] > ticket[tp] or ticket[tp] > tf[0][1]) and
           (tf[1][0] > ticket[tp] or ticket[tp] > tf[1][1]):
          positionToFields[tp.uint].fields.excl name

  positionToFields.sort do (x, y: PositionToFields) -> int:
    x.fields.card - y.fields.card

  for i in 0 ..< positionToFields.len - 1:
    for j in i + 1 ..< positionToFields.len:
      for field in positionToFields[i].fields:
        positionToFields[j].fields.excl field

  result = 1
  for pf in positionToFields:
    if pf.fields.toSeq[0].startsWith("departure"):
      result *= myTicket[pf.pos]

readInput("input.txt")
echo solvePartOne()
echo solvePartTwo()
