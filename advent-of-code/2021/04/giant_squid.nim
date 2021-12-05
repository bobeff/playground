import strutils, sequtils, std/enumerate

const ticketSize = 5

type
  Ticket = object
    numbers: array[ticketSize, array[ticketSize, int]]
    marks: array[ticketSize, array[ticketSize, bool]]
    finalized: bool

proc parseTicket(ticket: string): Ticket =
  for (i, line) in enumerate(ticket.splitLines):
    if i >= ticketSize:
      return
    let numbers = line.split(' ').filterIt(it != "").mapIt(it.parseInt)
    for j in 0 ..< ticketSize:
      result.numbers[i][j] = numbers[j]

proc readInput(fileName: string):
    tuple[numbers: seq[int], tickets: seq[Ticket]] =
  let content = fileName.readFile
  let parts = content.split("\n\n")
  result.numbers = parts[0].split(",").mapIt(it.parseInt)
  for i in 1 ..< parts.len:
    result.tickets.add parseTicket(parts[i])

proc mark(ticket: var Ticket, number: int) =
  for i in 0 ..< ticketSize:
    for j in 0 ..< ticketSize:
      if ticket.numbers[i][j] == number:
        ticket.marks[i][j] = true

proc check(ticket: Ticket): bool =
  for i in 0 ..< ticketSize:
    var hasRow = true
    var hasColumn = true
    for j in 0 ..< ticketSize:
      hasRow = hasRow and ticket.marks[i][j]
      hasColumn = hasColumn and ticket.marks[j][i]
    if hasRow or hasColumn:
      return true

proc calc(ticket: Ticket): int =
  for i in 0 ..< ticketSize:
    for j in 0 ..< ticketSize:
      if not ticket.marks[i][j]:
        result += ticket.numbers[i][j]

proc processTickets(tickets: var seq[Ticket], numbers: seq[int]):
    tuple[firstToWin, lastToWin: int] =
  for number in numbers:
    for ticket in tickets.mitems:
      mark(ticket, number)
      if not ticket.finalized and check(ticket):
        if result.firstToWin == 0:
          result.firstToWin = calc(ticket) * number
        result.lastToWin = calc(ticket) * number
        ticket.finalized = true

var (numbers, tickets) = "input.txt".readInput
let (firstToWin, lastToWin) = processTickets(tickets, numbers)
echo firstToWin
echo lastToWin
