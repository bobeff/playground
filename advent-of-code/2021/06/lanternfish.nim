import strutils, sequtils, tables

proc simulate_slow(data: seq[int], days: int): int =
  var data = data
  for i in 0 ..< days:
    let n = data.len
    for j in 0 ..< n:
      if data[j] == 0:
        data.add 8
        data[j] = 6
      else:
        data[j].dec
  data.len

proc newTable: Table[int, uint64] =
  for i in 0 .. 8:
    result[i] = 0

proc simulate(data: seq[int], days: int): uint64 =
  var t = newTable()
  for x in data:
    t[x].inc
  for i in 0 ..< days:
    var tt = newTable()
    for k, v in t:
      if k == 0:
        tt[6] += v
        tt[8] += v
      else:
        tt[k - 1] += v
    t = tt
  for _, v in t:
    result += v

let data = "input.txt".readFile.strip.split(',').mapIt(it.parseInt)
echo simulate_slow(data, 80)
echo simulate(data, 256)
