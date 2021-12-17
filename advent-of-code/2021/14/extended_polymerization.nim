import strutils, sequtils, tables, algorithm

proc readInput(fileName: string):
    tuple[tmpl: string, rules: Table[string, string]] =
  let data = fileName.readFile.split("\n\n")
  result.tmpl = data[0]
  for line in data[1].strip.split("\n"):
    let rule = line.split(" -> ")
    result.rules[rule[0]] = rule[1]

proc applyRules(tmpl: string, rules: Table[string, string], steps: int): string =
  var tmpl = tmpl
  for step in 0 ..< steps:
    var nextTmpl: string
    for i in 0 ..< tmpl.len - 1:
      let key = tmpl[i .. i + 1]
      nextTmpl &= key[0]
      nextTmpl &= rules[key]
    nextTmpl &= tmpl[^1]
    tmpl = nextTmpl
  tmpl

proc getIndex(c: char): int =
  c.ord - 'A'.ord

proc buildHistogram(polymer: string): array[26, uint64] =
  for c in polymer:
    result[c.getIndex] += 1

proc analyse(histogram: array[26, uint64]): uint64 =
  var hist = histogram.filterIt(it != 0)
  hist.sort
  hist[^1] - hist[0]

proc solvePart1(tmpl: string, rules: Table[string, string]): uint64 =
  let polymer = applyRules(tmpl, rules, 10)
  let histogram = buildHistogram(polymer)
  analyse(histogram)

proc solvePart2(tmpl: string, rules: Table[string, string]): uint64 =
  var histogram = buildHistogram(tmpl)
  var pairs: Table[string, uint64]
  for key, _ in rules:
    pairs[key] = 0
  for i in 0 ..< tmpl.len - 1:
    let key = tmpl[i .. i + 1]
    pairs[key].inc
  for step in 0 ..< 40:
    var newPairs = pairs
    for key, value in pairs:
      let ruleValue = rules[key]
      histogram[ruleValue[0].getIndex] += value
      newPairs[key[0] & ruleValue] += value
      newPairs[ruleValue & key[1]] += value
      newPairs[key] -= value
    pairs = newPairs
  analyse(histogram)

let (tmpl, rules) = readInput("input.txt")
echo solvePart1(tmpl, rules)
echo solvePart2(tmpl, rules)
