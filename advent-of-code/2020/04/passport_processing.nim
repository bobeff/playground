import tables, sets, strutils

const
  inputFileName = "input.txt"

type
  Passport = Table[string, string]

  PassportFields = enum
    byr, iyr, eyr, hgt, hcl, ecl, pid, cid

proc addRecord(passport: var Passport, key, value: var string) =
  passport[key] = value
  key = ""
  value = ""

proc parseInput(inputFile: string): seq[Passport] =
  type
    ParsingState = enum
      parsingKey, parsingValue, nextPassport

  var
    currentPassport: Passport
    parsingState = parsingKey
    currentKey, currentValue: string
    data = inputFile.readFile

  for i, c in data:
    if parsingState == nextPassport:
      parsingState = parsingKey
      continue

    if c == ':':
      parsingState = parsingValue
    elif c == ' ' or (c == '\n' and data[i + 1] != '\n'):
      addRecord(currentPassport, currentKey, currentValue)
      parsingState = parsingKey
    elif c == '\n' and data[i + 1] == '\n':
      addRecord(currentPassport, currentKey, currentValue)
      result.add currentPassport
      currentPassport.clear
      parsingState = nextPassport
    else:
      case parsingState
        of parsingKey: currentKey &= c
        of parsingValue: currentValue &= c
        of nextPassport: assert false, "Must not happen."

  result.add currentPassport

proc validateFields(passport: Passport): bool =
  var flags: set[PassportFields]

  for key, _ in passport:
    case key
      of $byr: flags.incl byr
      of $iyr: flags.incl iyr
      of $eyr: flags.incl eyr
      of $hgt: flags.incl hgt
      of $hcl: flags.incl hcl
      of $ecl: flags.incl ecl
      of $pid: flags.incl pid
      of $cid: flags.incl cid
  
  let flagsValue = cast[uint16](flags)
  return flagsValue == 127 or flagsValue == 255

proc validateYear(value: string, min, max: uint): bool =
  try:
    var year = parseUInt(value)
    return year >= min and year <= max
  except:
    return false

proc validBirthYear(value: string): bool =
  validateYear(value, 1920, 2002)

proc validIssueYear(value: string): bool =
  validateYear(value, 2010, 2020)

proc validExpirationYear(value: string): bool =
  validateYear(value, 2020, 2030)

proc validHeight(value: string): bool =
  if value.len < 4:
    return false

  try:
    let
      number = parseUInt(value[0 .. ^3])
      unit = value[^2 .. ^1]

    case unit
      of "cm":
        return number >= 150 and number <= 193
      of "in":
        return number >= 59 and number <= 76 
      else:
        return false
  except:
    return false

proc validHairColor(value: string): bool =
  if value.len != 7:
    return false

  try:
    if value[0] != '#':
      return false
    for i in 1 .. 6:
      if value[i] notin {'0' .. '9', 'a' .. 'f'}:
        return false
  except:
    return false

  return true

proc validEyeColor(value: string): bool =
  const
    validEyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].toHashSet
  return value in validEyeColors

proc validPassportId(value: string): bool =
  if value.len != 9:
    return false

  for c in value:
    if c notin {'0' .. '9'}:
      return false

  return true

proc validateValues(passport: Passport): bool =
  validBirthYear(passport["byr"]) and
  validIssueYear(passport["iyr"]) and
  validExpirationYear(passport["eyr"]) and
  validHeight(passport["hgt"]) and
  validHairColor(passport["hcl"]) and
  validEyeColor(passport["ecl"]) and
  validPassportId(passport["pid"])

proc validPassportsCountPartOne(passports: seq[Passport]): uint =
  for passport in passports:
    if validateFields(passport):
      result.inc

proc validPassportsCountPartTwo(passports: seq[Passport]): uint =
  for passport in passports:
    if validateFields(passport) and validateValues(passport):
      result.inc

let passports = parseInput(inputFileName)
echo validPassportsCountPartOne(passports)
echo validPassportsCountPartTwo(passports)
