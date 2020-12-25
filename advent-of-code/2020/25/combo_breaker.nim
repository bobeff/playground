import sugar

const
  cardPublicKey = 5099500
  doorPublicKey = 7648211
  # cardPublicKey = 5764801
  # doorPublicKey = 17807724
  subjectNumber = 7
  divider = 20201227

proc calcLoopSize(subjectNumber, publicKey: uint): uint =
  var
    value = 1'u
    times = 1'u
  while true:
    value = value * subjectNumber
    value = value mod divider
    #dump(value)
    if value == publicKey:
      return times
    times.inc

proc calcEncryptionKey(subjectNumber, loopSize: uint): uint =
  var
    subjectNumber = subjectNumber
    value = 1'u
  for i in 0 ..< loopSize:
    value = value * subjectNumber
    value = value mod divider
  return value

let cardLoopSize = calcLoopSize(subjectNumber, cardPublicKey)
dump(cardLoopSize)
let doorLoopSize = calcLoopSize(subjectNumber, doorPublicKey)
dump(doorLoopSize)
let cardEncryptionKey = calcEncryptionKey(doorPublicKey, cardLoopSize)
dump(cardEncryptionKey)
let doorEncryptionKey = calcEncryptionKey(cardPublicKey, doorLoopSize)
dump(doorEncryptionKey)
assert cardEncryptionKey == doorEncryptionKey
