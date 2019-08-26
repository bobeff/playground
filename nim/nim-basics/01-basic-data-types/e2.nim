# Check if your age is divisible by 3. (Hint: use mod)

import times, strformat

let currentTime = now()
let myBirthday = parse("12-09-1985", "dd-MM-yyyy")
let myLifeTimeInterval = between(myBirthday, currentTime)
let myAge = myLifeTimeInterval.years

let answer = if myAge mod 3 == 0: "yes" else: "no"
echo fmt"Is my age which is {myAge} years divisible by 3?: {answer}"
