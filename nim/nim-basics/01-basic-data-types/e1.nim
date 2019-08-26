# Create an immutable variable containing your age (in years).
# Print your age in days. (1 year = 365 days)

import times

const daysPerYear = 365

let currentTime = now()
let myBirthday = parse("12-09-1985", "dd-MM-yyyy")
let myLifeTimeInterval = between(myBirthday, currentTime)
let myLifeDuration = currentTime - myBirthday
let myAge = myLifeTimeInterval.years

echo "My age is ", myAge.years
echo "I have lived approximately ", myAge * daysPerYear, " days."
echo "More exactly I have lived ", myLifeDuration.days, " days."
