# Alice earns $400 every 15 days. Bob earns $3.14 per hour and works 8 hours
# a day, 7 days a week. After 30 days, has Alice earned more than Bob?
# (Hint: use relational operators)

import strformat

const aliceSalaryFor15days = 400
const bobSalaryPerHour = 3.14
const bobWorkHoursPerDay = 8
const workDays = 30

const aliceSalaryForAllWorkDays = workDays / 15 * aliceSalaryFor15days
const bobSalaryForAllWorkDays = workDays * bobWorkHoursPerDay * bobSalaryPerHour

echo "After ", workDays, " days Alice earns $", aliceSalaryForAllWorkDays, "."
echo "After ", workDays, " days Bob earns $", bobSalaryForAllWorkDays, "."

let (bigger, smaller) =
  if aliceSalaryForAllWorkDays > bobSalaryForAllWorkDays: ("Alice", "Bob")
  else: ("Bob", "Alice")

echo fmt"{bigger} earns more than {smaller}."
