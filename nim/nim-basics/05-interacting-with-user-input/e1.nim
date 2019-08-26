# Ask a user for their height and weight. Calculate their BMI. Report them the
# BMI value and the category.

import strUtils

proc bodyMassIndex(height, weight: float): float =
  result = weight / (height * height)

proc bodyMassCategory(bmi: float): string =
  if bmi < 15:
    result = "Very severely underweight"
  elif bmi < 16:
    result = "Severely underweight"
  elif bmi < 18.5:
    result = "Underweight"
  elif bmi < 25:
    result = "Normal (healthy weight)"
  elif bmi < 30:
    result = "Overweight"
  elif bmi < 35:
    result = "Obese Class I (Moderately obese)"
  elif bmi < 40:
    result = "Obese Class II (Severely obese)"
  elif bmi < 45:
    result = "Obese Class III (Very severely obese)"
  elif bmi < 50:
    result = "Obese Class IV (Morbidly Obese)"
  elif bmi < 60:
    result = "Obese Class V (Super Obese)"
  else:
    result = "Obese Class VI (Hyper Obese)"

stdout.write("Enter your height in meters: ")
let height = stdin.readLine().parseFloat()

stdout.write("Enter your weight in kilograms: ")
let weight = stdin.readLine().parseFloat()

let bmi =  bodyMassIndex(height, weight)
echo "Your body mass index is ", bmi
echo "Your body mass category is ", bodyMassCategory(bmi)
