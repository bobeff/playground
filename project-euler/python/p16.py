number = 2 ** 1000
sumOfDigits = 0

while number > 0:
    sumOfDigits += number % 10
    number //= 10
    
print sumOfDigits
