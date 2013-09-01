def fact(n):
    if n == 0: return 1
    else: return n * fact(n - 1)

number = fact(100)
print number;

sumOfDigits = 0
while number > 0:
    sumOfDigits += number % 10
    number //= 10

print sumOfDigits
