def is_armstrong_number(number):
    digits_count = len(str(number))
    sum = 0
    n = number
    while n > 0:
        sum += (n % 10) ** digits_count
        n //= 10
    return sum == number
