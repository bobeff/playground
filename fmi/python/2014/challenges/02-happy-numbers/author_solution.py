PRE_CALCULATED_SQUARES = {digit: int(digit) ** 2 for digit in "0123456789"}


def is_happy(number):
    sequence = set()
    while number > 1 and number not in sequence:
        sequence.add(number)
        number = sum(PRE_CALCULATED_SQUARES[digit] for digit in str(number))
    return number == 1


def is_prime(number):
    i = 2
    if number == 1:
        return False
    while i <= number ** 0.5:
        if number % i == 0:
            return False
        i += 1
    return True


def happy_primes(numbers):
    return [number for number in numbers
            if is_prime(number) and is_happy(number)]
