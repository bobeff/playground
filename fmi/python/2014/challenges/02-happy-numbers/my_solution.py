def get_digits(number):
    while number > 0:
        yield number % 10
        number //= 10

def is_happy(number):
    while number != 1 and number != 89:
        number = sum(digit ** 2 for digit in get_digits(number))
    return number == 1

def is_prime(number):
    if number == 1: return False
    if number == 2: return True
    if number % 2 == 0: return False
    return not any(number % divider == 0 \
        for divider in range(3, int(number ** 0.5) + 1, 2))

def happy_primes(iterable):
    return [number for number in iterable \
        if is_prime(number) and is_happy(number)]
