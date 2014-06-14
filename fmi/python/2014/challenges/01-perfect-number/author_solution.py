from functools import reduce
from operator import add


def is_perfect(number):
    divisors = [d for d in range(1, number) if number % d == 0]
    return reduce(add, divisors, 0) == number
