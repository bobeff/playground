def is_narcissistic(number, base=10):
    n = int(number, base)
    l = len(number)
    return sum(int(digit, base) ** l for digit in number) == n
