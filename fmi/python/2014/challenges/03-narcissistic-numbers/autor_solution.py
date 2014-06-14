def is_narcissistic(number, base=10):
    return (sum([int(digit, base) ** len(number) for digit in number]) ==
            int(number, base))
