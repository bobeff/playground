def factors(value):
    factors = []
    while value % 2 == 0:
        factors.append(2)
        value //= 2
    for n in range(3, value + 1, 2):
        while value % n == 0:
            factors.append(n)
            value //= n
        if value == 1:
            break
    return factors
