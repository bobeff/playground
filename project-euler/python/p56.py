def digits_sum(x):
    s = 0
    while x > 0:
        s += x % 10
        x //= 10
    return s

print(max(digits_sum(a ** b) for a in range(1, 100) for b in range(1, 100)))
