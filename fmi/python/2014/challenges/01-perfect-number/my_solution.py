def is_perfect(x):
    return x == sum(i for i in range(1, x // 2 + 1) if x % i == 0) and x != 0
