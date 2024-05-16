def classify(number):
    """ A perfect number equals the sum of its positive divisors.

    :param number: int a positive integer
    :return: str the classification of the input integer
    """
    if number <= 0:
        raise ValueError(
            'Classification is only possible for positive integers.')
    factors_sum = sum(n for n in range(1, number // 2 + 1) if number % n == 0)
    if factors_sum == number:
        return 'perfect'
    elif factors_sum > number:
        return 'abundant'
    else:
        return 'deficient'
