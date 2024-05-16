def sum_of_multiples(limit, multiples):
    points = set()
    for base_value in multiples:
        if base_value == 0:
            continue
        for value in range(base_value, limit, base_value):
            points.add(value)
    return sum(points)
