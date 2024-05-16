RESISTORS = {
    "black":  0,
    "brown":  1,
    "red":    2,
    "orange": 3,
    "yellow": 4,
    "green":  5,
    "blue":   6,
    "violet": 7,
    "grey":   8,
    "white":  9,
}

TOLERANCE = {
    'grey':    0.05,
    'violet':  0.1,
    'blue':    0.25,
    'green':   0.5,
    'brown':   1,
    'red':     2,
    'gold':    5,
    'silver': 10,
}

KILO = 1_000
MEGA = 1_000_000
GIGA = 1_000_000_000


def resistor_label(colors):
    tolerance = None

    match colors:
        case [value1]:
            ohms = RESISTORS[value1]
        case [value1, value2]:
            ohms = RESISTORS[value1] * 10 + RESISTORS[value2]
        case [value1, value2, multiplier]:
            ohms = (RESISTORS[value1] * 10 + RESISTORS[value2]) * \
                   10 ** RESISTORS[multiplier]
        case [value1, value2, multiplier, tolerance]:
            ohms = (RESISTORS[value1] * 10 + RESISTORS[value2]) * \
                   10 ** RESISTORS[multiplier]
            tolerance = TOLERANCE[tolerance]
        case [value1, value2, value3, multiplier, tolerance]:
            ohms = (RESISTORS[value1] * 100 + RESISTORS[value2] * 10 +
                    RESISTORS[value3]) * 10 ** RESISTORS[multiplier]
            tolerance = TOLERANCE[tolerance]
        case _:
            raise ValueError(f'Invalid input: {colors}.')

    if ohms >= GIGA:
        result = f'{ohms / GIGA:g} gigaohms'
    elif ohms >= MEGA:
        result = f'{ohms / MEGA:g} megaohms'
    elif ohms >= KILO:
        result = f'{ohms / KILO:g} kiloohms'
    else:
        result = f'{ohms} ohms'

    if tolerance is not None:
        result += f' ±{tolerance:g}%'

    return result
