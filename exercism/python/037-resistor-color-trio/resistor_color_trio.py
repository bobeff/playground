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


def value(colors):
    return (RESISTORS[colors[0]] * 10 + RESISTORS[colors[1]]) * \
           10 ** RESISTORS[colors[2]]


KILO = 1_000
MEGA = 1_000_000
GIGA = 1_000_000_000


def label(colors):
    ohms = value(colors)
    if ohms >= GIGA:
        return f'{ohms / GIGA:g} gigaohms'
    elif ohms >= MEGA:
        return f'{ohms / MEGA:g} megaohms'
    elif ohms >= KILO:
        return f'{ohms / KILO:g} kiloohms'
    else:
        return f'{ohms} ohms'
