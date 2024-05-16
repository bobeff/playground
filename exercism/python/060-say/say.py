ONES = {
    0: 'zero',
    1: 'one',
    2: 'two',
    3: 'three',
    4: 'four',
    5: 'five',
    6: 'six',
    7: 'seven',
    8: 'eight',
    9: 'nine',
    10: 'ten',
    11: 'eleven',
    12: 'twelve',
    13: 'thirteen',
    14: 'fourteen',
    15: 'fifteen',
    16: 'sixteen',
    17: 'seventeen',
    18: 'eighteen',
    19: 'nineteen',
}

TENS = {
    2: 'twenty',
    3: 'thirty',
    4: 'forty',
    5: 'fifty',
    6: 'sixty',
    7: 'seventy',
    8: 'eighty',
    9: 'ninety',
}


def say_below_hundred(number):
    assert number < 100, 'The number is out of range.'
    if number < 20:
        return ONES[number]
    saying = TENS[number // 10]
    ones = number % 10
    if ones > 0:
        saying += '-' + ONES[ones]
    return saying


def say_helper(number, divider, func, phrase):
    if number < divider:
        return func(number)
    saying = func(number // divider) + ' ' + phrase
    remainder = number % divider
    if remainder > 0:
        saying += ' ' + func(remainder)
    return saying


def say_below_thousand(number):
    assert number < 1000, 'The number is out of range'
    return say_helper(number, 100, say_below_hundred, 'hundred')


def say_below_million(number):
    assert number < 1_000_000, 'The number is out of range.'
    return say_helper(number, 1000, say_below_thousand, 'thousand')


def say_below_billion(number):
    assert number < 1_000_000_000, 'The number is out of range.'
    return say_helper(number, 1_000_000, say_below_million, 'million')


def say(number):
    if number < 0 or number > 999_999_999_999:
        raise ValueError("input out of range")
    return say_helper(number, 1_000_000_000, say_below_billion, 'billion')
