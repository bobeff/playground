import re


def build_regex(key):
    regex_str = '^ *'
    for i in range(len(key) - 1):
        regex_str += 'X{%s} +' % key[i]
    regex_str += 'X{%s} *$' % key[-1]
    return re.compile(regex_str)


def validate_row(row, key):
    regex = build_regex(key)
    return regex.match(''.join(row))


def is_valid(nonogram, keys):
    for row, key in zip(nonogram, keys):
        if not validate_row(row, key):
            return False
    return True


def validate_nonogram(nonogram, keys):
    return is_valid(nonogram, keys['rows']) and \
        is_valid(zip(*nonogram), keys['columns'])
